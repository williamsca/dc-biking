# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, stargazer, estimatr, lmtest, Hmisc, weights, openxlsx,  ggplot2)
# Hmisc: calculate weighted standard deviations
# weights: produce weighted histograms

dt <- readRDS("derived/Capital Bikeshare Flows (2015-2019).Rds")

#################################################################################
# DATA EXPLORATION
#################################################################################
nrow(dt[nTrips == 0]) / nrow(dt) # share of origin-destination-month-year tuples with zero trips

# station-pair level statistics and distributions
# (includes all station pairs which are ever both active in the same month of 2019)
dt.pairs <- dt[year == 2019,.(nTrips19 = sum(nTrips)), 
                  by = .(startNAME, endNAME, dist_geo, duration, distance, dist_joules, dElevation)]

dt.pairs <- dt.pairs[startNAME != endNAME]

summary(dt.pairs[, distance])
summary(dt.pairs[, dist_joules])
hist(dt.pairs[, dist_joules], breaks = 260, xlim = c(0, 4000000)) # biking distance

# the average trip in 2019 is ~2.27km long, ends at a station ~2.7m lower in elevation than it began, and requires ~780,000 joules (~187 Calories)
dt.summary <- dt.pairs[, .(avgDElev = weighted.mean(dElevation, nTrips19), 
                           stdDElev = sqrt(wtd.var(dElevation, nTrips19)),
                           avgDist = weighted.mean(distance, nTrips19),
                           stdDist = sqrt(wtd.var(distance, nTrips19)),
                           avgJoules = weighted.mean(dist_joules, nTrips19),
                           stdJoules = sqrt(wtd.var(dist_joules, nTrips19)))] 

wtd.hist(dt.pairs[, dElevation], weight = dt.pairs[, nTrips19])
wtd.hist(dt.pairs[, distance], weight = dt.pairs[, nTrips19])
wtd.hist(dt.pairs[, dist_joules], weight = dt.pairs[, nTrips19])

# active stations increase from 200 to 300 between 2015 and 2019, while total trips hover around 3 million
dt[, isActive := (startNTrips > 0 | endNTrips > 0)]
dt.totals <- dt[, .(nTrips = sum(nTrips), nStations = max(isActive)), by = .(year, startNAME)]
dt.totals <- dt.totals[, .(nTrips = sum(nTrips), nStations = sum(nStations)), by = .(year)]

# net trip counts by station-month
dt.stations <- dt[startNAME == endNAME]
dt.stations <- dt.stations[, .(startNAME, ID, month, year, startNTrips, endNTrips)]
dt.stations[, netTrips := endNTrips - startNTrips]
dt.stations[, grossTrips := endNTrips + startNTrips]
dt.stations[, openYear := min(year), by = .(startNAME)]

dt.stations.year <- dt.stations[, .(netTrips = sum(netTrips), grossTrips = sum(grossTrips)),
                               .(startNAME, ID, year, openYear)] # sum over months
# TODO: reshape wide?
setorder(dt.stations.year, year, netTrips) # 
table(dt.stations.year[, openYear])

dt.stations19 <- dt.stations[year == 2019]
dt.stations19 <- dt.stations19[, .(netTrips = sum(netTrips), grossTrips = sum(grossTrips)),
                               .(startNAME, endNAME, ID, year)]
dt.stations19[, rankGross := frank(-grossTrips, ties.method = "max"), .(year)]
dt.stations19[, rankNet := frank(-netTrips, ties.method = "max"), .(year)]

setorder(dt.stations19, rankGross)
# write.xlsx(dt.stations19[, .(startNAME, rankGross, grossTrips, netTrips)], 
#            "output/20220105 Busiest Stations (2019).xlsx",
#            overwrite = TRUE) # don't write over this file; it contains the formatted table

# Capital BikeShare is evidently moving between 10,000 and 20,000 bikes per month
dt.moved <- dt.stations[, .(nMoved = sum(abs(netTrips))/2), by = .(month, year)]


dt.trips <- dt[startNAME != endNAME] # It is not obvious to me how to think about trips which originate and end at the same station.
dt.trips[, startI := as.factor(startNAME)][, endI := as.factor(endNAME)][, monthI := as.factor(month)][, yearI := as.factor(year)]
dt.trips[, lnStartNTrips := log(startNTrips + 1)][, lnEndNTrips := log(endNTrips + 1)][, ln] # TODO: create log versions of distances

hist(dt.trips)

# Trial run of a gravity model. 
# Naive log-log form; exclude observations with zero trips.
# TODO: get robust SEs working
lm_geo <- lm(log(nTrips) ~ log(dist_geo) + log(startNTrips) + log(endNTrips) + as.factor(month):as.factor(year), 
         dt.trips[nTrips > 0 & startNTrips > 0 & endNTrips > 0])

lm_real <- lm(log(nTrips) ~ log(distance) + log(startNTrips) + log(endNTrips) + as.factor(month):as.factor(year), 
         dt.trips[nTrips > 0 & startNTrips > 0 & endNTrips > 0])

lm_joule <- lm(log(nTrips) ~ log(dist_joules + 1) + log(startNTrips) + log(endNTrips) + as.factor(month):as.factor(year), 
         dt.trips[nTrips > 0 & startNTrips > 0 & endNTrips > 0])

stargazer(lm_geo, lm_real, lm_time, type = "text", covariate.labels = c("Euclidian Distance", "Taxicab Distance", "Duration"), 
          omit = c("month", "year"), se = starprep(lm_geo, lm_real, lm_time))

ppml_geo <- glm(nTrips ~ log(dist_geo) + lnStartNTrips + lnEndNTrips + monthI + yearI, family = "quasipoisson", data = dt.trips)
ppml_manhattan <- glm(nTrips ~ log(distance) + lnStartNTrips + lnEndNTrips + monthI + yearI, family = "quasipoisson", data = dt.trips)
ppml_joules <- glm(nTrips ~ log(dist_joules + 1) + lnStartNTrips + lnEndNTrips + monthI + yearI, family = "quasipoisson", data = dt.trips)
ppml_joint <- glm(nTrips ~ log(dist_joules +10) +  log(distance) + lnStartNTrips + lnEndNTrips + monthI + yearI, family = "quasipoisson", data = dt.trips)

summary(ppml_geo)
summary(ppml_manhattan) 
summary(ppml_joules)
summary(ppml_joint)

dt.ebikes <- dt.trips[, dist_joules := 10]
dt.ebikes[, nTripsE := predict(ppml_joint, dt.ebikes)]

# postestimation plots
postest <- dt.trips[, resid := residuals(ppml_joint)] [, fitted := fitted(ppml_joint)]
ggplot(data = postest, aes(x = distance, y = resid)) + 
  geom_point()


# Consider the station at 22nd and P St NW. How far do bikers travel from here?
dt.ex <- dt[startNAME == "22nd & P ST NW" & year == 2019 & month == 8]
setorder(dt.ex, distance)

plt <- ggplot(dt.ex, aes(x = distance, y = nTrips)) +
  geom_point()
plot(plt)