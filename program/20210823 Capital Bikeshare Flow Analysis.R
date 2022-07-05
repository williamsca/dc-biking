# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, stargazer, estimatr, lmtest, Hmisc, weights, openxlsx,  ggplot2, lubridate)
# Hmisc: calculate weighted standard deviations
# weights: produce weighted histograms

# (startNAME, endNAME, year, month)
dt <- readRDS("derived/Capital Bikeshare Flows (2015-2019).Rds")

# (startNAME, year, month)
dt.stations <- dt[startNAME == endNAME, .(startNAME, ID, date, startNTrips, endNTrips)]
dt.stations[, netTrips := endNTrips - startNTrips][, grossTrips := endNTrips + startNTrips]
dt.stations[, `Trip Ratio` := netTrips / grossTrips]
dt.stations[, openYear := min(year(date)), by = .(startNAME)][, nStations := 1]

# (month)
dt.months <- dt.stations[, .(nStations = sum(nStations), nTrips = sum(startNTrips)), by = .(date)]
dt.months[, avgTrips := nTrips / days_in_month(date)]

# (year)
dt.years <- dt.stations[, .(nTrips = sum(startNTrips)), by = .(year = year(date), startNAME)]
dt.years[, nStations := 1]
dt.years <- dt.years[, .(nTrips = sum(nTrips), nStations = sum(nStations)), by  = .(year)]

#################################################################################
#                             DATA EXPLORATION
#################################################################################
ggplot(data = dt.years, mapping = aes(x = year, y = nTrips / 365)) +
  geom_line(linetype = "dashed", color = "gray") +
  geom_point(color = "black", size = 3) +
  labs(title = "Average Daily Trips on Capital Bikeshare",
       subtitle = "2015-2019",
       x = "Year", y = "Average Daily Trips") +
  scale_y_continuous(limits = c(7500, 9000), breaks  = c(7500, 8000,  8500, 9000)) +
  theme_light()

ggplot(data = dt.months, mapping = aes(x = date, y = nTrips / days_in_month(date))) +
  geom_line(linetype = "dashed", color = "gray") +
  geom_point(color = "black", size = 3) +
  labs(title = "Average Daily Trips on Capital Bikeshare by Month", # (by month)
       subtitle = "2015-2019",
       x = "Year", y = "Average Daily Trips") +
  scale_y_continuous(limits =  c(0, 12000)) +
  scale_x_date(date_breaks = "1 year") +
  theme_light()

ggplot(data = dt.years[, avgStation := nTrips / (nStations * 365)], mapping = aes(x = year, y = avgStation)) +
  geom_line(linetype = "dashed", color = "gray") +
  geom_point(color = "black", size = 3) +
  labs(title = "Average Daily Trips per Station on Capital Bikeshare",
       subtitle = "2015-2019",
       x = "Year", y = "Average Daily Trips per Station") +
  scale_y_continuous(limits = c(25, 37.5), breaks = c(25, 27.5, 30, 32.5, 35, 37.5)) +
  theme_light()

nrow(dt[nTrips == 0]) / nrow(dt) # share of origin-destination-month-year tuples with zero trips

# station-pair statistics and distributions for 2019
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

# net trip counts by station-month
dt.stations <- dt[startNAME == endNAME]
dt.stations <- dt.stations[, .(startNAME, ID, month, year, startNTrips, endNTrips)]
dt.stations[, netTrips := endNTrips - startNTrips]
dt.stations[, grossTrips := endNTrips + startNTrips]
dt.stations[, openYear := min(year), by = .(startNAME)]

dt.stations.year <- dt.stations[, .(netTrips = sum(netTrips), grossTrips = sum(grossTrips)),
                               .(startNAME, ID, year, openYear)] # sum over months
dt.stations.wide <- dcast(dt.stations.year, startNAME + ID + openYear ~ year, 
              value.var = c("netTrips", "grossTrips"), fill = 0)
setorder(dt.stations.wide, openYear, grossTrips_2019, grossTrips_2018) # 

# Average gross trips in 2019 by opening year
dt.avgbyyear <- dt.stations.wide[, .(avgGrossTrips19 = mean(grossTrips_2019), avgNetTrips19 = mean(netTrips_2019)), 
                                 .(openYear)]

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

########################################################################################################
#                                      REGRESSION ANALYSIS
########################################################################################################
dt.trips <- dt[startNAME != endNAME] # It is not obvious to me how to think about trips which originate and end at the same station.
dt.trips[, startI := as.factor(startNAME)][, endI := as.factor(endNAME)][, monthI := as.factor(month)][, yearI := as.factor(year)]
dt.trips[, lnStartNTrips := log(startNTrips + 1)][, lnEndNTrips := log(endNTrips + 1)][, lnDist := log(distance)][, lnDistCal := log(dist_cal + 1)]

# OLS (levels)
lm_geo <- lm(nTrips ~ dist_geo + monthI:yearI, data = dt.trips)
lm_route <- lm(nTrips ~ dist_geo + distance + monthI:yearI, data = dt.trips)
lm_joule <- lm(nTrips ~ dist_geo + distance + dist_cal + monthI:yearI, data = dt.trips)

stargazer(lm_geo, lm_route, lm_joule, type = "text", covariate.labels = c("Euclidian Distance", "Taxicab Distance", "Energy (Joules)"), 
          omit = c("monthI", "yearI"))

rm(lm_geo, lm_route, lm_joule)

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

################################################################################
#                                 SUPERSEDED
################################################################################
# Line chart showing yearly trends in trips and active stations
# coeff <- 10000
# tripColor <- "#69b3a2"
# stationColor<- rgb(0.2, 0.6, 0.9, 1)
# 
# ggplot(dt.totals, aes(x=year)) +
#   geom_line(aes(y = nStations), size = 2, color = stationColor) +
#   geom_line(aes(y = nTrips / coeff), size = 2, color = tripColor) +
#   
#   scale_y_continuous(
#     
#     # First axis
#     name = "Total Trips (#)",
#   
#     sec.axis = sec_axis(~.*coeff, name = "Total Stations (#)")) +
#   
#   theme_minimal() +
#   
#   theme( axis.title.y = element_text(color = stationColor, size = 13),
#          axis.title.y.right = element_text(color = tripColor, size = 13)) +
#   
#   ggtitle("Capital Bikeshare Demand and Capacity")