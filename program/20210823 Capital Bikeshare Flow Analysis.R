# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, stargazer, estimatr, lmtest)

dt <- readRDS("derived/Capital Bikeshare Flows (2015-2019).Rds")

#################################################################################
# DATA EXPLORATION
#################################################################################
dt.stations <- unique(dt[, .(startNAME, endNAME, dist_geo, duration, distance)])
summary(dt.stations[, distance])
hist(dt.stations[, distance]) # biking distance

nrow(dt.trips[nTrips == 0]) / nrow(dt.trips) # share of origin-destination-month-year tuples with zero trips

dt.trips <- dt[startNAME != endNAME] # It is not obvious to me how to think about trips which originate and end at the same station.
dt.trips[, start := as.factor(startNAME)][, end := as.factor(endNAME)][, monthI := as.factor(month)][, yearI := as.factor(year)]

# Trial run of a gravity model. 
# Naive log-log form; exclude observations with zero trips.
# TODO: get robust SEs working
lm_geo <- lm(log(nTrips) ~ log(dist_geo) + log(startNTrips) + log(endNTrips) + as.factor(month):as.factor(year), 
         dt.trips[nTrips > 0 & startNTrips > 0 & endNTrips > 0])

lm_real <- lm(log(nTrips) ~ log(distance) + log(startNTrips) + log(endNTrips) + as.factor(month):as.factor(year), 
         dt.trips[nTrips > 0 & startNTrips > 0 & endNTrips > 0])

lm_time <- lm(log(nTrips) ~ log(duration) + log(startNTrips) + log(endNTrips) + as.factor(month):as.factor(year), 
         dt.trips[nTrips > 0 & startNTrips > 0 & endNTrips > 0])

stargazer(lm_geo, lm_real, lm_time, type = "text", covariate.labels = c("Euclidian Distance", "Taxicab Distance", "Duration"), 
          omit = c("month", "year"), se = starprep(lm_geo, lm_real, lm_time))

ppml <- glm(nTrips ~ log(distance) + yearI*monthI + startNTrips + endNTrips, family = "quasipoisson", data = dt.trips)
summary(ppml)

# Consider the station at 22nd and P St NW. How far do bikers travel from here?
dt.ex <- dt[startNAME == "22nd & P ST NW" & year == 2019 & month == 8]
setorder(dt.ex, distance)

plt <- ggplot(dt.ex, aes(x = distance, y = nTrips)) +
  geom_point()
plot(plt)