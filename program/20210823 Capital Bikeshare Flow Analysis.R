# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, ggmap, stargazer)

dt <- readRDS("derived/Capital Bikeshare Flows (2015-2019).Rds")

#################################################################################
# DATA EXPLORATION
#################################################################################
# Note that our data include only station pairs with >0 trips. It may be necessary to
# impute 0s for those station pairs which are both operating in a given month but do not
# see any pairwise traffic.
dt.stations <- unique(dt[, .(startNAME, endNAME, distance)])
summary(dt.stations[, distance])
hist(dt.stations[, distance])


# Trial run of a gravity model. How should one measure the "size" of each bike station?
dt.trips <- dt[startNAME != endNAME] # It is not obvious to me how to think about trips which originate and end at the same station.
dt.trips[, distance2 := distance^2]

lm <- lm(nTrips ~ -1  +  distance + distance2 + as.factor(month)*as.factor(year), dt.trips)

stargazer(lm, type = "text", covariate.labels = c("Distance (mi)", "Distance\\^2 (mi)"), omit = c("month", "year"))

# Consider the station at 22nd and P St NW. How far do bikers travel from here?
dt.ex <- dt[startNAME == "22nd & P ST NW" & year == 2019 & month == 8]
setorder(dt.ex, distance)

plt <- ggplot(dt.ex, aes(x = distance, y = nTrips)) +
  geom_point()
plot(plt)