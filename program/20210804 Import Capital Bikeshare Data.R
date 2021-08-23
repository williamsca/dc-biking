# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

source <- "data/Capital Bikeshare/"

pacman::p_load(data.table, lubridate, ggmap, openxlsx, geosphere)

#################################################################################
# IMPORT CAPITAL BIKESHARE TRIP DATA
# https://www.capitalbikeshare.com/system-data
# To save space, the raw and derived data are not tracked by git.
#################################################################################
# files <- list.files(paste0(source, "Trips/"))
# files <- paste0(source, "Trips/", files)
# dt <- rbindlist(lapply(files, fread))
# 
# dt[, startTime := parse_date_time(`Start date`, "ymdHMS")]
# dt[, endTime := parse_date_time(`End date`, "ymdHMS")]
# dt[, month := month(startTime)][, year := year(startTime)]
# 
# saveRDS(dt, file = "derived/Capital Bikeshare Trips (2015-2019).Rds")

#################################################################################
# MERGE IN COORDINATES AND CALCULATE FLOWS
#################################################################################
dt <- readRDS("derived/Capital Bikeshare Trips (2015-2019).Rds")

# Calculate flows by month
dt.flows <- dt[, .(nTrips = .N), .(month, year, `Start station number`, `Start station`, `End station number`, `End station`)]
setorder(dt.flows, `Start station number`, -nTrips)

# Merge in dock coordinates
# dt.coordinates <- fread(paste0(source, "20210805 Capital_Bike_Share_Locations.csv"))

# Many station numbers appear with conflicting names. Generally, the names clearly refer to the  same location. However,
# some are substantially different. I've therefore defined each station as the combination of the station number and the station name.
# Each of those tuples maps onto a unique NAME value.
dt.coordinates <- setDT(read.xlsx("derived/LOOKUP Station ~ Coordinates.xlsx"))
dt.coordinates <- unique(dt.coordinates[, .(`Start.station.number`, `Start.station`, NAME, X, Y)]) 

dt.flows <- merge(dt.flows, dt.coordinates, by.x = c("Start station number", "Start station"), by.y = c("Start.station.number", "Start.station"), all.x = TRUE)
setnames(dt.flows, c("NAME", "X", "Y"), c("startNAME", "startX", "startY"))

dt.flows <- merge(dt.flows, dt.coordinates, by.x = c("End station number", "End station"), by.y = c("Start.station.number", "Start.station"), all.x = TRUE)
setnames(dt.flows, c("NAME", "X", "Y"), c("endNAME", "endX", "endY"))

dt.flows <- dt.flows[, .(nTrips = sum(nTrips)), .(month, year, `startNAME`, `endNAME`, `startX`, `startY`, `endX`, `endY`)]
anyDuplicated(dt.flows, by = c("month", "year", "startNAME", "endNAME")) == 0 # verify that each station has a unique (longitude, latitude) tuple

dt.flows[, distance := distVincentyEllipsoid(cbind(startX, startY), cbind(endX, endY)) / 1609] # distance calculations (in miles)

nrow(dt.flows[startNAME != endNAME & distance == 0]) == 0 # verify that no two stations have the same location

saveRDS(dt.flows, file = "derived/Capital Bikeshare Flows (2015-2019).Rds")




