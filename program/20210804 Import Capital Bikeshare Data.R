rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

source <- "source/Capital Bikeshare/"

pacman::p_load(data.table, lubridate, openxlsx, sf)

#################################################################################
# IMPORT CAPITAL BIKESHARE TRIP DATA
# https://www.capitalbikeshare.com/system-data
# To save space, the raw and derived data are not tracked by git. I've saved a
# backup on an external hard drive.
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
# Many station numbers appear with conflicting names. Generally, the names clearly refer to the same location. However,
# some are substantially different. I've therefore defined each station as the combination of the station number and the station name.
# Each of those tuples maps m:1 onto one NAME.
# dt.coordinates <- setDT(read.xlsx("lookup/LOOKUP Station ~ Coordinates.xlsx"))
dt.coordinates <- readRDS("lookup/LOOKUP Station ~ Coordinates (2021.10.03).Rds")
dt.coordinates <- unique(dt.coordinates[, .(`Start.station.number`, `Start.station`, NAME, X, Y, county)]) 

dt.flows <- merge(dt.flows, dt.coordinates, by.x = c("Start station number", "Start station"), by.y = c("Start.station.number", "Start.station"), all.x = TRUE)
setnames(dt.flows, c("NAME", "X", "Y", "county"), c("startNAME", "startX", "startY", "startCounty"))

dt.flows <- merge(dt.flows, dt.coordinates, by.x = c("End station number", "End station"), by.y = c("Start.station.number", "Start.station"), all.x = TRUE)
setnames(dt.flows, c("NAME", "X", "Y", "county"), c("endNAME", "endX", "endY", "endCounty"))

# Define a station as "active" in a particular month if any trips originate OR end there
dt.starts <- unique(dt.flows[, .(startNAME, month, year)])
dt.ends <- unique(dt.flows[, .(endNAME, month, year)])
setnames(dt.ends, c("endNAME"), c("startNAME"))
dt.active <- funion(dt.starts, dt.ends, all = FALSE)
# dt.active <- fintersect(dt.starts, dt.ends)
dt.possible <- dt.active[, CJ(startNAME = startNAME, endNAME = startNAME), .(month, year)]

dt.coordinates_NAME <- unique(dt.coordinates[, .(NAME, X, Y, county)]) # CAPACITY, STATION_ID_CW
dt.possible <- merge(dt.possible, dt.coordinates_NAME, by.x = c("startNAME"), by.y = c("NAME"))
setnames(dt.possible, c("X", "Y", "county"), c("startX", "startY", "startCounty"))
dt.possible <- merge(dt.possible, dt.coordinates_NAME, by.x = c("endNAME"), by.y = c("NAME"))
setnames(dt.possible, c("X", "Y", "county"), c("endX", "endY", "endCounty"))

# Roll up over cases where the same station appears with conflicting names/IDs
dt.flows <- dt.flows[, .(nTrips = sum(nTrips)), .(month, year, `startNAME`, `endNAME`, `startX`, `startY`, `endX`, `endY`, startCounty, endCounty)]

# This line imputes '0's for all station pairs which are both active in a given month but which don't see any trips.
dt.flows <- merge(dt.possible, dt.flows, by = c("month", "year", "startNAME", "endNAME", "startX", "startY", "endX", "endY", "startCounty", "endCounty"), all = TRUE)
dt.flows[is.na(nTrips), nTrips := 0]

anyDuplicated(dt.flows, by = c("month", "year", "startNAME", "endNAME")) == 0 # TRUE -> each station has a unique (longitude, latitude) tuple

sf.routes <- readRDS("derived/20211216 Route and Elevation Calculations.Rds")
st_geometry(sf.routes) <- NULL
sf.routes[, c("startX", "startY", "endX", "endY") := NULL]

dt.flows <- merge(sf.routes, dt.flows, by = c("startNAME", "endNAME"), all.y = TRUE)

nrow(dt.flows[startNAME != endNAME & distance <= 1e-2]) == 0 # TRUE -> no two stations are within .01 miles

# Filter to stations located inside DC
dt.flows <- dt.flows[startCounty == "Washington, DC" & endCounty == "Washington, DC"] 

# Compute total trips by startNAME and endNAME and month
dt.flows[, startNTrips := sum(nTrips), .(startNAME, month, year)]
dt.flows[, endNTrips := sum(nTrips), .(endNAME, month, year)]

dt.flows <- dt.flows[, c("startCounty", "endCounty", "startX", "startY", "endX", "endY") := NULL]

saveRDS(dt.flows, file = "derived/Capital Bikeshare Flows (2015-2019).Rds")




