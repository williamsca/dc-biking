# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

source <- "data/Capital Bikeshare/"

pacman::p_load(data.table, lubridate, ggmap, openxlsx)

#################################################################################
# IMPORT CAPITAL BIKESHARE TRIP DATA
# To save space, the raw data are not tracked by git.
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

dt <- readRDS("derived/Capital Bikeshare Trips (2015-2019).Rds")

# Geocode dock locations
dt.coordinates <- fread(paste0(source, "20210805 Capital_Bike_Share_Locations.csv"))

dt.docks <- unique(dt[, .(`Start station number`, `Start station`)])
dt.docks <- merge(dt.docks, dt.coordinates, by.x = "Start station", by.y = "NAME", all.x = TRUE, all.y = TRUE)

write.xlsx(dt.docks[, .(`Start station number`, `Start station`, X, Y)], "int/20210805 LOOKUP Station Names.xlsx")

# dt.tmp <- unique(dt[, .(`End station number`, `End station`)])
# dt.tmp <- merge(dt.docks, dt.tmp, by.x = c("Start station number"), by.y = c("End station number"), all.x = TRUE, all.y = TRUE)
mutate_geocode(dt.docks, `Start station`, output = "latlona")


# TODO: calculate flows by month

dt.flows <- dt[, .(nTrips = .N), .(month, year, `Start station number`, `Start station`, `End station number`, `End station`)]
setorder(dt.flows, `Start station number`, -nTrips)
