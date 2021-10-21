# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, tmaptools, openxlsx)

#################################################################################
#                     Prepare bike dock data
#################################################################################

dt.coordinates <- setDT(read.xlsx("int/LOOKUP Station ~ Coordinates.xlsx"))

dt.capacities <- fread(paste0("source/20210805 Capital_Bike_Share_Locations.csv"))
nrow(dt.capacities[NUM_DOCKS_AVAILABLE + NUM_DOCKS_DISABLED + NUM_BIKES_AVAILABLE + NUM_BIKES_DISABLED != CAPACITY]) # why don't these fields total to the capacity?

dt.capacities <- dt.capacities[, .(NAME, STATION_ID, NUM_DOCKS_AVAILABLE, NUM_DOCKS_DISABLED, NUM_BIKES_AVAILABLE, NUM_BIKES_DISABLED, CAPACITY, REGION_NAME)]

dt.docks <- merge(dt.coordinates, dt.capacities, by = "NAME", all = TRUE)

dt.docks[, STATION_ID_CW := .I]

dt.noRegion <- dt.docks[is.na(REGION_NAME)]
dt.address <- setDT(rev_geocode_OSM(dt.noRegion$X, dt.noRegion$Y))

dt.docks <- merge(dt.docks, dt.address[, .(x,y,city,county,state)], by.x = c("X", "Y"), by.y = c("x","y"), all.x = TRUE)

dt.docks[is.na(county), county := REGION_NAME]
dt.docks[grep("Montgomery County", county), county := "Montgomery County, MD"]
dt.docks[city == "Washington", county := "Washington, DC"]
dt.docks[city == "Alexandria", county := "Alexandria, VA"]
dt.docks[grep("Fairfax", county), county := "Fairfax, VA"]
dt.docks[grep("Arlington", county), county := "Arlington, VA"]

dt.docks[, city := NULL][, state := NULL][, address := NULL][, REGION_NAME := NULL]

saveRDS(dt.docks, "lookup/LOOKUP Station ~ Coordinates (2021.10.03).Rds")