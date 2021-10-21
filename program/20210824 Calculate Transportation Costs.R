rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, openxlsx, geosphere, osrm, sf, elevatr)

options(osrm.server = "http://localhost:5000/", osrm.profile = "bike")

#################################################################################
# CALCULATE VARIOUS MEASURES OF DISTANCE BETWEEN DOCKS
#################################################################################
dt <- readRDS("derived/Capital Bikeshare Trips (2015-2019).Rds")

dt.flows <- unique(dt[, .(`Start station number`, `Start station`, `End station number`, `End station`, month, year)])

# Merge in dock coordinates
# Many station numbers appear with conflicting names. Generally, the names clearly refer to the same location. However,
# some are substantially different. I've therefore defined each station as the combination of the station number and the station name.
# Each of those tuples maps m:1 onto one NAME.
dt.coordinates <- readRDS("lookup/LOOKUP Station ~ Coordinates (2021.10.03).Rds")
dt.NAME <- unique(dt.coordinates[, .(NAME, X, Y, county)])

# Take the cartesian product of stations
dt.possible <- expand.grid(dt.NAME$NAME, dt.NAME$NAME)
dt.possible <- merge(dt.NAME, dt.possible, by.x = "NAME", by.y = "Var1")
dt.possible <- merge(dt.possible, dt.NAME, by.x = "Var2", by.y = "NAME")
setnames(dt.possible, c("NAME", "Var2", "X.x", "Y.x", "X.y", "Y.y"), 
         c("endNAME", "startNAME", "endX", "endY", "startX", "startY"))

dt.possible[, dist_geo := distVincentySphere(cbind(startX, startY), cbind(endX, endY)) / 1609] # naive distance calculations (in miles)

# No doubt there is a cleaner way to do this
# dt.test <- head(dt.possible)
# src_i <- st_as_sf(dt.possible[1,  .(startY, startX)], coords = c("startX", "startY"), crs = 4326)
# dst_i <- st_as_sf(dt.possible[1, .(endY, endX)], coords = c("endX", "endY"), crs = 4326)
# sf.routes <- osrmRoute(src = src_i, dst = dst_i, returnclass = "sf", osrm.profile = "bike")
# sf.routes$ID <- 1
sf.routes <- readRDS("int/20210830 Temp Route Calculations (1-363916).Rds")

dt.possible[, ID := .I]
dt.possible <- dt.possible[county.x == "Washington, DC"  & county.y == "Washington, DC"]

dt.remaining <- dt.possible[ID > 363916]

for (i in 1:length(dt.remaining$ID)) {
  src_i <- st_as_sf(dt.remaining[i,  .(startY, startX)], coords = c("startX", "startY"), crs = 4326)
  dst_i <- st_as_sf(dt.remaining[i, .(endY, endX)], coords = c("endX", "endY"), crs = 4326)

  v <- tryCatch({
    tmp <- osrmRoute(src  = src_i, dst = dst_i, returnclass = "sf", osrm.profile = "bike")
    tmp$ID <- dt.remaining[i, ID]
    sf.routes <- rbind(sf.routes, tmp)
  },
  error = function(err) {
  print(paste0("Failed to find row ", str(i)))
  print(err)
  })      #Sys.sleep(2)
}

saveRDS(sf.routes, "int/20210830 Temp Route Calculations (All DC).Rds")

sf.biking <- st_as_sf(merge(dt.possible, sf.routes, by = c("ID"), all.x = TRUE), crs = 4326)

saveRDS(sf.biking, "derived/20211018 Route Calculations.Rds")
