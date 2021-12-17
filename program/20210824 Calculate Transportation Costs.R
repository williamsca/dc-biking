rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, openxlsx, geosphere, osrm, sf, elevatr, broom)

#################################################################################
# CALCULATE VARIOUS MEASURES OF DISTANCE BETWEEN DOCKS
#################################################################################
options(osrm.server = "http://localhost:5000/", osrm.profile = "bike")

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

dt.possible[, dist_geo := distVincentySphere(cbind(startX, startY), cbind(endX, endY)) / 1000] # naive distance calculations (in kilometers)

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

#################################################################################
# CALCULATE ELEVATION CHANGES & WORK REQUIREMENTS
#################################################################################
sf <- readRDS("derived/20211018 Route Calculations.Rds")

sf$src <- NULL
sf$dst <- NULL

# TODO: diagnostics on asymmetries in travel duration/distance?

# Impute the distance/duration/geometry for routes ending at '34th & Water St NW'
sf.lookup <- st_drop_geometry(subset(sf, select = c(ID, startNAME, endNAME)))

sf.matched <- data.table(merge(sf, sf.lookup, by.x = c("endNAME", "startNAME"), by.y = c("startNAME", "endNAME")))
sf.matched[, ID_pair := paste0(min(ID.x, ID.y), "-", max(ID.x, ID.y)), by = ID.x] # generate ID for each pair of stations

sf.missing <- sf.matched[is.na(duration),]
sf.missing[, geometry := NULL][, duration := NULL][, distance := NULL]
sf.missing <- merge(sf.missing, sf.matched[, .(ID_pair, duration, distance, geometry)], by = c("ID_pair"), all.x = TRUE)
sf.missing <- st_as_sf(sf.missing[!is.na(duration)])
sf.missing$geometry <- st_reverse(sf.missing$geometry) # reverse the order of points in the geometry

sf.matched <- st_as_sf(sf.matched[!is.na(duration)])

sf.filled <- rbind(sf.matched, sf.missing)

# Calculate changes in elevation.
# Split each LINESTRING into the associated POINTs
sf.points <- st_cast(sf.filled, "POINT")

sf.elevations <- get_aws_points(sf.points, z = 14) # elevation in meters
sf.elevations <- sf.elevations[[1]]

saveRDS(sf.elevations, file = "int/20211104 Elevations.Rds")
###################################################################################
###################################################################################
## Calculate average slope across each trip segment
sf.elevations <- readRDS("int/20211104 Elevations.Rds")

dt.elevations <- data.table(sf.elevations)

dt.elevations[, L_elevation := shift(elevation, 1L, type = "lag"), by = .(ID.x)]
dt.elevations[, geoX := st_coordinates(geometry)[,1]][, geoY := st_coordinates(geometry)[,2]]

dt.elevations[, dist_segment := distVincentySphere(cbind(geoX, geoY), cbind(shift(geoX, 1L), shift(geoY, 1L))), by = .(ID.x)]  # distance in meters
mean(dt.elevations$dist_segment, na.rm = TRUE) # the average segment is roughly 1/2 of a kilometer

# dt.test <- dt.elevations[, .(sum(dist_segment, na.rm = TRUE)), by = .(endNAME, startNAME, ID.x, dist_geo, distance)]
# dt.test[, diff := V1 - distance]
#  summary(dt.test$diff) # NB: On average, the "distance" field returned by the OSRM Route API is about 30m *longer* than the sum of the component segments as computed by the Vincenty sphere method.

dt.elevations[, dElevation := elevation - L_elevation] 
dt.elevations[, slope := dElevation/dist_segment * 100]

# Calculate rider energy consumption
K_A = 0.245 # drag factor
V = 4 # m/s (rider velocity)
V_W = 0 # m/s (wind velocity)
m = 80 # kg (mass of rider)
g = 9.807 # m/s^2 (acceleration of gravity)
C_R = .004 # tire rolling coefficient
dt.elevations[, W_rider := (K_A * (V + V_W)^2 + m*g*(slope+C_R))*V] # energy consumption of rider in watts (joules / second)
dt.elevations[W_rider < 0, W_rider := 0]
dt.elevations[, J_segment := W_rider * (dist_segment / V)] # total joules necessary to bike across segment
dt.elevations[is.na(J_segment) | is.nan(J_segment), J_segment := 0]

dt.final_distances <- dt.elevations[, .(dist_joules = sum(J_segment, na.rm = TRUE), dElevation = sum(dElevation, na.rm = TRUE)), 
                                        by = .(endNAME, startNAME, ID.x, dist_geo, duration, distance)]

# merge back in geometry for mapping
sf <- readRDS("derived/20211018 Route Calculations.Rds")
sf <- subset(sf, select = c(ID, startNAME, endNAME, geometry))

sf.final_distances <- merge(sf, dt.final_distances, by.y = c("ID.x", "startNAME", "endNAME"), by.x = c("ID", "startNAME", "endNAME"))

saveRDS(sf.final_distances, "derived/20211216 Route and Elevation Calculations.Rds")
