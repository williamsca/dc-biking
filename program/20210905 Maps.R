rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, sf, elevatr, rgdal, ggmap) # broom, ggmap

#################################################################################
# MAPS
#################################################################################
shp.water <- readOGR(dsn = "derived/Mapping/district-of-columbia-latest-free.shp", 
                  layer = "gis_osm_water_a_free_1")

shp.roads <- readOGR(dsn = "derived/Mapping/district-of-columbia-latest-free.shp", 
                  layer = "gis_osm_roads_free_1")

df.water <- fortify(shp.water)
df.roads <- fortify(shp.roads)

sf.routes <- readRDS("derived/20211018 Route Calculations.Rds")

# verify that biking distance is weakly greater than geographic distance
# ggplot(data = sf.routes) + geom_point(aes(x = dist_geo, y = distance)) 


df.stations <- unique(sf.routes[, c("startNAME", "startX", "startY"), drop = TRUE])

sf.22ndP <- subset(sf.routes, startNAME == "22nd & P ST NW")
df.22ndP <- as.data.frame(st_coordinates(sf.22ndP))


ggplot() + 
  
  # road network
  geom_path(data = df.roads,
               aes(group = group, x = long, y = lat),
               colour = "gray", alpha = .8) +
  
  # water
  geom_polygon(data = df.water,
               aes(group = group, x = long, y = lat),
               fill  = "lightblue") + 
  
  # bikeshare stations
  geom_point(data = df.stations,
             aes(x = startX, y = startY)) +
  
  # shortest paths from 22nd and P station
  geom_path(data = df.22ndP,
            aes(group = L1, x = X, y = Y),
            colour = "black") +
  
  # cleaning up
  coord_cartesian(xlim = c(-77.1, -77.0), ylim = c(38.85, 38.95)) +
  theme_minimal()




  
