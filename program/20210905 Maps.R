rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

library(data.table, rgdal, sf, ggplot2)

# pacman::p_load(data.table, sf, rgdal, ggplot2, broom) # broom, ggmap, elevatr

#################################################################################
# MAPS
#################################################################################
shp.water <- rgdal::readOGR(dsn = "derived/Mapping/district-of-columbia-latest-free.shp", 
                  layer = "gis_osm_water_a_free_1")

shp.roads <- rgdal::readOGR(dsn = "derived/Mapping/district-of-columbia-latest-free.shp", 
                  layer = "gis_osm_roads_free_1")

df.water <- ggplot2::fortify(shp.water)
df.roads <- ggplot2::fortify(shp.roads)

# fortify may be deprecated; this is the alternative
# df.water2 <- broom::tidy(shp.water)
# df.roads2 <- broom::tidy(shp.roads)

sf.routes <- readRDS("derived/20211018 Route Calculations.Rds")

# verify that biking distance is weakly greater than geographic distance
# ggplot(data = sf.routes) + geom_point(aes(x = dist_geo, y = distance)) 


df.stations <- unique(sf.routes[, c("startNAME", "startX", "startY"), drop = TRUE])
df.stations[,is22P := startNAME == "22nd & P ST NW"]

sf.22ndP <- subset(sf.routes, startNAME == "22nd & P ST NW")
df.22ndP <- as.data.frame(st_coordinates(sf.22ndP))


ggplot() + 
  
  # road network
  geom_path(data = df.roads,
               aes(group = group, x = long, y = lat),
               colour = "gray", alpha = .8) +
  
  # water
  geom_polygon(data = df.water2,
               aes(group = group, x = long, y = lat),
               fill  = "lightblue") + 
  
  # shortest paths from 22nd and P station
  geom_path(data = df.22ndP,
            aes(group = L1, x = X, y = Y),
            colour = "black") +
  
  # bikeshare stations
  geom_point(data = df.stations[is22P == FALSE],
             aes(x = startX, y = startY)) +
  
  geom_point(data = df.stations[is22P == TRUE],
             aes(x = startX, y = startY), color = "red", size = 3) + 
  
  # cleaning up
  coord_cartesian(xlim = c(-77.1, -77.0), ylim = c(38.85, 38.95)) +
  theme_minimal()




  
