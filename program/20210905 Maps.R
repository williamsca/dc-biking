rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, sf, rgdal, ggplot2) # broom, ggmap, elevatr

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

sf.routes <- readRDS("derived/20211216 Route and Elevation Calculations.Rds")

# check that biking distance is weakly greater than geographic distance
# ggplot(data = sf.routes) + geom_point(aes(x = dist_geo, y = distance)) 

df.stations <- unique(sf.routes[, c("startNAME", "startX", "startY"), drop = TRUE])
df.stations[,is14I := startNAME == "14th & Irving St NW"]

df.14IFrom <- unique(sf.routes[, c("startNAME", "startX", "startY")])

minJoules <- 80000
sf.14IFrom <- subset(sf.routes, startNAME == "14th & Irving St NW" & dist_joules <= minJoules
                     & endNAME != "14th & Irving St NW")
sf.14ITo <- subset(sf.routes, endNAME == "14th & Irving St NW" & dist_joules <= minJoules
                     & startNAME != "14th & Irving St NW")
# df.14ITo <- as.data.frame(st_coordinates(sf.14ITo))
# df.14IFrom <- as.data.frame(st_coordinates(sf.14IFrom))

ggplot() + 
  
  # # road network
  geom_path(data = df.roads,
               aes(group = group, x = long, y = lat),
               colour = "gray", alpha = .8) +
  
  # water
  geom_polygon(data = df.water,
               aes(group = group, x = long, y = lat),
               fill  = "lightblue") + 
  
  # bikeshare stations
  # geom_point(data = df.stations[is14I == FALSE],
  #            aes(x = startX, y = startY)) +
  
  geom_point(data = df.stations[is14I == TRUE],
             aes(x = startX, y = startY), color = "red", size = 3) + 
  geom_point(data = sf.14IFrom, aes(x = endX, y = endY), color = "black") +
  
  # cleaning up
  labs(title = "Leaving 14th and Irving St NW is Easy",
       x = "", y = "") + 
  coord_cartesian(xlim = c(-77.075, -77.0), ylim = c(38.85, 38.95)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(size = 12),
        panel.background = element_blank())

ggplot() + 
  
  # # road network
  # geom_path(data = df.roads,
  #              aes(group = group, x = long, y = lat),
  #              colour = "gray", alpha = .8) +
  
  # water
  geom_polygon(data = df.water,
               aes(group = group, x = long, y = lat),
               fill  = "blue") + 
  
  # bikeshare stations
  # geom_point(data = df.stations[is14I == FALSE],
  #            aes(x = startX, y = startY)) +
  
  geom_point(data = df.stations[is14I == TRUE],
             aes(x = startX, y = startY), color = "red", size = 3) + 
  geom_point(data = sf.14ITo, aes(x = startX, y = startY), color = "black") +
  
  # cleaning up
  labs(title = "Getting to 14th and Irving St NW is Hard",
       x = "", y = "") + 
  coord_cartesian(xlim = c(-77.075, -77.0), ylim = c(38.85, 38.95)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(size = 12),
        panel.background = element_blank())



  
