rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, openxlsx, ggmap, sf, elevatr, rgdal)

#################################################################################
# MAPS
#################################################################################

shp.dc.water <- readOGR(dsn = "derived/Mapping/district-of-columbia-latest-free.shp", 
                  layer = "gis_osm_water_a_free_1")

shp.dc.roads <- readOGR(dsn = "derived/Mapping/district-of-columbia-latest-free.shp", 
                  layer = "gis_osm_roads_free_1")

df.water <- fortify(shp.dc.water)
df.roads <- fortify(shp.dc.roads)

ggplot() + 
  geom_polygon(data = df.roads,
               aes(group = group, x = long, y = lat),
               fill = "lightgray") +
  geom_polygon(data = df.water,
               aes(group = group, x = long, y = lat),
               fill  = "lightblue") + 
  coord_cartesian(xlim = c(-77.077579, -77.020599), 
                  ylim = c(39.89241, 39.93265)) + 
  theme_minimal()

  
