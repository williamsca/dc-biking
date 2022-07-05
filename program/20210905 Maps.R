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

dt <- readRDS("derived/Capital Bikeshare Flows (2015-2019).Rds")

# check that biking distance is weakly greater than geographic distance
# ggplot(data = sf.routes) + geom_point(aes(x = dist_geo, y = distance)) 

dt.stations <- dt[startNAME == endNAME, .(startNAME, ID, date, startNTrips, endNTrips, startX, startY)]
dt.stations <- dt.stations[, .(startNTrips = sum(startNTrips), endNTrips = sum(endNTrips)), .(startNAME, year = year(date), startX, startY)]
dt.stations[, netTrips := endNTrips - startNTrips][, grossTrips := endNTrips + startNTrips]
dt.stations[, `Trip Ratio` := netTrips / grossTrips]
dt.stations <- dt.stations[grossTrips >= 5]

# overview map
wh <- c(38.89782,-77.03656)
dim <- c(38.99526-wh[1], -76.90945-wh[2])
df.box <- expand.grid(c(38.88,38.93), c(-77.056,-77.01))
df.box <- df.box[c(2,4,3,1,2),]

ggplot() + 
  
  # road network
  geom_path(data = df.roads,
               aes(group = group, x = long, y = lat),
               colour = "gray", alpha = .7) +

  # water
  geom_polygon(data = df.water,
               aes(group = group, x = long, y = lat),
               fill  = "lightblue") +
  
  geom_path(data = df.box,
               aes(x = Var2, y = Var1)) +

  # bikeshare stations
  geom_point(data = dt.stations[year == 2019],  aes(x = startX, y = startY)) +
  
  # cleaning up
  labs(title = "DC Bikeshare Stations", subtitle = "2019", caption = "Source: Capital Bikeshare System Data. 
Note: Chart identifies all stations with at least five combined 
departures and arrivals during 2019.",
       x = "", y = "") + 
  coord_cartesian(xlim = c(-77.11, wh[2]+dim[2]), ylim = c(wh[1]-dim[1], wh[1]+dim[1])) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(size = 12, vjust = -10, face = "bold"), plot.subtitle = element_text(vjust = -10, face = "bold"), # lower the titles into chart area
        plot.caption = element_text(vjust = 15),
        panel.background = element_blank())
ggsave("output/20220703 Bikeshare Stations (2019).png")

# map of downtown + columbia heights
ggplot() + 
  
  # road network
  geom_path(data = df.roads,
               aes(group = group, x = long, y = lat),
                colour = "gray", alpha = .6) +

  # water
  geom_polygon(data = df.water,
               aes(group = group, x = long, y = lat),
               fill  = "lightblue") +
  
  # bikeshare stations
  geom_point(data = dt.stations[year == 2019],
              aes(x = startX, y = startY, size = grossTrips, color = `Trip Ratio`)) +
  scale_size(range = c(0, 6), guide = NA) + # set point size range
  scale_colour_gradient2(mid = "lightyellow3") + # set color gradient
  
  # cleaning up
  labs(title = "Bikeshare Station Usage", subtitle = "2019",
       x = "", y = "") + 
  coord_cartesian(xlim = c(df.box[1,2], df.box[2,2]), ylim = c(df.box[3,1], df.box[1,1])) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(size = 12, face =  "bold"), plot.subtitle = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        panel.background = element_blank()) +
  guides(colour = "colorbar", size = "none")
ggsave("output/20220703 Bikeshare Usage (2019).png")





  
