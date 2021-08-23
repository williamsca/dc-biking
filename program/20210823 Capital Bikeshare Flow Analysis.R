# Import Capital Bikeshare trip data

rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, ggmap)

dt <- readRDS("derived/Capital Bikeshare Flows (2015-2019).Rds")

# It is not obvious to me how to think about trips which originate and end at the same station.
dt.trips <- dt.flows[startName != endNAME]