rm(list = ls())

dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(dir)

pacman::p_load(data.table, openxlsx, ggmap, sf, elevatr)

#################################################################################
# MAPS
#################################################################################

