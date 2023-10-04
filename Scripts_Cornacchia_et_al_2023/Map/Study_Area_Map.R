# Loading packages

library(ggsn)
library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(tidyr)
library(data.table)
library(splitstackshape)
library(leaflet)
install.packages("tmap")
library(tmap)
install.packages("openintro")
install.packages("palmerpenguins")
install.packages("maps")
install.packages("ggthemes")
install.packages()
library(tidyverse)
library(lubridate)
library(openintro)
library(palmerpenguins)
library(maps)
library(ggmap)
library(ggthemes)
install.packages("mapview")
library(mapview)

# I load the created shapefile of the nyc borough boundaries

bor_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Map/Borough_Boundaries_Shapefile/geo_export_6ad50992-466c-49d4-a8ed-67ac9ff9e418.shp")

# I need to transform the projection of the shapefile into NAD83

nad83_bor <- st_transform(bor_shp, 2263)
st_crs(nad83_bor)

# Plotting together the different layers with the same CRS

ggplot() + 
  geom_sf(data = nad83_bor)

nad83_bor %>% 
  mapview(color = "black", lwd = 1, layer.name = "nad83_bor", alpha.regions = 0.0, col.regions = "light blue")

?mapview


