# I want to show the citizen science biodiversity observation in the NYC Counties

# Loading packages

library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(tidyr)
library(data.table)
library(ggsn)

# I load the created shapefile of the nyc neighborhood tabulation areas

nta_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Neighborhood_Tabulation_Areas_&_PUMAs/Neighborhood_Tabulation_Areas/nynta_21a/nynta.shp")
                   
# I need to eliminate all the rows of nta_shp belonging to Manhattan
                   
nta_shp <- nta_shp[nta_shp$BoroName != "Manhattan", ]

# Converting ebird_obs_noManhattan.csv into spatial data using 'st_read' function of the sf package

obs <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Counties_csv/ebird_obs_noManhattan.csv", 
               options = c("X_POSSIBLE_NAMES=lon", 
                           "Y_POSSIBLE_NAMES=lat"), 
               crs = 4326)

# I need to transform all of the spatial data into a single projection so that it's all represented in space in the same way
# I want the projection of ebird data to be the same of the NYC nta shapefile

nad83_obs <- st_transform(obs, 2263)
st_crs(nad83_obs)

# Plotting together the different layers with the same CRS

ggplot() + 
  geom_sf(data = nta_shp) + 
  geom_sf(data = nad83_obs) +
  cowplot::theme_cowplot() +
  theme_void() +
  north(nta_shp, location = "topleft", symbol = 3)

# Now I want to create choropleth maps to show differences in species richness (S) and relative abundance between NTAs

# Importing and combining .csv files with S data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/S_Br_Ki_Qu_Ri")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep = ",")
S_Counties <- rbindlist( temp )

# Importing and combining .csv files with H data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/H_Br_Ki_Qu_Ri")
files1 <- list.files(pattern = ".csv")
temp1 <- lapply(files1, fread, sep = ",")
H_Counties <- rbindlist( temp1 )

# Checking the match by the column 'NTAName'

anti_join(nta_shp, S_Counties, by = "NTAName")

# Now I want to merge the S dataframe with the nta_shp by the column 'NTAName'

S_NTA <- merge(nta_shp, S_Counties, by = "NTAName")

# Now I want to merge the H dataframe with the nta_shp by the column 'NTAName'

H_NTA <- merge(nta_shp, H_Counties, by = "NTAName")

# In the H column, I have numerous NAs that I transform in zeros

H_NTA$H[which(is.na(H_NTA$H))] <- 0 

# Selecting columns

S_NTA <- S_NTA %>% select(NTAName, BoroName, S)
H_NTA <- H_NTA %>% select(NTAName, BoroName, H)

# Looking at NTAs with S smaller than 100

S_100 <- filter(S_NTA, S <= 100)
S_10 <- filter(S_NTA, S <= 10)

# Looking at NTAs with S between 100 and 200

S_100_200 <- filter(S_NTA, S > 100 & S <= 200)

# Looking at NTAs with S between 200 and 300

S_200_300 <- filter(S_NTA, S > 200 & S <= 300)

# Looking at NTAs with S Greater than 300

S_300 <- filter(S_NTA, S > 300)

# Looking at NTAs with H smaller than 1

H_1 <- filter(H_NTA, H <= 1)

# Looking at NTAs with H between 1 and 2

H_1_2 <- filter(H_NTA, H > 1 & H <= 2)

# Looking at NTAs with H between 2 and 3

H_2_3 <- filter(H_NTA, H > 2 & H <= 3)

# Looking at NTAs with H between 3 and 4

H_3_4 <- filter(H_NTA, H > 3 & H <= 4)

# Looking at NTAs with H Greater than 4

H_4 <- filter(H_NTA, H > 4)

# Selecting columns

S_NTA <- S_NTA %>% select(S)
H_NTA <- H_NTA %>% select(H)

# plotting 

plot(S_NTA)
plot(H_NTA)
?plot

# Plotting with ggplot2

ggplot(S_NTA) + geom_sf(aes(fill = S)) +
  scale_fill_viridis_c(option = "viridis") +
  cowplot::theme_cowplot() +
  theme_void() +
  north(S_NTA, location = "topleft", symbol = 3)

ggplot(H_NTA) + geom_sf(aes(fill = H)) +
  scale_fill_viridis_c(option = "viridis") +
  cowplot::theme_cowplot() +
  theme_void() +
  north(H_NTA, location = "topleft", symbol = 3)

# I want to count how many observation events are in each NTA

count <- nta_shp %>% 
  mutate(counts = lengths(st_intersects(., nad83_obs)))

# Counting census tract with no observation events

sum(count$counts == 0)
sum(count$counts != 0)
