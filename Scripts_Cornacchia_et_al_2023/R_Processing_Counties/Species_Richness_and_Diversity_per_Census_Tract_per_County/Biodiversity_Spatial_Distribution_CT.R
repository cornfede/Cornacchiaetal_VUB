# I want to show the citizen science biodiversity observation in the NYC Counties

# Loading packages

install.packages('ggsn')
library(ggsn)
library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(tidyr)
library(data.table)
library(splitstackshape)
library(leaflet)

# I load the created shapefile of the nyc census tracts

ct_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Census_Blocks_&_Tracts/Census_Tracts_2010_Clipped_to_shoreline/nyct2010_21a/nyct2010.shp")

# I need to upload all the ebird biodiversity observations
# Importing and combining .csv files with ebird data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/ebird_observations_per_county_except_manhattan")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep = ",")
ebird_obs <- rbindlist( temp )

# Exporting ebird_obs as .csv file in folder "Counties_csv"

write.table(ebird_obs, file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Counties_csv/ebird_obs_noManhattan.csv", row.names = F, sep = ",")

# Converting ebird_obs_noManhattan.csv into spatial data using 'st_read' function of the sf package

obs <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Counties_csv/ebird_obs_noManhattan.csv", 
                        options = c("X_POSSIBLE_NAMES=lon", 
                                    "Y_POSSIBLE_NAMES=lat"), 
                        crs = 4326)

# I need to transform all of the spatial data into a single projection so that it's all represented in space in the same way
# I want the projection of ebird data to be the same of the NYC ct shapefile

nad83_obs <- st_transform(obs, 2263)
st_crs(nad83_obs)

-----------------------------------------------------------------------------------------------------

# I export the spatial dataframe nad83_obs as .csv file for later visualisations in ArcGIS

# write.table(nad83_obs, 
            #file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Housing_Transaction_Data_Preprocessing/Geocoded_Filtered_Sales_Shapefile/Sales.csv", 
            #row.names = F, sep = ",")

----------------------------------------------------------------------------------------------------------------------------------

# Plotting together the different layers with the same CRS

ggplot() + 
  geom_sf(data = ct_shp) + 
  geom_sf(data = nad83_obs) +
  cowplot::theme_cowplot() +
  theme_void() +
  north(ct_shp, location = "topleft", symbol = 3)

# Now I want to create choropleth maps to show differences in species richness (S) and relative abundance between census tracts

# Importing and combining .csv files with S data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_Census_Tract_per_County/S_Br_Ki_Qu_Ri")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep = ",")
S_Counties <- rbindlist( temp )

# Importing and combining .csv files with H data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_Census_Tract_per_County/H_Br_Ki_Qu_Ri")
files1 <- list.files(pattern = ".csv")
temp1 <- lapply(files1, fread, sep = ",")
H_Counties <- rbindlist( temp1 )

# Checking the match by the column 'CT_BOR'

anti_join(ct_shp, S_Counties, by = "CT_BOR")

# I need to eliminate all the rows of ct_shp belonging to Manhattan

ct_shp <- ct_shp[ct_shp$BoroName != "Manhattan", ]

# Now I want to merge the S dataframe with the ct_shp by the column 'CT_BOR'

S_CT <- merge(ct_shp, S_Counties, by = "CT_BOR")

# Now I want to merge the H dataframe with the ct_shp by the column 'CT_BOR'

H_CT <- merge(ct_shp, H_Counties, by = "CT_BOR")

# In the H column, I have numerous NAs that I transform in zeros

H_CT$H[which(is.na(H_CT$H))] <- 0 

# Selecting columns

S_CT <- S_CT %>% select(CT_BOR, BoroName, S)
H_CT <- H_CT %>% select(CT_BOR, BoroName, H)

# Looking at census tracts with S smaller than 100

S_100 <- filter(S_CT, S <= 100)
S_10 <- filter(S_CT, S <= 10)

# Looking at census tracts with S between 100 and 200

S_100_200 <- filter(S_CT, S > 100 & S <= 200)

# Looking at census tracts with S between 200 and 300

S_200_300 <- filter(S_CT, S > 200 & S <= 300)

# Looking at census tracts with S Greater than 300

S_300 <- filter(S_CT, S > 300)

# Looking at census tracts with H smaller than 1

H_1 <- filter(H_CT, H <= 1)

# Looking at census tracts with H between 1 and 2

H_1_2 <- filter(H_CT, H > 1 & H <= 2)

# Looking at census tracts with H between 2 and 3

H_2_3 <- filter(H_CT, H > 2 & H <= 3)

# Looking at census tracts with H between 3 and 4

H_3_4 <- filter(H_CT, H > 3 & H <= 4)

# Looking at census tracts with H Greater than 4

H_4 <- filter(H_CT, H > 4)

# Selecting columns

S_CT <- S_CT %>% select(S)
H_CT <- H_CT %>% select(H)

# plotting 

plot(S_CT)
plot(H_CT)
?plot

# Plotting with ggplot2

ggplot(S_CT) + geom_sf(aes(fill = S)) +
  scale_fill_viridis_c(option = "viridis") +
  cowplot::theme_cowplot() +
  theme_void() +
  north(S_CT, location = "topleft", symbol = 3)

ggplot(H_CT) + geom_sf(aes(fill = H)) +
  scale_fill_viridis_c(option = "viridis") +
  cowplot::theme_cowplot() +
  theme_void() +
  north(H_CT, location = "topleft", symbol = 3)

# I want to count how many observation events are in each census tract

count <- ct_shp %>% 
  mutate(counts = lengths(st_intersects(., nad83_obs)))

# Counting census tract with no observation events

sum(count$counts == 0)
sum(count$counts != 0)
