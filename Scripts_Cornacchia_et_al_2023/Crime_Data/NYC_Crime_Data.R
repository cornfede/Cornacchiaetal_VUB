# Install package to access publicly available police-recorded open crime data from large cities in the United States that are included in the Crime Open Database (https://osf.io/35n7u/)

install.packages("crimedata")
library(crimedata)
library(dplyr)
library(sf)
library(ggplot2)
library(raster)
library(sp)
crime_data <- get_crime_data()

nyc <- get_crime_data(
  years = 2019, 
  cities = "New York", 
  type = "core")

# Selecting columns

nyc <- nyc %>% select(offense_group, longitude, latitude)

# Now I save as .csv file the dataframe of nyc crime data 

write.table(nyc, file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Crime_Data/NYC_2019_Crime_Events.csv", row.names = F, sep = ",")

# Converting NYC_2019_Crime_Events.csv into spatial data using 'st_read' function of the sf package, 
# the 'option' function to specify location of coordinates in the df and 'crs' to specify reference system

nyc_crimes <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Crime_Data/NYC_2019_Crime_Events.csv", 
                        options = c("X_POSSIBLE_NAMES=longitude", 
                                    "Y_POSSIBLE_NAMES=latitude"), 
                        crs = 4326)

# I load the shapefiles of census tracts boundaries and NTA boundaries

ct_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Census_Blocks_&_Tracts/Census_Tracts_2010_Clipped_to_shoreline/nyct2010_21a/nyct2010.shp")
nta_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Neighborhood_Tabulation_Areas_&_PUMAs/Neighborhood_Tabulation_Areas/nynta_21a/nynta.shp")

# Looking up the Coordinate Reference Systems

st_crs(nyc_crimes)
st_crs(ct_shp)
st_crs(nta_shp)

ggplot() + 
  geom_sf(data = ct_shp)
ggplot() + 
  geom_sf(data = nta_shp)

# I need to transform all of the spatial data into a single projection so that it's all represented in space in the same way
# I want the projection of nyc crimes to be the same of the ct and nta boundaries

nad83_crimes <- st_transform(nyc_crimes, 2263)
st_crs(nad83_crimes)

# I need to remove the census tracts and the NTAs belonging to Manhattan

ct_NoMan <- ct_shp[!(ct_shp$BoroName == "Manhattan"), ]
nta_NoMan <- nta_shp[!(nta_shp$BoroName == "Manhattan"), ]

# Plotting together the different layers with the same CRS (ct and crimes)

ggplot() + 
  geom_sf(data = ct_NoMan) + 
  geom_sf(data = nad83_crimes)

# Plotting together the different layers with the same CRS (nta and crimes)

ggplot() + 
  geom_sf(data = nta_NoMan) + 
  geom_sf(data = nad83_crimes)

# I am interested in counting the total number of crimes per census tract and the total number of crimes per NTA

st_agr(ct_NoMan) = "constant"
st_agr(nta_NoMan) = "constant"
st_agr(nad83_crimes) = "constant"

intersection_ct <- st_intersection(x = ct_NoMan, y = nad83_crimes)
intersection_nta <- st_intersection(x = nta_NoMan, y = nad83_crimes)

crimes_per_ct <- intersection_ct %>% 
  group_by(CT_BOR) %>% 
  count()

crimes_per_nta <- intersection_nta %>% 
  group_by(NTAName) %>% 
  count()

# I remove the geometry column

crimes_per_ct <- st_set_geometry(crimes_per_ct, NULL)
crimes_per_nta <- st_set_geometry(crimes_per_nta, NULL)

# Crime rate is calculated dividing the number of crime events by the total population of the census tract or nta and then multiplying by 100000, because a crime rate describes the number of crimes for every 100000 persons (but I am going to modify this unit of measurement with 1000 for census tracts and 10000 for NTAs)

# Therefore I need to import the data sets containing the total population per census tract and per nta

pop_ct <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/Pop_per_CT.csv", header = TRUE, sep = ",")
pop_nta <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/Pop_per_NTA.csv", header = TRUE, sep = ",")

# I need to merge the population dataframes with the crimes dataframes
# However crimes_per_ct has 1870 rows (missing some census tract)

anti_join(pop_ct, crimes_per_ct, by = "CT_BOR")
anti_join(pop_nta, crimes_per_nta, by = "NTAName")
crimes_per_ct$CT_BOR[crimes_per_ct$CT_BOR=="170.10-Staten Island"] <- "170.1-Staten Island"

# I need to add 7 rows to the crimes_per_ct dataframe, with n = 0

str(crimes_per_ct$CT_BOR)
str(crimes_per_ct$n)
"1871" <- data.frame("702.03-Brooklyn", 0, row.names = "1871")
"1872" <- data.frame("613.02-Queens", 0, row.names = "1872")
"1873" <- data.frame("624-Queens", 0, row.names = "1873")
"1874" <- data.frame("793-Queens", 0, row.names = "1874")
"1875" <- data.frame("916.02-Queens", 0, row.names = "1875")
"1876" <- data.frame("999-Queens", 0, row.names = "1876")
"1877" <- data.frame("9901-Staten Island", 0, row.names = "1877")
names(`1871`) <- names(crimes_per_ct)
names(`1872`) <- names(crimes_per_ct)
names(`1873`) <- names(crimes_per_ct)
names(`1874`) <- names(crimes_per_ct)
names(`1875`) <- names(crimes_per_ct)
names(`1876`) <- names(crimes_per_ct)
names(`1877`) <- names(crimes_per_ct)
crimes_per_ct <- rbind(crimes_per_ct, `1871`)
crimes_per_ct <- rbind(crimes_per_ct, `1872`)
crimes_per_ct <- rbind(crimes_per_ct, `1873`)
crimes_per_ct <- rbind(crimes_per_ct, `1874`)
crimes_per_ct <- rbind(crimes_per_ct, `1875`)
crimes_per_ct <- rbind(crimes_per_ct, `1876`)
crimes_per_ct <- rbind(crimes_per_ct, `1877`)

# Now I can merge the dataframes

crimes_pop_ct <- merge(pop_ct, crimes_per_ct, by = "CT_BOR")
crimes_pop_nta <- merge(pop_nta, crimes_per_nta, by = "NTAName")

# Now I can add a new column calculating the crime rate per ct and per nta
# For NTA I multiply per 10000
# For census tracts I multiply per 1000 (see Nau et al., 2020)

crimes_pop_ct$crime_rt_ct <- (crimes_pop_ct$n / crimes_pop_ct$tot_pop) * 1000
crimes_pop_nta$crime_rt_nta <- (crimes_pop_nta$n / crimes_pop_nta$tot_pop) * 10000

# Selecting columns

crimes_rate_ct <- crimes_pop_ct %>% dplyr::select(CT_BOR, crime_rt_ct)
crimes_rate_nta <- crimes_pop_nta %>% dplyr::select(NTAName, crime_rt_nta)

# Replacing infinite values with NAs

is.na(crimes_rate_ct) <- sapply(crimes_rate_ct, is.infinite)
is.na(crimes_rate_nta) <- sapply(crimes_rate_nta, is.infinite)

# Exporting .csv files

write.table(crimes_rate_ct, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Crime_Data/Crime_Rate_CT.csv", 
            row.names = F, sep = ",")
write.table(crimes_rate_nta, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Crime_Data/Crime_Rate_NTA.csv", 
            row.names = F, sep = ",")
