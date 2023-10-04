# Geospatial analysis of the eBird data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties")

# Reading spatial data

install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages("vegan")
library("dplyr")
library("sf")
library("ggplot2")
library("vegan")


# Converting Richmond_nofilt.csv into spatial data using 'st_read' function of the sf package, the 'option'
# function to specify location of coordinates in the df and 'crs' to specify reference system
# I set digits = 9 to read the entire lon and lat data when I open dataframes

options(digits = 9)
Richmond_nofilt <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Counties_csv/Richmond_nofilt.csv", 
                        options = c("X_POSSIBLE_NAMES=lon", 
                                    "Y_POSSIBLE_NAMES=lat"), 
                        crs = 4326)

# trying to plot the observation data (now spatial object) to check it

ggplot() + 
  geom_sf(data = Richmond_nofilt)

# I load the created shapefile of the neighborhood tabulation areas (NTAs) of Richmond

richmond_nta_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Shapefiles_NTA_Counties/Richmond_NTA.shp")


# Plotting vector data

ggplot() + 
  geom_sf(data = richmond_nta_shp)

# Looking up the Coordinate Reference System (WGS84 for ebird data and NAD83 for NYC nta and ct map)

st_crs(richmond_nta_shp)
st_crs(Richmond_nofilt)

# I need to transform all of the spatial data into a single projection so that it's all represented in space in the same way
# I want the projection of ebird data to be the same of the NYC nta

nad83_richmond_nofilt <- st_transform(Richmond_nofilt, 2263)
st_crs(nad83_richmond_nofilt)

# Plotting together the different layers with the same CRS (NTAs and no filtered ebird data)

ggplot() + 
  geom_sf(data = richmond_nta_shp) + 
  geom_sf(data = nad83_richmond_nofilt)

# Now I calculate species richness (S) and diversity (H) index

# Before calculating the species richness (S) and diversity (H) I spatially join the spatial dataframe of the shapefile and the spatial dataframe of the ebd observations
# In this way I can have in the same dataframe the NTAName column, the sci_name column, and the obs_count column

spatial_join <- richmond_nta_shp %>% 
  st_join(nad83_richmond_nofilt)

# To easily view the new dataframe, I remove the geometry column

spatial_join <- st_set_geometry(spatial_join, NULL)

# First, to calculate the species richness index (S), I need to group by NTA name

species_by_nta <- group_by(spatial_join, NTAName)

# Now I use the 'summarise' function to do species richness (S) calculations on each NTA

S_NTA_Richmond <- summarise(species_by_nta, S = n_distinct(sci_name))

# Now I can proceed with the Shannon-Wiener Index (H) calculation, using the 'diversity' function of the package vegan
# Now, creating a new dataframe grouping by the NTA name and the scientific name (so grouping the sci_name by NTA), I can calculate what is the total number of observations
# (meaning individuals) per each species in each neighbourhood tabulation area, using the summarise and the sum functions of dplyr

obs_by_area_by_species <- group_by(spatial_join, NTAName, sci_name)

# I have to make the variable (column) obs_count numeric

obs_by_area_by_species$obs_count = as.numeric(obs_by_area_by_species$obs_count)

# Now I can calculate the number of individuals of a certain species in a certain NTA

n_ind_per_species <- summarise(obs_by_area_by_species, n_ind = sum(obs_count))

# I will now proceed with the calculation of the Shannon-Wiener Index (H) for diversity using the produced n_ind_per_species dataframe, combining the summarise function of the dplyr 
# package and the diversity function of the vegan package 

H_NTA_Richmond <- summarise(n_ind_per_species, H = diversity(n_ind, index = "shannon"))

# Exporting the S and H dataframes as .csv files

write.table(S_NTA_Richmond, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/Richmond_NTA/S_Richmond.csv", 
            row.names = F, sep = ",")
write.table(H_NTA_Richmond, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/Richmond_NTA/H_Richmond.csv", 
            row.names = F, sep = ",")

# Check if the produced .csv file is correct

options(digits = 9)
S_Richmond <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/Richmond_NTA/S_Richmond.csv", header = TRUE, sep = ",")
H_Richmond <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/Richmond_NTA/H_Richmond.csv", header = TRUE, sep = ",")

# -----------------------------------------------------------------------------------------------

# Now I do the same procedure to calculate S and H per census tracts per County

# Geospatial analysis of the eBird data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties")

# Reading spatial data

install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages("vegan")
library("dplyr")
library("sf")
library("ggplot2")
library("vegan")


# Converting Richmond_nofilt.csv into spatial data using 'st_read' function of the sf package, the 'option'
# function to specify location of coordinates in the df and 'crs' to specify reference system
# I set digits = 9 to read the entire lon and lat data when I open dataframes

options(digits = 9)
Richmond_nofilt <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Counties_csv/Richmond_nofilt.csv", 
                        options = c("X_POSSIBLE_NAMES=lon", 
                                    "Y_POSSIBLE_NAMES=lat"), 
                        crs = 4326)

# trying to plot the observation data (now spatial object) to check it

ggplot() + 
  geom_sf(data = Richmond_nofilt)

# I load the created shapefile of Richmond census tracts

richmond_ct_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Shapefiles_Census_Tracts_Counties/Richmond_CT.shp")

# Plotting vector data

ggplot() + 
  geom_sf(data = richmond_ct_shp)

# Looking up the Coordinate Reference System (WGS84 for ebird data and NAD83 for NYC ct map)

st_crs(richmond_ct_shp)
st_crs(Richmond_nofilt)

# I need to transform all of the spatial data into a single projection so that it's all represented in space in the same way
# I want the projection of ebird data to be the same of the NYC ct

nad83_richmond_nofilt <- st_transform(Richmond_nofilt, 2263)
st_crs(nad83_richmond_nofilt)

# Plotting together the different layers with the same CRS (CTs and no filtered ebird data)

ggplot() + 
  geom_sf(data = richmond_ct_shp) + 
  geom_sf(data = nad83_richmond_nofilt)

# Now I calculate species richness (S) and diversity (H) index

# Before calculating the species richness (S) and diversity (H) I spatially join the spatial dataframe of the shapefile and the spatial dataframe of the ebd observations
# In this way I can have in the same dataframe the CTLabel column, the sci_name column, and the obs_count column

spatial_join <- richmond_ct_shp %>% 
  st_join(nad83_richmond_nofilt)

# To easily view the new dataframe, I remove the geometry column

spatial_join <- st_set_geometry(spatial_join, NULL)

# First, to calculate the species richness index (S), I need to group by Census Tract label

species_by_ct <- group_by(spatial_join, CTLabel)

# Now I use the 'summarise' function to do species richness (S) calculations on each CT

S_CT_Richmond <- summarise(species_by_ct, S = n_distinct(sci_name))

# Renaming column

colnames(S_CT_Richmond)[colnames(S_CT_Richmond) == "CTLabel"] <- "CTLABEL"

# Adding column named 'BORONAME' filled with 'Staten Island'

S_CT_Richmond$BORONAME <- "Staten Island"

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

S_CT_Richmond <- unite(S_CT_Richmond, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Now I can proceed with the Shannon-Wiener Index (H) calculation, using the 'diversity' function of the package vegan
# Now, creating a new dataframe grouping by the CT Label and the scientific name (so grouping the sci_name by CTLabel), I can calculate what is the total number of observations (meaning individuals) per each species in each census tract, using the summarise and the sum functions of dplyr

obs_by_area_by_species <- group_by(spatial_join, CTLabel, sci_name)

# I have to make the variable (column) obs_count numeric

obs_by_area_by_species$obs_count = as.numeric(obs_by_area_by_species$obs_count)

# Now I can calculate the number of individuals of a certain species in a certain census tract

n_ind_per_species <- summarise(obs_by_area_by_species, n_ind = sum(obs_count))

# I will now proceed with the calculation of the Shannon-Wiener Index (H) for diversity using the produced n_ind_per_species dataframe, combining the summarise function of the dplyr package and the diversity function of the vegan package 

H_CT_Richmond <- summarise(n_ind_per_species, H = diversity(n_ind, index = "shannon"))

# Renaming column

colnames(H_CT_Richmond)[colnames(H_CT_Richmond) == "CTLabel"] <- "CTLABEL"

# Adding column named 'BORONAME' filled with 'Staten Island'

H_CT_Richmond$BORONAME <- "Staten Island"

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

H_CT_Richmond <- unite(H_CT_Richmond, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Exporting the S and H dataframes as .csv file

write.table(S_CT_Richmond, file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_Census_Tract_per_County/Richmond_ct/S_CT_Richmond.csv", row.names = F, sep = ",")
write.table(H_CT_Richmond, file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_Census_Tract_per_County/Richmond_ct/H_CT_Richmond.csv", row.names = F, sep = ",")
