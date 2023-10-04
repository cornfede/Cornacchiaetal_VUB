# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019")

# Loading useful packages

install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)

# Importing data on 2019 population per census tract in Bronx, Kings (Brooklyn), Queens, and Richmond (Staten Island)

pop_tract <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/All_Counties/Population.csv", header = TRUE, sep = ",")

# Considering that I am interested in the total population, I need to select this variable from the dataset
# column B01003_001E

pop_ct <- pop_tract %>% select(NAME, B01003_001E)

# For later calculations, I need to remove the first row of the dataframe that do not contain useful data

pop_ct <- pop_ct[-1, ]

# Now, to be able to merge the produced dataframe with another dataframe that contains also NTA names, I need to extract only the number of the census tracts from the NAME column

pop_ct$CTLABEL <- parse_number(pop_ct$NAME)

# Now I need to create a new column containing the names of the boroughs (or counties)
# To do this, I divide the first column 'NAME' in different columns based on the existing commas in the column

pop_ct_1 <- cSplit(pop_ct, "NAME", sep=",")

# I select only the columns that I need

pop_ct_1 <- pop_ct_1 %>% select(NAME_2, CTLABEL, B01003_001E)

# Now I create a new column called 'BORONAME' containing only the names of the boroughs (counties) that I need for later steps

pop_ct_1$BORONAME[pop_ct_1$NAME_2 == "Bronx County"] <- "Bronx"
pop_ct_1$BORONAME[pop_ct_1$NAME_2 == "Kings County"] <- "Brooklyn"
pop_ct_1$BORONAME[pop_ct_1$NAME_2 == "Queens County"] <- "Queens"
pop_ct_1$BORONAME[pop_ct_1$NAME_2 == "Richmond County"] <- "Staten Island"

# I extract the column that I need

pop_ct_1 <- pop_ct_1 %>% select(CTLABEL, B01003_001E, BORONAME)

# renaming column

colnames(pop_ct_1)[colnames(pop_ct_1) == "B01003_001E"] <- "tot_pop"

# I convert the travel time column into a numeric column so that the NA values are displayed

pop_ct_1$tot_pop = as.numeric(pop_ct_1$tot_pop)

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

pop_ct_1 <- unite(pop_ct_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Now I import the dataset with the census tract areas (with associated NTA name per census tract)

tract_nta <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/CT_Area.csv", header = TRUE, sep = ",")

# Removing Manhattan rows

tract_nta <- tract_nta[!(tract_nta$BoroName == "Manhattan"), ]

# Selecting columns

tract_nta <- tract_nta %>% select(CT_BOR, NTAName)

# Modifying row

tract_nta$CT_BOR[tract_nta$CT_BOR=="170.10-Staten Island"] <- "170.1-Staten Island"

# Merging the two dataframes

pop_ct_nta <- merge(pop_ct_1, tract_nta, by = "CT_BOR")

# Now I can calculate the total population per NTA

group_by_nta <- group_by(pop_ct_nta, NTAName)
nta_tot_pop <- summarise(group_by_nta, tot_pop = sum(tot_pop, na.rm=TRUE))

# Exporting .csv file 

write.table(nta_tot_pop, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/Pop_per_NTA.csv", 
            row.names = F, sep = ",")

# Now I import the dataset with NTA areas

nta_area <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/NTA_Area.csv", header = TRUE, sep = ",")

# Removing Manhattan rows

nta_area <- nta_area[!(nta_area$BoroName == "Manhattan"), ]

# Selecting columns

nta_area <- nta_area %>% select(NTAName, Shape_Area)

# Merging dataframes

pop_area <- merge(nta_tot_pop, nta_area, by = "NTAName")

# Calculating area in square miles

pop_area$area_miles <- pop_area$Shape_Area * 0.00000003587006428

# Now I can calculate Population Density per square mile per NTA (number of people/land area) adding a new column

pop_area$pop_den_nta <- pop_area$tot_pop / pop_area$area_miles

# Selecting columns

pop_area <- pop_area %>% select(NTAName, pop_den_nta)

# Exporting .csv file 

write.table(pop_area, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/NTA_Pop_Density.csv", 
            row.names = F, sep = ",")
