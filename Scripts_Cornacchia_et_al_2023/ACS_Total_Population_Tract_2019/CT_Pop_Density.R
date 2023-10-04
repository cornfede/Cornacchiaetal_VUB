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

# Now, to be able to merge the produced dataframe with another dataframe that contains also CT Labels, I need to extract only the number of the census tracts from the NAME column

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

# I convert the pop column into a numeric column so that the NA values are displayed

pop_ct_1$tot_pop = as.numeric(pop_ct_1$tot_pop)

# I remove the two census tract that are not present in the datasets that I used for calculations of S,H, NDVI etc.

pop_ct_1 <- pop_ct_1[-c(1100, 1769), ]

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

pop_ct_1 <- unite(pop_ct_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Exporting .csv file 

write.table(pop_ct_1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/Pop_per_CT.csv", 
            row.names = F, sep = ",")

# Now I import the dataset with the census tract areas

tract <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/CT_Area.csv", header = TRUE, sep = ",")

# Removing Manhattan rows

tract <- tract[!(tract$BoroName == "Manhattan"), ]

# Selecting columns

area_tract <- tract %>% select(CT_BOR, Shape_Area)

# Checking unmatch

anti_join(pop_ct_1, area_tract, by = "CT_BOR")

# Modifying row

area_tract$CT_BOR[area_tract$CT_BOR=="170.10-Staten Island"] <- "170.1-Staten Island"

# Merging dataframes

pop_area_ct <- merge(pop_ct_1, area_tract, by = "CT_BOR")

# Calculating area in square miles

pop_area_ct$area_miles <- pop_area_ct$Shape_Area * 0.00000003587006428

# Now I can calculate Population Density per square mile per census tract (number of people/land area) adding a new column

pop_area_ct$pop_den_ct <- pop_area_ct$tot_pop / pop_area_ct$area_miles

# Selecting columns

pop_area_ct <- pop_area_ct %>% select(CT_BOR, pop_den_ct)

# Exporting .csv file 

write.table(pop_area_ct, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/CT_Pop_Density.csv", 
            row.names = F, sep = ",")
