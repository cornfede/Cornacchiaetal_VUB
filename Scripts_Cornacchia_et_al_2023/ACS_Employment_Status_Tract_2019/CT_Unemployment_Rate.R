# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Employment_Status_Tract_2019")

# Loading useful packages

install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)

# Importing data on 2019 employment status data per census tract in Bronx, Kings (Brooklyn), Queens, and Richmond (Staten Island)

employment_status_tract <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Employment_Status_Tract_2019/All_Counties/Employment_Status_Tract.csv", header = TRUE, sep = ",")

# Considering that I am interested in the unemployment rate of population 16 years and over, I need to select this variable from the dataset
# unemployment rate of population 16 years and over estimate is the column S2301_C04_001E

ue_rt_ct <- employment_status_tract %>% select(NAME, S2301_C04_001E)

# For later calculations, I need to remove the first row of the ue_rt_ct dataframe that do not contain useful data

ue_rt_ct <- ue_rt_ct[-1, ]

# Now, to be able to merge the produced dataframe with another dataframe that contains also Ct Labels, I need to extract only the number of the census tract from the NAME column

ue_rt_ct$CTLABEL <- parse_number(ue_rt_ct$NAME)

# Now I need to create a new column containing the names of the boroughs (or counties)
# To do this, I divide the first column 'NAME' in different columns based on the existing commas in the column

ue_rt_ct_1 <- cSplit(ue_rt_ct, "NAME", sep=",")

# I select only the columns that I need

ue_rt_ct_1 <- ue_rt_ct_1 %>% select(NAME_2, CTLABEL, S2301_C04_001E)

# Now I create a new column called 'BORONAME' containing only the names of the boroughs (counties) that I need for later steps

ue_rt_ct_1$BORONAME[ue_rt_ct_1$NAME_2 == "Bronx County"] <- "Bronx"
ue_rt_ct_1$BORONAME[ue_rt_ct_1$NAME_2 == "Kings County"] <- "Brooklyn"
ue_rt_ct_1$BORONAME[ue_rt_ct_1$NAME_2 == "Queens County"] <- "Queens"
ue_rt_ct_1$BORONAME[ue_rt_ct_1$NAME_2 == "Richmond County"] <- "Staten Island"

# I extract the column that I need

ue_rt_ct_1 <- ue_rt_ct_1 %>% select(CTLABEL, S2301_C04_001E, BORONAME)

# renaming column

colnames(ue_rt_ct_1)[colnames(ue_rt_ct_1) == "S2301_C04_001E"] <- "ue_rt_ct"

# I convert the income column into a numeric column so that the NA values are displayed

ue_rt_ct_1$ue_rt_ct = as.numeric(ue_rt_ct_1$ue_rt_ct)

# I remove the two census tract in 'ue_rt_ct_1' that are not present in the other dataframe

ue_rt_ct_1 <- ue_rt_ct_1[-c(1100, 1769), ]

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

ue_rt_ct_1 <- unite(ue_rt_ct_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Exporting .csv file with unemployment rate per CT

write.table(ue_rt_ct_1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Employment_Status_Tract_2019/Calculated_Unemployment_Rate_per_CT/ct_ue_rt.csv", row.names = F, sep = ",")
