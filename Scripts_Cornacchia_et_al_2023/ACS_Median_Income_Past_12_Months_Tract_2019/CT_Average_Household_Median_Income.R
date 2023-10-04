# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Median_Income_Past_12_Months_Tract_2019")

# Loading useful packages

install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)

# Importing data on 2019 median income data per census tract in Bronx, Kings (Brooklyn), Queens, and Richmond (Staten Island)

median_income_tract <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Median_Income_Past_12_Months_Tract_2019/All_Counties/Median_Income_Tract.csv", header = TRUE, sep = ",")

# Considering that I am interested in the median household income, I need to select this variable from the dataset
# Median household income estimate and margin of error are columns S1903_C03_001E and S1903_C03_001M respectively

hh_med_ct <- median_income_tract %>% select(NAME, S1903_C03_001E)

# For later calculations, I need to remove the first row of the hh_med_ct dataframe that do not contain useful data

hh_med_ct <- hh_med_ct[-1, ]

# Now, to be able to merge the produced dataframe with another dataframe that contains also CT Labels, I need to extract only the number of the census tracts from the NAME column

hh_med_ct$CTLABEL <- parse_number(hh_med_ct$NAME)

# Now I need to create a new column containing the names of the boroughs (or counties)
# To do this, I divide the first column 'NAME' in different columns based on the existing commas in the column

hh_med_ct_1 <- cSplit(hh_med_ct, "NAME", sep=",")

# I select only the columns that I need

hh_med_ct_1 <- hh_med_ct_1 %>% select(NAME_2, CTLABEL, S1903_C03_001E)

# Now I create a new column called 'BORONAME' containing only the names of the boroughs (counties) that I need for later steps

hh_med_ct_1$BORONAME[hh_med_ct_1$NAME_2 == "Bronx County"] <- "Bronx"
hh_med_ct_1$BORONAME[hh_med_ct_1$NAME_2 == "Kings County"] <- "Brooklyn"
hh_med_ct_1$BORONAME[hh_med_ct_1$NAME_2 == "Queens County"] <- "Queens"
hh_med_ct_1$BORONAME[hh_med_ct_1$NAME_2 == "Richmond County"] <- "Staten Island"

# I extract the column that I need

hh_med_ct_1 <- hh_med_ct_1 %>% select(CTLABEL, S1903_C03_001E, BORONAME)

# renaming column

colnames(hh_med_ct_1)[colnames(hh_med_ct_1) == "S1903_C03_001E"] <- "med_inc_ct"

# I convert the income column into a numeric column so that the NA values are displayed

hh_med_ct_1$med_inc_ct = as.numeric(hh_med_ct_1$med_inc_ct)

# I remove the two census tract in 'hh_med_ct_1' that are not present in the datasets that I used for calculations of S,H, NDVI etc.

hh_med_ct_1 <- hh_med_ct_1[-c(1100, 1769), ]

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

hh_med_ct_1 <- unite(hh_med_ct_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Exporting .csv file with average household median income per CT

write.table(hh_med_ct_1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Median_Income_Past_12_Months_Tract_2019/Calculated_Household_Median_Income_per_CT/ct_hh_med_inc.csv", 
            row.names = F, sep = ",")
