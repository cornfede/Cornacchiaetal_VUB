# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Commuting_Characteristics_by_Sex_Tract_2019")

# Loading useful packages

install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)

# Importing data on 2019 commuting data per census tract in Bronx, Kings (Brooklyn), Queens, and Richmond (Staten Island)

com_tract <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Commuting_Characteristics_by_Sex_Tract_2019/All_Counties/Mean_Travel_Time.csv", header = TRUE, sep = ",")

# Considering that I am interested in the mean travel time to work, I need to select this variable from the dataset
# Mean travel time to work (minutes) estimate is column S0801_C01_046E

avg_tt_ct <- com_tract %>% select(NAME, S0801_C01_046E)

# For later calculations, I need to remove the first row of the avg_tt_ct dataframe that do not contain useful data

avg_tt_ct <- avg_tt_ct[-1, ]

# Now, to be able to merge the produced dataframe with another dataframe that contains also CT Labels, I need to extract only the number of the census tracts from the NAME column

avg_tt_ct$CTLABEL <- parse_number(avg_tt_ct$NAME)

# Now I need to create a new column containing the names of the boroughs (or counties)
# To do this, I divide the first column 'NAME' in different columns based on the existing commas in the column

avg_tt_ct_1 <- cSplit(avg_tt_ct, "NAME", sep=",")

# I select only the columns that I need

avg_tt_ct_1 <- avg_tt_ct_1 %>% select(NAME_2, CTLABEL, S0801_C01_046E)

# Now I create a new column called 'BORONAME' containing only the names of the boroughs (counties) that I need for later steps

avg_tt_ct_1$BORONAME[avg_tt_ct_1$NAME_2 == "Bronx County"] <- "Bronx"
avg_tt_ct_1$BORONAME[avg_tt_ct_1$NAME_2 == "Kings County"] <- "Brooklyn"
avg_tt_ct_1$BORONAME[avg_tt_ct_1$NAME_2 == "Queens County"] <- "Queens"
avg_tt_ct_1$BORONAME[avg_tt_ct_1$NAME_2 == "Richmond County"] <- "Staten Island"

# I extract the column that I need

avg_tt_ct_1 <- avg_tt_ct_1 %>% select(CTLABEL, S0801_C01_046E, BORONAME)

# renaming column

colnames(avg_tt_ct_1)[colnames(avg_tt_ct_1) == "S0801_C01_046E"] <- "avg_tt_ct"

# I convert the travel time column into a numeric column so that the NA values are displayed

avg_tt_ct_1$avg_tt_ct = as.numeric(avg_tt_ct_1$avg_tt_ct)

# I remove the two census tract in 'avg_tt_ct_1' that are not present in the datasets that I used for calculations of S,H, NDVI etc.

avg_tt_ct_1 <- avg_tt_ct_1[-c(1100, 1769), ]

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

avg_tt_ct_1 <- unite(avg_tt_ct_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Exporting .csv file with mean travel time to work per census tract

write.table(avg_tt_ct_1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Commuting_Characteristics_by_Sex_Tract_2019/Calculated_Mean_Travel_Time_to_Work_per_CT/avg_tt_ct.csv", 
            row.names = F, sep = ",")
