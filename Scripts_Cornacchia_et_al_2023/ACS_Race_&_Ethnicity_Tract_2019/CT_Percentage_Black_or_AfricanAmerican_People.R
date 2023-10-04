# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Race_&_Ethnicity_Tract_2019")

# Loading useful packages

install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)

# Importing data on 2019 race data per census tract in Bronx, Kings (Brooklyn), Queens, and Richmond (Staten Island)

race_tract <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Race_&_Ethnicity_Tract_2019/All_Counties/Race_Tract.csv", header = TRUE, sep = ",")

# Considering that I am interested in the % of black or African-American people, I need to select the variable of estimated total pop. per census tract and the variable of estimated total pop. of black or African-American people per census tract
# these two columns are respectively B02001_001E e B02001_003E

tot_pop_tot_bk <- race_tract %>% select(NAME, B02001_001E, B02001_003E)

# For later calculations, I need to remove the first row of the tot_pop_tot_bk dataframe that do not contain useful data

tot_pop_tot_bk <- tot_pop_tot_bk[-1, ]

# Now, to be able to merge the produced dataframe with another dataframe that contains also CT Labels, I need to extract only the number of the census tract from the NAME column

tot_pop_tot_bk$CTLABEL <- parse_number(tot_pop_tot_bk$NAME)

# Now I need to create a new column containing the names of the boroughs (or counties)
# To do this, I divide the first column 'NAME' in different columns based on the existing commas in the column

tot_pop_tot_bk_1 <- cSplit(tot_pop_tot_bk, "NAME", sep=",")

# I select only the columns that I need

tot_pop_tot_bk_1 <- tot_pop_tot_bk_1 %>% select(NAME_2, CTLABEL, B02001_001E, B02001_003E)

# Now I create a new column called 'BORONAME' containing only the names of the boroughs (counties) that I need for later steps

tot_pop_tot_bk_1$BORONAME[tot_pop_tot_bk_1$NAME_2 == "Bronx County"] <- "Bronx"
tot_pop_tot_bk_1$BORONAME[tot_pop_tot_bk_1$NAME_2 == "Kings County"] <- "Brooklyn"
tot_pop_tot_bk_1$BORONAME[tot_pop_tot_bk_1$NAME_2 == "Queens County"] <- "Queens"
tot_pop_tot_bk_1$BORONAME[tot_pop_tot_bk_1$NAME_2 == "Richmond County"] <- "Staten Island"

# I extract the column that I need

tot_pop_tot_bk_1 <- tot_pop_tot_bk_1 %>% select(CTLABEL, B02001_001E, B02001_003E, BORONAME)

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

tot_pop_tot_bk_1 <- unite(tot_pop_tot_bk_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# renaming columns

colnames(tot_pop_tot_bk_1)[colnames(tot_pop_tot_bk_1) == "B02001_001E"] <- "tot_pop_ct"
colnames(tot_pop_tot_bk_1)[colnames(tot_pop_tot_bk_1) == "B02001_003E"] <- "tot_bk_pop_ct"
str(tot_pop_tot_bk_1$tot_pop_ct)
str(tot_pop_tot_bk_1$tot_bk_pop_ct)

# I convert the population columns into numeric columns

tot_pop_tot_bk_1$tot_pop_ct = as.numeric(tot_pop_tot_bk_1$tot_pop_ct)
tot_pop_tot_bk_1$tot_bk_pop_ct = as.numeric(tot_pop_tot_bk_1$tot_bk_pop_ct)

# I remove the two census tract in 'ue_rt_ct_1' that are not present in the other dataframe

tot_pop_tot_bk_1 <- tot_pop_tot_bk_1[-c(1100, 1769), ]

# Now I calculate the % of black or African-American people per CT

tot_pop_tot_bk_1$pct_bk_ppl <- ((tot_pop_tot_bk_1$tot_bk_pop_ct) / (tot_pop_tot_bk_1$tot_pop_ct)) * 100

# I select only the variables that I need

tot_pop_tot_bk_1 <- tot_pop_tot_bk_1 %>% select(CT_BOR, pct_bk_ppl)

# Exporting .csv file with % of black or African-American people per CT

write.table(tot_pop_tot_bk_1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Race_&_Ethnicity_Tract_2019/Calculated_Percentage_Black_or_AfricanAmerican_People_per_CT/ct_pct_bk_ppl.csv", 
            row.names = F, sep = ",")
