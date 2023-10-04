# Loading packages

library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)
library(data.table)

# I want to combine the different .csv file of species richness index (S) and diversity index (H) per census tract that I have per each County into one dataframe
# Importing S and H .csv files
# Considering that I do not have single-family housing transactions in Manhattan I won't import the S and H data regarding Manhattan (New York)

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

# Now I want to merge the two dataframes by the column 'CT_BOR'

S_H_Tract <- merge(S_Counties, H_Counties, by = "CT_BOR")

# Now I import the .csv file containing mean NDVI data per Census Tract

NDVI <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/NDVI_Landsat8_OLI_TIRS_C2_L2/NYC_NDVI/Mean_NDVI_per_Census_Tract.csv", header = TRUE, sep = ",")

# I merge the two dataframes

S_H_NDVI <- merge(NDVI, S_H_Tract, by = "CT_BOR")

# Renaming one row of the 'inc' dataframe

S_H_NDVI$CT_BOR[S_H_NDVI$CT_BOR=="170.10-Staten Island"] <- "170.1-Staten Island"

# Now I can import the .csv file with the household median income per census tract

inc <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Median_Income_Past_12_Months_Tract_2019/Calculated_Household_Median_Income_per_CT/ct_hh_med_inc.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

inc_S_H_NDVI <- merge(S_H_NDVI, inc, by = "CT_BOR")

# Now I can import the .csv file with the unemployment rate per census tract

ue_rt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Employment_Status_Tract_2019/Calculated_Unemployment_Rate_per_CT/ct_ue_rt.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

ue_inc_S_H_NDVI <- merge(inc_S_H_NDVI, ue_rt, by = "CT_BOR")

# Now I can import the .csv file with the percentage of black or African-American people per census tract

pct_bk <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Race_&_Ethnicity_Tract_2019/Calculated_Percentage_Black_or_AfricanAmerican_People_per_CT/ct_pct_bk_ppl.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

bk_ue_inc_S_H_NDVI <- merge(ue_inc_S_H_NDVI, pct_bk, by = "CT_BOR")

# Now I can import the .csv file with the poverty rate per census tract

pv_rt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Poverty_Status_Past_12_Months_Tract_2019/Calculated_Poverty_Rate_per_CT/ct_pv_rt.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

pv_bk_ue_inc_S_H_NDVI <- merge(bk_ue_inc_S_H_NDVI, pv_rt, by = "CT_BOR")

# Now I can import the .csv file with population density per census tract (per square mile)

pop_den <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/CT_Pop_Density.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

den_pv_bk_ue_inc_S_H_NDVI <- merge(pv_bk_ue_inc_S_H_NDVI, pop_den, by = "CT_BOR")

# Now I import the .csv file with average travel time to work (minutes) per census tract

avg_tt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Commuting_Characteristics_by_Sex_Tract_2019/Calculated_Mean_Travel_Time_to_Work_per_CT/avg_tt_ct.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

tt_den_pv_bk_ue_inc_S_H_NDVI <- merge(den_pv_bk_ue_inc_S_H_NDVI, avg_tt, by = "CT_BOR")

# Now I import the .csv file with crime rate per census tract (per 1000 people)

crime_rt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Crime_Data/Crime_Rate_CT.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

crime_tt_den_pv_bk_ue_inc_S_H_NDVI <- merge(tt_den_pv_bk_ue_inc_S_H_NDVI, crime_rt, by = "CT_BOR")

# Now I import the .csv that contains the structural variables and the proximity variables and the dependent variable (adjusted price)

str_prox_var <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Sales_Joined_to_Census_Tracts/Sales_with_Census_Tract.csv", header = TRUE, sep = ",")

# I select only the columns that I need

str_prox_var <- str_prox_var %>% select(CTLabel, BoroName, ADJ_PRICE, LAND_SQUAR, GROSS_SQUA, AGE_PROPER, SCH_DIST, WT_DIST, CBD_DIST)

# Merging 'CTLabel' and 'BoroName' columns

str_prox_var <- unite(str_prox_var, CT_BOR, CTLabel, BoroName, sep = "-")

# Checking the match by the column 'CT_BOR'

anti_join(str_prox_var, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = 'CT_BOR')

# Now I can merge the dataframes (I will lose only one row due to the one NA in the CT_BOR column)

variables <- merge(str_prox_var, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = "CT_BOR")

# Renaming

names(variables)[names(variables) == "ADJ_PRICE" ] <- "price"
names(variables)[names(variables) == "LAND_SQUAR" ] <- "land_square_feet"
names(variables)[names(variables) == "GROSS_SQUA" ] <- "gross_square_feet"
names(variables)[names(variables) == "AGE_PROPER" ] <- "age_property"
names(variables)[names(variables) == "SCH_DIST" ] <- "sch_dist"
names(variables)[names(variables) == "WT_DIST" ] <- "wt_dist"
names(variables)[names(variables) == "CBD_DIST" ] <- "cbd_dist"

# Exporting the .csv file with the final variables for later regression analysis

write.table(variables, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_CT.csv", 
            row.names = F, sep = ",")

# ---------------------------------------------------------------------------------------------------------------------

# Now to obtain a different dataset with more aggragated data I calculate the aggregated mean of the variables

# I need to aggregate the mean of ADJ_PRICE, LAND_SQUAR, GROSS_SQUA, AGE_PROPER, SCH_DIST, WT_DIST, CBD_DIST on the CT level
# To this end, I have to first group by CT_BOR

var_by_ct <- group_by(str_prox_var, CT_BOR)

# Now I can calculated the aggregate mean on the CT level

agg_mean <- var_by_ct %>% summarize(avg_price = mean(ADJ_PRICE), 
                                     avg_land_square_feet = mean(LAND_SQUAR), 
                                     avg_gross_square_feet = mean(GROSS_SQUA), 
                                     avg_age = mean(AGE_PROPER), 
                                     avg_sch_dist = mean(SCH_DIST), 
                                     avg_wt_dist = mean(WT_DIST), 
                                     avg_cbd_dist = mean(CBD_DIST))

# Checking the match by the column 'NTANAME'

anti_join(agg_mean, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = 'CT_BOR')

# Merging. I will lose one row (obs.) of the 1130 observations of 'agg_mean' due to the one NA in the CT_BOR column

variables1 <- merge(agg_mean, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = "CT_BOR")

# Exporting the .csv file with the final variables for later regression analysis

write.table(variables1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_Agg_CT.csv", 
            row.names = F, sep = ",")
