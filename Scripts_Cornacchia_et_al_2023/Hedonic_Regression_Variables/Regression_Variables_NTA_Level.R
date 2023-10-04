# Loading packages

library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)
library(data.table)

# I want to combine the different .csv file of species richness index (S) and diversity index (H) per NTA that I have per each County into one dataframe
# Importing S and H .csv files
# Considering that I do not have single-family housing transactions in Manhattan I won't import the S and H data regarding Manhattan (New York)

# Importing and combining .csv files with S data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/S_Br_Ki_Qu_Ri")
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep = ",")
S_Counties <- rbindlist( temp )

# Importing and combining .csv files with H data

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Species_Richness_and_Diversity_per_NTA_per_County/H_Br_Ki_Qu_Ri")
files1 <- list.files(pattern = ".csv")
temp1 <- lapply(files1, fread, sep = ",")
H_Counties <- rbindlist( temp1 )

# Now I want to merge the two dataframes by the column 'NTAName'

S_H_NTA <- merge(S_Counties, H_Counties, by = "NTAName")

# Renaming the column 'NTAName'

names(S_H_NTA)[names(S_H_NTA) == "NTAName" ] <- "NTANAME"

# Now I import the .csv file containing mean NDVI data per NTA

NDVI <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/NDVI_Landsat8_OLI_TIRS_C2_L2/NYC_NDVI/NTA_Mean_NDVI/Mean_NDVI_per_NTA.csv", header = TRUE, sep = ",")

# I need to remove the NTAs belonging to Manhattan (New York) in order to merge this dataframe with the 'S_H_NTA' dataframe
# I check what are the rows that I need to remove

anti_join(NDVI, S_H_NTA, by = 'NTANAME')

# I remove the NTAs in 'NDVI' that are not present in the other dataframe

NDVI <- NDVI[NDVI$ï..OID_ != 33, ]
NDVI <- NDVI[NDVI$ï..OID_ != 48, ]
NDVI <- NDVI[NDVI$ï..OID_ != 49, ]
NDVI <- NDVI[NDVI$ï..OID_ != 50, ]
NDVI <- NDVI[NDVI$ï..OID_ != 51, ]
NDVI <- NDVI[NDVI$ï..OID_ != 65, ]
NDVI <- NDVI[NDVI$ï..OID_ != 69, ]
NDVI <- NDVI[NDVI$ï..OID_ != 70, ]
NDVI <- NDVI[NDVI$ï..OID_ != 71, ]
NDVI <- NDVI[NDVI$ï..OID_ != 72, ]
NDVI <- NDVI[NDVI$ï..OID_ != 88, ]
NDVI <- NDVI[NDVI$ï..OID_ != 89, ]
NDVI <- NDVI[NDVI$ï..OID_ != 91, ]
NDVI <- NDVI[NDVI$ï..OID_ != 94, ]
NDVI <- NDVI[NDVI$ï..OID_ != 95, ]
NDVI <- NDVI[NDVI$ï..OID_ != 107, ]
NDVI <- NDVI[NDVI$ï..OID_ != 111, ]
NDVI <- NDVI[NDVI$ï..OID_ != 116, ]
NDVI <- NDVI[NDVI$ï..OID_ != 127, ]
NDVI <- NDVI[NDVI$ï..OID_ != 131, ]
NDVI <- NDVI[NDVI$ï..OID_ != 132, ]
NDVI <- NDVI[NDVI$ï..OID_ != 146, ]
NDVI <- NDVI[NDVI$ï..OID_ != 147, ]
NDVI <- NDVI[NDVI$ï..OID_ != 178, ]
NDVI <- NDVI[NDVI$ï..OID_ != 179, ]
NDVI <- NDVI[NDVI$ï..OID_ != 186, ]
NDVI <- NDVI[NDVI$ï..OID_ != 187, ]
NDVI <- NDVI[NDVI$ï..OID_ != 194, ]
NDVI <- NDVI[NDVI$ï..OID_ != 195, ]

# I select only the columns of 'NDVI' that I need

NDVI <- NDVI %>% select(NTANAME, MEAN)

# Renaming the column 'MEAN'

names(NDVI)[names(NDVI) == "MEAN" ] <- "mean_ndvi"

# Now I can merge the two dataframes

S_H_NDVI <- merge(NDVI, S_H_NTA, by = "NTANAME")

# Now I can import the .csv file with the (average) household median income per NTA

inc <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Median_Income_Past_12_Months_Tract_2019/Calculated_Average_Household_Median_Income_per_NTA/nta_avg_hh_med_inc.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

inc_S_H_NDVI <- merge(S_H_NDVI, inc, by = "NTANAME")

# Now I can import the .csv file with the average unemployment rate per NTA

ue_rt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Employment_Status_Tract_2019/Calculated_Average_Unemployment_Rate_per_NTA/nta_avg_ue_rt.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

ue_inc_S_H_NDVI <- merge(inc_S_H_NDVI, ue_rt, by = "NTANAME")

# Now I can import the .csv file with the percentage of black or African-American people per NTA

pct_bk <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Race_&_Ethnicity_Tract_2019/Calculated_Percentage_Black_or_AfricanAmerican_People_per_NTA/nta_pct_bk_ppl.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

bk_ue_inc_S_H_NDVI <- merge(ue_inc_S_H_NDVI, pct_bk, by = "NTANAME")

# Now I can import the .csv file with the poverty rate per NTA

pv_rt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Poverty_Status_Past_12_Months_Tract_2019/Calculated_Poverty_Rate_per_NTA/nta_pv_rt.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

pv_bk_ue_inc_S_H_NDVI <- merge(bk_ue_inc_S_H_NDVI, pv_rt, by = "NTANAME")

# Now I import the .csv file with population density per NTA (per square mile)

pop_den <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Total_Population_Tract_2019/NTA_Pop_Density.csv", header = TRUE, sep = ",")

# Renaming the column 'NTAName'

names(pop_den)[names(pop_den) == "NTAName" ] <- "NTANAME"

# And I can merge this with the previous dataframe

den_pv_bk_ue_inc_S_H_NDVI <- merge(pv_bk_ue_inc_S_H_NDVI, pop_den, by = "NTANAME")

# Now I import the .csv file with average travel time to work (minutes) per NTA

avg_tt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Commuting_Characteristics_by_Sex_Tract_2019/Calculated_Mean_Travel_Time_to_Work_per_NTA/avg_tt_nta.csv", header = TRUE, sep = ",")

# And I can merge this with the previous dataframe

tt_den_pv_bk_ue_inc_S_H_NDVI <- merge(den_pv_bk_ue_inc_S_H_NDVI, avg_tt, by = "NTANAME")

# Now I import the .csv file with crime rate per NTA (per 10000 people)

crime_rt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Crime_Data/Crime_Rate_NTA.csv", header = TRUE, sep = ",")

# Renaming the column 'NTAName'

names(crime_rt)[names(crime_rt) == "NTAName" ] <- "NTANAME"

# And I can merge this with the previous dataframe

crime_tt_den_pv_bk_ue_inc_S_H_NDVI <- merge(tt_den_pv_bk_ue_inc_S_H_NDVI, crime_rt, by = "NTANAME")

# Now I import the .csv that contains the structural variables, proximity variables and the dependent variable (adjusted price)

str_prox_var <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Sales_Joined_to_Census_Tracts/Sales_with_Census_Tract.csv", header = TRUE, sep = ",")

# I select only the columns that I need

str_prox_var <- str_prox_var %>% select(NTAName, ADJ_PRICE, LAND_SQUAR, GROSS_SQUA, AGE_PROPER, SCH_DIST, WT_DIST, CBD_DIST)

# Renaming the column 'NTAName'

names(str_prox_var)[names(str_prox_var) == "NTAName" ] <- "NTANAME"

# Checking the match by the column 'NTANAME'

anti_join(str_prox_var, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = 'NTANAME')

# Merging. I will lose one row (obs.) of the 152 observations of 'agg_mean' due to the one NA in the NTANAME column

variables <- merge(str_prox_var, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = "NTANAME")

# Selecting the variables that I need

variables <- variables %>% select(ADJ_PRICE, LAND_SQUAR, GROSS_SQUA, AGE_PROPER, SCH_DIST, WT_DIST, CBD_DIST, 
                                  mean_ndvi, S, H, avg_med_inc, avg_ue_rt, pct_bk_ppl, pv_rt, pop_den_nta, 
                                  avg_tt, crime_rt_nta)

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
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_NTA.csv", 
            row.names = F, sep = ",")

# ---------------------------------------------------------------------------------------------------------------

# Now to obtain a different dataset with more aggragated data I calculate the aggregated mean of the variables

# I need to aggregate the mean of ADJ_PRICE, LAND_SQUAR, GROSS_SQUA, AGE_PROPER, SCH_DIST, WT_DIST, CBD_DIST on the NTA level
# To this end, I have to first group by NTANAME

var_by_nta <- group_by(str_prox_var, NTANAME)

# Now I can calculated the aggregate mean on the NTA level

agg_mean <- var_by_nta %>% summarize(avg_price = mean(ADJ_PRICE), 
                                     avg_land_square_feet = mean(LAND_SQUAR), 
                                     avg_gross_square_feet = mean(GROSS_SQUA), 
                                     avg_age = mean(AGE_PROPER), 
                                     avg_sch_dist = mean(SCH_DIST), 
                                     avg_wt_dist = mean(WT_DIST), 
                                     avg_cbd_dist = mean(CBD_DIST))

# Checking the match by the column 'NTANAME'

anti_join(agg_mean, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = 'NTANAME')

# Merging. I will lose one row (obs.) of the 152 observations of 'agg_mean' due to the one NA in the NTANAME column

variables1 <- merge(agg_mean, crime_tt_den_pv_bk_ue_inc_S_H_NDVI, by = "NTANAME")

# Exporting the .csv file with the final variables for later regression analysis

write.table(variables1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_Agg_NTA.csv", 
            row.names = F, sep = ",")


