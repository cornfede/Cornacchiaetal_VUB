# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Poverty_Status_Past_12_Months_Tract_2019")

# Loading useful packages

install.packages("splitstackshape")
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)

# Importing data on 2019 poverty status data per census tract in Bronx, Kings (Brooklyn), Queens, and Richmond (Staten Island)

poverty_status_tract <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Poverty_Status_Past_12_Months_Tract_2019/All_Counties/Poverty_Status_Tract.csv", header = TRUE, sep = ",")

# Considering that I am interested in the poverty rate, I need to select the variable of estimated total pop. per census tract and the variable of 
# estimated total pop. below poverty level per census tract
# these two columns are respectively S1701_C01_001E e S1701_C02_001E

tot_pop_tot_pv <- poverty_status_tract %>% select(NAME, S1701_C01_001E, S1701_C02_001E)

# For later calculations, I need to remove the first row of the tot_pop_tot_bk dataframe that do not contain useful data

tot_pop_tot_pv <- tot_pop_tot_pv[-1, ]

# Now, to be able to merge the produced dataframe with another dataframe that contains also NTA names, I need to extract only the number of the census tract from the NAME column

tot_pop_tot_pv$CTLABEL <- parse_number(tot_pop_tot_pv$NAME)

# Now I need to create a new column containing the names of the boroughs (or counties)
# To do this, I divide the first column 'NAME' in different columns based on the existing commas in the column

tot_pop_tot_pv_1 <- cSplit(tot_pop_tot_pv, "NAME", sep=",")

# I select only the columns that I need

tot_pop_tot_pv_1 <- tot_pop_tot_pv_1 %>% select(NAME_2, CTLABEL, S1701_C01_001E, S1701_C02_001E)

# Now I create a new column called 'BORONAME' containing only the names of the boroughs (counties) that I need for later steps

tot_pop_tot_pv_1$BORONAME[tot_pop_tot_pv_1$NAME_2 == "Bronx County"] <- "Bronx"
tot_pop_tot_pv_1$BORONAME[tot_pop_tot_pv_1$NAME_2 == "Kings County"] <- "Brooklyn"
tot_pop_tot_pv_1$BORONAME[tot_pop_tot_pv_1$NAME_2 == "Queens County"] <- "Queens"
tot_pop_tot_pv_1$BORONAME[tot_pop_tot_pv_1$NAME_2 == "Richmond County"] <- "Staten Island"

# I extract the column that I need

tot_pop_tot_pv_1 <- tot_pop_tot_pv_1 %>% select(CTLABEL, S1701_C01_001E, S1701_C02_001E, BORONAME)

# I check how many census tracts there are per County (339, 761, 669, 110 rows respectively)

nrow(subset(tot_pop_tot_pv_1, BORONAME == "Bronx"))
nrow(subset(tot_pop_tot_pv_1, BORONAME == "Brooklyn"))
nrow(subset(tot_pop_tot_pv_1, BORONAME == "Queens"))
nrow(subset(tot_pop_tot_pv_1, BORONAME == "Staten Island"))

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

tot_pop_tot_pv_1 <- unite(tot_pop_tot_pv_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# renaming columns

colnames(tot_pop_tot_pv_1)[colnames(tot_pop_tot_pv_1) == "S1701_C01_001E"] <- "tot_pop"
colnames(tot_pop_tot_pv_1)[colnames(tot_pop_tot_pv_1) == "S1701_C02_001E"] <- "tot_pv"

# I convert the population columns into numeric columns

tot_pop_tot_pv_1$tot_pop = as.numeric(tot_pop_tot_pv_1$tot_pop)
tot_pop_tot_pv_1$tot_pv = as.numeric(tot_pop_tot_pv_1$tot_pv)

# Now I import the table where I have census tract numbers associated with the corresponding NTA

ct_nta <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Census_Tracts_&_Corresponding_NTA/ct_&_nta.csv", header = TRUE, sep = ",")

# I select only the columns that I need

ct_nta <- ct_nta %>% select(CTLABEL, BORONAME, NTANAME)

# I need to remove the census tract located in Manhattan (New York county) considering that I do not have single-family transaction data there

ct_nta_filt <- ct_nta[!(ct_nta$BORONAME == "Manhattan"), ]

# I check how many census tracts there are per County (339, 760, 668, 110 rows respectively)

nrow(subset(ct_nta_filt, BORONAME == "Bronx"))
nrow(subset(ct_nta_filt, BORONAME == "Brooklyn"))
nrow(subset(ct_nta_filt, BORONAME == "Queens"))
nrow(subset(ct_nta_filt, BORONAME == "Staten Island"))

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

ct_nta_filt <- unite(ct_nta_filt, CT_BOR, CTLABEL, BORONAME, sep = "-")

# Now I should merge the two dataframes by the 'CT_BOR' column, but they are of different lenght
# So I check what are the rows in "tot_pop_tot_pv_1" that are not in "ct_nta_filt" using the dplyr function 'anti_join'

anti_join(tot_pop_tot_pv_1, ct_nta_filt, by = 'CT_BOR')

# I remove the two census tract in 'tot_pop_tot_pv_1' that are not present in the other dataframe

tot_pop_tot_pv_1 <- tot_pop_tot_pv_1[-c(1100, 1769), ]

# Now I merge the two dataframes by the column 'CT_BOR' 

pop_pv_nta <- merge(tot_pop_tot_pv_1, ct_nta_filt, by = "CT_BOR")

# Now I can calculate the total population per NTA and the total population below poverty level per NTA

group_by_nta <- group_by(pop_pv_nta, NTANAME)
group_by_nta$tot_pop = as.numeric(group_by_nta$tot_pop)
group_by_nta$tot_pv = as.numeric(group_by_nta$tot_pv)
nta_tot_pv <- group_by_nta %>% summarise(tot_pop = sum(tot_pop), 
                                      tot_pv = sum(tot_pv))

# Now I calculate the poverty rate per NTA

nta_tot_pv$pv_rt <- (nta_tot_pv$tot_pv) / (nta_tot_pv$tot_pop)

# I select only the variables that I need

nta_tot_pv <- nta_tot_pv %>% select(NTANAME, pv_rt)

# Exporting .csv file with poverty rate per NTA

write.table(nta_tot_pv, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Poverty_Status_Past_12_Months_Tract_2019/Calculated_Poverty_Rate_per_NTA/nta_pv_rt.csv", 
            row.names = F, sep = ",")
