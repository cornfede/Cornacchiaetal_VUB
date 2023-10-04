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

# For later calculations, I need to remove the first row of the dataframe that do not contain useful data

avg_tt_ct <- avg_tt_ct[-1, ]

# Now, to be able to merge the produced dataframe with another dataframe that contains also NTA names, I need to extract only the number of the census tract from the NAME column

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

# I check how many census tracts there are per County (339, 761, 669, 110 rows respectively)

nrow(subset(avg_tt_ct_1, BORONAME == "Bronx"))
nrow(subset(avg_tt_ct_1, BORONAME == "Brooklyn"))
nrow(subset(avg_tt_ct_1, BORONAME == "Queens"))
nrow(subset(avg_tt_ct_1, BORONAME == "Staten Island"))

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

avg_tt_ct_1 <- unite(avg_tt_ct_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# renaming column

colnames(avg_tt_ct_1)[colnames(avg_tt_ct_1) == "S0801_C01_046E"] <- "avg_tt"

# I convert the travel column into a numeric column so that the NA values are displayed

avg_tt_ct_1$avg_tt = as.numeric(avg_tt_ct_1$avg_tt)

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
# So I check what are the rows in "avg_tt_ct_1" that are not in "ct_nta_filt" using the dplyr function 'anti_join'

anti_join(avg_tt_ct_1, ct_nta_filt, by = 'CT_BOR')

# I remove the two census tract in 'avg_tt_ct_1' that are not present in the other dataframe
# These deletion won't affect the later calculation considering their NAs in the median income column

avg_tt_ct_1 <- avg_tt_ct_1[-c(1100, 1769), ]

# Now I merge the two dataframes by the column 'CT_BOR' 

ct_tt_nta <- merge(avg_tt_ct_1, ct_nta_filt, by = "CT_BOR")

# Now I can calculate the average travel time to work per NTA

group_by_nta <- group_by(ct_tt_nta, NTANAME)
nta_avg_tt <- summarise(group_by_nta, avg_tt = mean(avg_tt, na.rm=TRUE))

# Exporting .csv file 

write.table(nta_avg_tt, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Commuting_Characteristics_by_Sex_Tract_2019/Calculated_Mean_Travel_Time_to_Work_per_NTA/avg_tt_nta.csv", 
            row.names = F, sep = ",")
