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

# Now, to be able to merge the produced dataframe with another dataframe that contains also NTA names, I need to extract only the number of the census tract from the NAME column

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

# I check how many census tracts there are per County (339, 761, 669, 110 rows respectively)

nrow(subset(hh_med_ct_1, BORONAME == "Bronx"))
nrow(subset(hh_med_ct_1, BORONAME == "Brooklyn"))
nrow(subset(hh_med_ct_1, BORONAME == "Queens"))
nrow(subset(hh_med_ct_1, BORONAME == "Staten Island"))

# Now I want to combine the content of 'CTLABEL' and 'BORONAME' in one column named CT_BOR

hh_med_ct_1 <- unite(hh_med_ct_1, CT_BOR, CTLABEL, BORONAME, sep = "-")

# renaming column

colnames(hh_med_ct_1)[colnames(hh_med_ct_1) == "S1903_C03_001E"] <- "med_inc_ct"

# I convert the income column into a numeric column so that the NA values are displayed

hh_med_ct_1$med_inc_ct = as.numeric(hh_med_ct_1$med_inc_ct)

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
# So I check what are the rows in "hh_med_ct_1" that are not in "ct_nta_filt" using the dplyr function 'anti_join'

anti_join(hh_med_ct_1, ct_nta_filt, by = 'CT_BOR')

# I remove the two census tract in 'hh_med_ct_1' that are not present in the other dataframe
# These deletion won't affect the later calculation considering their NAs in the median income column

hh_med_ct_1 <- hh_med_ct_1[-c(1100, 1769), ]

# Now I merge the two dataframes by the column 'CT_BOR' 

ct_med_nta <- merge(hh_med_ct_1, ct_nta_filt, by = "CT_BOR")

# Now I can calculate the average household median income per NTA

group_by_nta <- group_by(ct_med_nta, NTANAME)
nta_avg_hh_med_inc <- summarise(group_by_nta, avg_med_inc = mean(med_inc_ct, na.rm=TRUE))

# Exporting .csv file with average household median income per NTA

write.table(nta_avg_hh_med_inc, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/ACS_Median_Income_Past_12_Months_Tract_2019/Calculated_Average_Household_Median_Income_per_NTA/nta_avg_hh_med_inc.csv", 
            row.names = F, sep = ",")
