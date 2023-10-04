# Loading required packages

library(tidyr)
library(readxl)
library(dplyr)
library(writexl)

# I merge the 5 .xls housing sales files so that I can process the transaction data removing outliers and selecting only housing characteristics of interest

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Housing_Transactions_NYC_Counties/Address_Adjusted_Sales")

my_files <- list.files(pattern = "*.xlsx")
my_files

nyc_housing_sales = lapply(my_files, function(i){ 
  x = read_excel(i, sheet = 1, 
                 col_types = c("text", "text", "text", 
                               "text", "numeric", "numeric", "numeric", 
                               "text", "text", "text", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "text", "text", "numeric", "date", 
                               "text", "text", "text", "text", "text"))})

nyc_housing_sales[[1]]
nyc_housing_sales = do.call("rbind.data.frame", nyc_housing_sales)

# Select only single-family housing sales, meaning only the sales of 'one family dwellings' within the variable 'BUILDING CLASS CATEGORY'

one_family_dwellings <- nyc_housing_sales[nyc_housing_sales$`BUILDING CLASS CATEGORY` == "01 ONE FAMILY DWELLINGS", ]

# Removing sales with a transaction price equal to zero

nozero_price_sales <- one_family_dwellings[one_family_dwellings$`SALE PRICE` != "0", ]

# check the data, particularly sale price and land square feet of the houses

# I want to remove the outliers of SALE PRICE

boxplot(nozero_price_sales$`SALE PRICE`, horizontal = TRUE)
summary(nozero_price_sales$`SALE PRICE`)

# Before discarding the outliers from the dataset I create a benchmark, and if any observation falls above this benchmark I am going to delete it
# To create the benchmark I use the upper quartile that is 824837 in $

bench <- 824837 + 1.5*IQR(nozero_price_sales$`SALE PRICE`) 
bench

# So if any value of sale price is greater than 1327092.5, I am going to discard it

no_price_outliers_df <- nozero_price_sales[nozero_price_sales$`SALE PRICE` <= 1327092.5, ]
boxplot(no_price_outliers_df$`SALE PRICE`)
summary(no_price_outliers_df$`SALE PRICE`)

# Now I do a similar procedure with 'land square feet'

# To create the benchmark I use the upper quartile that is 4000

noNA_landsquare_df <- no_price_outliers_df[complete.cases(no_price_outliers_df$`LAND SQUARE FEET`), ]
summary(noNA_landsquare_df$`LAND SQUARE FEET`)
boxplot(noNA_landsquare_df$`LAND SQUARE FEET`)
bench2 <- 4000 + 1.5*IQR(noNA_landsquare_df$`LAND SQUARE FEET`)

# So if any value of land square footage is greater than 7000 square feet, I am going to discard it

filtered_sales_df <- noNA_landsquare_df[noNA_landsquare_df$`LAND SQUARE FEET` <= 7000, ]

# Now I need to geocode the addresses associated with the housing transactions in order to obtain their specific coordinates (lon, lat)

install.packages("tidygeocoder")
library(tidygeocoder)
geocode <- geocode(filtered_sales_df, address = "COMP_ADD")

# Counting the number of NAs in the latitude and longitude column (691 NAs)

sum(is.na(geocode$lat))

# Now I save the produced dataframe as .xlsx file

write.table(geocode, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Housing_Transaction_Data_Preprocessing/Geocoded_Transactions.csv", 
            row.names = F, sep = ",")
write_xlsx(filtered_sales_df, "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Housing_Transaction_Data_Preprocessing/Filtered_Sales.xlsx")



