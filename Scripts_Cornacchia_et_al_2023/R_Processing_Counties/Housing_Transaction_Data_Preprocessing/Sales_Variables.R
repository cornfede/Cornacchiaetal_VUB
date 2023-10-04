setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties")

library(dplyr)
library(sf)
library(ggplot2)
library(vegan)
library(lubridate)

sales <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Housing_Transaction_Data_Preprocessing/Geocoded_Transactions.csv", 
                        options = c("X_POSSIBLE_NAMES=long", 
                                    "Y_POSSIBLE_NAMES=lat"), 
                        crs = 4326)

# Removing the transactions of which I do not have the coordinates

noNA_lat_long <- sales[complete.cases(sales$`lat`), ]

# I noticed that in the column 'Sales Price' there are unusable values such as 1, 10, 5000 etc.that are almost certainly not the result of market prices
# Therefore I am going to remove such unusable values via removing only the outliers less than the lower price benchmark (65000)

noNA_lat_long$SALE.PRICE = as.numeric(noNA_lat_long$SALE.PRICE)
summary(noNA_lat_long$SALE.PRICE)
bench <- 479000 - 1.5*IQR(noNA_lat_long$SALE.PRICE)

# I remove prices smaller or equal to 65000 dollars (total of 301 rows)

filtered_geocoded_sales <- filter(noNA_lat_long, SALE.PRICE > 65000)

# Now I need to make the sale prices adjusted to inflation
# deflating them will extinguish the fraction of the up-down movement in them that was a consequence of general inflationary pressure
# I adjust the sale prices to the February 2021 level, using the House Price Index (HPI) of February 2021 (2021/02/01) for New York
# The formula applied to the prices is Adj_price=sale_price*((HPI_Feb2021)/(HPI_sold_month))

# First, I want to create a new column called 'MONTH' that contains the name of the month that I am going to extract from the 'SALE DATE' column

filtered_geocoded_sales$MONTH <- lubridate::month(filtered_geocoded_sales$SALE.DATE, label = TRUE, abbr = FALSE)

# However, currently the HPI for March 2021 are not available
# Therefore, I should exclude the sales dated March 2021 for which it is not possible to apply the formula

nrow(filtered_geocoded_sales[filtered_geocoded_sales$MONTH == "March", ])
no_march_sales <- filtered_geocoded_sales[filtered_geocoded_sales$MONTH != "March", ]

# Now I need to add a column that contains the HPI corresponding to the month when the sale took place

options(digits = 15)
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "April"] <- 204.919258639868
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "May"] <- 204.949215091033
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "June"] <- 204.911101803752
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "July"] <- 205.422129517703
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "August"] <- 208.043842752408
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "September"] <- 211.164865570129
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "October"] <- 215.127705225285
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "November"] <- 219.836085562540
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "December"] <- 223.210039559688
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "January"] <- 225.968843585203
no_march_sales$HPI_MONTH[no_march_sales$MONTH == "February"] <- 228.585519457751

# Now I can add a new column 'ADJ_PRICE' containing the adjusted sale prices

no_march_sales$ADJ_PRICE <- no_march_sales$SALE.PRICE * ((228.585519457751) / (no_march_sales$HPI_MONTH))

# Now I need to transform the variable 'YEAR BUILT' into 'AGE_PROPERTY'
# To this end, I need to do the current year (2021) minus the year built

no_march_sales$YEAR.BUILT = as.numeric(no_march_sales$YEAR.BUILT)
sum(is.na(no_march_sales$YEAR.BUILT))

# Removing NAs from YEAR BUILT column (only one NA)

no_march_sales <- filter(no_march_sales, !is.na(no_march_sales$YEAR.BUILT))

# Transforming year built in age of the property

no_march_sales$AGE_PROPERTY <- 2021 - no_march_sales$YEAR.BUILT

# Reading the shapefile of the nyc NTA boundaries

nyc_nta_shp <- st_read("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Neighborhood_Tabulation_Areas_&_PUMAs/Neighborhood_Tabulation_Areas/nynta_21a/nynta.shp")
st_crs(nyc_nta_shp)

# Making the projection of the sales dataframe equal to the projection of the NTA shapefile (NAD83)

nad83_sales <- st_transform(no_march_sales, 2263)

# Plotting together the housing transaction locations and the NTA boundaries shapefile

ggplot() + 
  geom_sf(data = nyc_nta_shp) + 
  geom_sf(data = nad83_sales)

# Now I need to attribute to each row of the dataframe (transaction) the name of the NTA where it is located
# To this end I need to spatially join (spatial overlay) the nad83_sales and the nyc_nta_shp spatial dataframes

spatial_join <- st_join(nad83_sales, nyc_nta_shp, join = st_within)

# I remove the geometry column because I don't need it anymore

spatial_join <- st_set_geometry(spatial_join, NULL)

# I select only the variables (columns) that I need for later calculations

Sale_Variables <- spatial_join %>% select(BoroName, NTAName, ADJ_PRICE, LAND.SQUARE.FEET, GROSS.SQUARE.FEET, AGE_PROPERTY)

# I export the dataframe as .csv file

write.table(Sale_Variables, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Housing_Transaction_Data_Preprocessing/Sale_Variables.csv", 
            row.names = F, sep = ",")

# Check if the produced .csv file is correct

Variables <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Housing_Transaction_Data_Preprocessing/Sale_Variables.csv", header = TRUE, sep = ",")






# I closed the session after producing the .csv Sale_Variables
# Now, after opening again this script, I remove the geometry column from the nad83_sales dataframe

no_geom_sales <- st_set_geometry(nad83_sales, NULL)

# I export the dataframe no_geom_sales as .csv file for later spatial analysis in ArcGIS

write.table(no_geom_sales, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Housing_Transaction_Data_Preprocessing/Geocoded_Filtered_Sales_Shapefile/Sales.csv", 
            row.names = F, sep = ",")

