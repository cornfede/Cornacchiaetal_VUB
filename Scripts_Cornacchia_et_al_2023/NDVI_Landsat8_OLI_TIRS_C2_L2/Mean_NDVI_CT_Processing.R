# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/NDVI_Landsat8_OLI_TIRS_C2_L2/NYC_NDVI")

# Loading useful packages

install.packages("hablar")
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(hablar)

# Importing .csv file with NDVI data per census tract

ndvi_ct <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/NDVI_Landsat8_OLI_TIRS_C2_L2/NYC_NDVI/Mean_NDVI_per_CT.csv", header = TRUE, sep = ",")

# Selection only needed columns

ndvi_ct <- ndvi_ct %>% select(CT_BOR.C.254, MEAN.N.19.11)

# Renaming columns

colnames(ndvi_ct)[colnames(ndvi_ct) == "CT_BOR.C.254"] <- "CT_BOR"
colnames(ndvi_ct)[colnames(ndvi_ct) == "MEAN.N.19.11"] <- "mean_ndvi_ct"

# Discarding rows relative to manhattan

ndvi_ct_1 <- ndvi_ct %>% filter(str_detect(CT_BOR, "Manhattan", negate = TRUE))

# Convert column

str(ndvi_ct_1$mean_ndvi_ct)
ndvi_ct_1$mean_ndvi_ct = as.factor(ndvi_ct_1$mean_ndvi_ct)
ndvi_ct_1$mean_ndvi_ct <- gsub(",",".",ndvi_ct_1$mean_ndvi_ct)
ndvi_ct_1$mean_ndvi_ct = as.numeric(ndvi_ct_1$mean_ndvi_ct)

# Save as .csv file

write.table(ndvi_ct_1, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/NDVI_Landsat8_OLI_TIRS_C2_L2/NYC_NDVI/Mean_NDVI_per_Census_Tract.csv", 
            row.names = F, sep = ",")
