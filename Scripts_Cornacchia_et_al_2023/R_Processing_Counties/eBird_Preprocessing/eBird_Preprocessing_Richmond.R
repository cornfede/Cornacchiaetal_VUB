# Set working directory

setwd("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties")

# Importing ebd observations for Bronx

Richmond <- read.delim("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/eBird_Observations_Textfiles/Observations_Richmond_County.txt", quote="", stringsAsFactors=TRUE)

# For R you always have to attach your data frame or your data after you imported it, otherwise Rstudio won't recognize
# the data you just imported, so you need to do attach(name of the data frame)

attach(Richmond)

# Install and load packages dplyr (tools for working with data frame), vegan (tools for descriptive community ecology), sp
# (to analyze spatial data)

install.packages("dplyr")
library(dplyr)

# Use the 'select' function of dplyr, followed by the %>% 'then' operator (aka, infix), to select certain columns and make the data frame more readable

df_richmond <- Richmond %>% select(GLOBAL.UNIQUE.IDENTIFIER, COMMON.NAME, SCIENTIFIC.NAME, OBSERVATION.COUNT, COUNTY, LOCALITY, LOCALITY.TYPE, LATITUDE, LONGITUDE, OBSERVATION.DATE, PROTOCOL.TYPE, DURATION.MINUTES, EFFORT.DISTANCE.KM, NUMBER.OBSERVERS, ALL.SPECIES.REPORTED)

# Select detections only (removing all non-detections from the data)

det_only <- df_richmond[df_richmond$OBSERVATION.COUNT != "X", ]

# Now some data processing steps to deal with biases often associated with citizen science data

# Select only complete checklists (addressing species bias)

complete_checklists <- det_only[det_only$ALL.SPECIES.REPORTED == "1", ]

# I need to remove checklists (observation events) with a "eBird Pelagic Protocol" type because in this analysis I am going to consider census tracts and neighbourhood tabulation areas clipped to shorelines

protocol_checklists <- complete_checklists[complete_checklists$PROTOCOL.TYPE != "eBird Pelagic Protocol", ]
summary(protocol_checklists$PROTOCOL.TYPE)

# Renaming columns (otherwise it gives problems in creating shapefiles in later steps)

summary(protocol_checklists)
names(protocol_checklists)[names(protocol_checklists) == "GLOBAL.UNIQUE.IDENTIFIER" ] <- "gui"
names(protocol_checklists)[names(protocol_checklists) == "COMMON.NAME" ] <- "com_name"
names(protocol_checklists)[names(protocol_checklists) == "SCIENTIFIC.NAME" ] <- "sci_name"
names(protocol_checklists)[names(protocol_checklists) == "OBSERVATION.COUNT" ] <- "obs_count"
names(protocol_checklists)[names(protocol_checklists) == "COUNTY" ] <- "county"
names(protocol_checklists)[names(protocol_checklists) == "LOCALITY" ] <- "locality"
names(protocol_checklists)[names(protocol_checklists) == "LOCALITY.TYPE" ] <- "loc_type"
names(protocol_checklists)[names(protocol_checklists) == "LATITUDE" ] <- "lat"
names(protocol_checklists)[names(protocol_checklists) == "LONGITUDE" ] <- "lon"
names(protocol_checklists)[names(protocol_checklists) == "OBSERVATION.DATE" ] <- "obs_date"
names(protocol_checklists)[names(protocol_checklists) == "PROTOCOL.TYPE" ] <- "prot_type"
names(protocol_checklists)[names(protocol_checklists) == "DURATION.MINUTES" ] <- "dur_min"
names(protocol_checklists)[names(protocol_checklists) == "EFFORT.DISTANCE.KM" ] <- "eff_dist_km"
names(protocol_checklists)[names(protocol_checklists) == "NUMBER.OBSERVERS" ] <- "observers"
names(protocol_checklists)[names(protocol_checklists) == "ALL.SPECIES.REPORTED" ] <- "all_sp_rep"

# Turn all the NA into zeros

protocol_checklists$eff_dist_km[which(is.na(protocol_checklists$eff_dist_km))] <- 0.03

# Now I want to create 2 different datasets, one without filtering (eBird_obs_nofilt) and the other (eBird_obs_filt) produced by filtering to obtain a more standardized range of effort within the data

# For the first dataset I can already produce the .csv file

# Exporting the filtered data frame as .csv file

write.table(protocol_checklists, 
            file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Counties_csv/Richmond_nofilt.csv", 
            row.names = F, sep = ",")

# Check if the produced .csv file is correct

options(digits = 9)
Richmond_nofilt <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/R_Processing_Counties/Counties_csv/Richmond_nofilt.csv", header = TRUE, sep = ",")
