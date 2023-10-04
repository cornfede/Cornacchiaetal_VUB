# Reading Bronx housing transaction data

install.packages("writexl")
library(writexl)
library(readxl)
bronx <- read_excel("rollingsales_bronx.xls", 
                    skip = 4)

# Now I need to add columns with address information, in this way I can have complete address for the geocoding step later on
# I need to add a column called COUNTY, in this case filled with 'Bronx'

bronx$COUNTY <- "Bronx"

# I also need to add a column named COUNTRY, filled with 'United States'

bronx$COUNTRY <- "United States"

# I also add a column, named 'COMP_ZIP' containing the complete zip code, meaning with NY before the number
# But before that I add a column named 'STATE' filled with NY

bronx$STATE <- "NY"

# So I can now create a new column named 'COMP_ZIP' equal to the union of the 2 columns of ZIP CODE and STATE
# Define variables for merge

my_cols <- c("STATE", "ZIP CODE")

# Apply do.call & paste

bronx$COMP_ZIP <- do.call(paste, c(bronx[my_cols], sep = "\t"))

# I also have to create a column that contains all the address info combined

my_cols_1 <- c("ADDRESS", "COUNTY", "COMP_ZIP", "COUNTRY")

# Apply do.call & paste

bronx$COMP_ADD <- do.call(paste, c(bronx[my_cols_1], sep = ", "))

# Now I save the produced dataframe as .xlsx file

write_xlsx(bronx, "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Housing_Transactions_NYC_Counties/Address_Adjusted_Sales/Bronx.xlsx")

#-------------------------------------------------------------------------------------------------------------------------------

# Reading Brooklyn housing transaction data

brooklyn <- read_excel("rollingsales_brooklyn.xls", 
                    skip = 4)

# I need to add a column called COUNTY, in this case filled with 'Brooklyn'

brooklyn$COUNTY <- "Brooklyn"

# I also need to add a column named COUNTRY, filled with 'United States'

brooklyn$COUNTRY <- "United States"

# I also add a column, named 'COMP_ZIP' containing the complete zip code, meaning with NY before the number
# But before that I add a column named 'STATE' filled with NY

brooklyn$STATE <- "NY"

# So I can now create a new column named 'COMP_ZIP' equal to the union of the 2 columns of ZIP CODE and STATE
# Define variables for merge

my_cols <- c("STATE", "ZIP CODE")

# Apply do.call & paste

brooklyn$COMP_ZIP <- do.call(paste, c(brooklyn[my_cols], sep = "\t"))

# I also have to create a column that contains all the address info combined

my_cols_1 <- c("ADDRESS", "COUNTY", "COMP_ZIP", "COUNTRY")

# Apply do.call & paste

brooklyn$COMP_ADD <- do.call(paste, c(brooklyn[my_cols_1], sep = ", "))

# Now I save the produced dataframe as .xlsx file

write_xlsx(brooklyn, "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Housing_Transactions_NYC_Counties/Address_Adjusted_Sales/Brooklyn.xlsx")

#------------------------------------------------------------------------------------------------------------------------------

# Reading Manhattan housing transaction data
  
manhattan <- read_excel("rollingsales_manhattan.xls", 
                    skip = 4)

# Now I need to add columns with address information, in this way I can have complete address for the geocoding step later on
# I need to add a column called COUNTY, in this case filled with 'Manhattan'

manhattan$COUNTY <- "Manhattan"

# I also need to add a column named COUNTRY, filled with 'United States'

manhattan$COUNTRY <- "United States"

# I also add a column, named 'COMP_ZIP' containing the complete zip code, meaning with NY before the number
# But before that I add a column named 'STATE' filled with NY

manhattan$STATE <- "NY"

# So I can now create a new column named 'COMP_ZIP' equal to the union of the 2 columns of ZIP CODE and STATE
# Define variables for merge

my_cols <- c("STATE", "ZIP CODE")

# Apply do.call & paste

manhattan$COMP_ZIP <- do.call(paste, c(manhattan[my_cols], sep = "\t"))

# I also have to create a column that contains all the address info combined

my_cols_1 <- c("ADDRESS", "COUNTY", "COMP_ZIP", "COUNTRY")

# Apply do.call & paste

manhattan$COMP_ADD <- do.call(paste, c(manhattan[my_cols_1], sep = ", "))

# Now I save the produced dataframe as .xlsx file

write_xlsx(manhattan, "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Housing_Transactions_NYC_Counties/Address_Adjusted_Sales/Manhattan.xlsx")

#-----------------------------------------------------------------------------------------------------------------------------

# Reading Queens housing transaction data
  
queens <- read_excel("rollingsales_queens.xls", 
                    skip = 4)

# Now I need to add columns with address information, in this way I can have complete address for the geocoding step later on
# I need to add a column called COUNTY, in this case filled with 'Queens'

queens$COUNTY <- "Queens"

# I also need to add a column named COUNTRY, filled with 'United States'

queens$COUNTRY <- "United States"

# I also add a column, named 'COMP_ZIP' containing the complete zip code, meaning with NY before the number
# But before that I add a column named 'STATE' filled with NY

queens$STATE <- "NY"

# So I can now create a new column named 'COMP_ZIP' equal to the union of the 2 columns of ZIP CODE and STATE
# Define variables for merge

my_cols <- c("STATE", "ZIP CODE")

# Apply do.call & paste

queens$COMP_ZIP <- do.call(paste, c(queens[my_cols], sep = "\t"))

# I also have to create a column that contains all the address info combined

my_cols_1 <- c("ADDRESS", "COUNTY", "COMP_ZIP", "COUNTRY")

# Apply do.call & paste

queens$COMP_ADD <- do.call(paste, c(queens[my_cols_1], sep = ", "))

# Now I save the produced dataframe as .xlsx file

write_xlsx(queens, "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Housing_Transactions_NYC_Counties/Address_Adjusted_Sales/Queens.xlsx")

#----------------------------------------------------------------------------------------------------------------------------

# Reading Staten Island housing transaction data

statenisland <- read_excel("rollingsales_statenisland.xls", 
                    skip = 4)

# Now I need to add columns with address information, in this way I can have complete address for the geocoding step later on
# I need to add a column called COUNTY, in this case filled with 'Staten Island'

statenisland$COUNTY <- "Staten Island"

# I also need to add a column named COUNTRY, filled with 'United States'

statenisland$COUNTRY <- "United States"

# I also add a column, named 'COMP_ZIP' containing the complete zip code, meaning with NY before the number
# But before that I add a column named 'STATE' filled with NY

statenisland$STATE <- "NY"

# So I can now create a new column named 'COMP_ZIP' equal to the union of the 2 columns of ZIP CODE and STATE
# Define variables for merge

my_cols <- c("STATE", "ZIP CODE")

# Apply do.call & paste

statenisland$COMP_ZIP <- do.call(paste, c(statenisland[my_cols], sep = "\t"))

# I also have to create a column that contains all the address info combined

my_cols_1 <- c("ADDRESS", "COUNTY", "COMP_ZIP", "COUNTRY")

# Apply do.call & paste

statenisland$COMP_ADD <- do.call(paste, c(statenisland[my_cols_1], sep = ", "))

# Now I save the produced dataframe as .xlsx file

write_xlsx(statenisland, "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Housing_Transactions_NYC_Counties/Address_Adjusted_Sales/StatenIsland.xlsx")

