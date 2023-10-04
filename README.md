# Cornacchiaetal_VUB
Scripts for analysis in Cornacchia et al. - 'Valuing Urban Biodiversity using Citizen Science Data: A Hedonic Price Model Approach'

GENERAL INFORMATION

== Author Information ==

	A. Corresponding author
		Name: Federico Cornacchia
		Institution: Department of Environmental Sciences, Informatics and Statistics, 
                Ca' Foscari University of Venice, via Torino 155, 30172 Venice, Italy; 
                Fondazione Eni Enrico Mattei, Climate Change Adaptation Group (ADAPT@VE), 
                Palazzo Ca’ Tron, Santa Croce 1957, 30135 Venice, Italy
		Email: federico.cornacchia@unive.it

	B. Author
		Name: Koen de Koning
		Institution: Wageningen University, Wageningen University and Research, 
                Environmental Systems Analysis Group, P.O. box 47, 6700 AA Wageningen, 
                The Netherlands, ORCID 0000-0002-2586-0184
		Email: koen.dekoning@wur.nl
        
  C. Author
		Name: Okmyung Bin
		Institution: Department of Economics, East Carolina University, Greenville, 
                NC 27858, United States 
                Email: bino@ecu.edu


== Keywords used to describe the data topic == 

Hedonic price; Biodiversity; Citizen science; Economic valuation; Ecosystem services; Urban;

== List of abbreviations ==

CT = census tract
NTA = Neighborhood Tabulation Area
ACS = American Community Survey

== DATA & FILE OVERVIEW (Folders, files and description of their content) ==

Organization of the presentation of folders and sub-folders (or files) in this readme file:

1. folder
     1.1. sub-folder (or file/s)
          1.1.1. sub-folder of the sub-folder (or file/s)
               etc.

Scripts_Cornacchia_et_al_2023 (MAIN FOLDER)

----------------------------------------------------------------------------------------------
1. 'ACS_Commuting_Characteristics_by_Sex_Tract_2019'

     1.1. 'CT_Average_Travel_Time' --> R script (.R) containing the calculation of the mean travel time to work per CT

     1.2. 'NTA_Average_Travel_Time' --> R script (.R) containing the calculation of the mean travel time to work per NTA
  
----------------------------------------------------------------------------------------------
2. 'ACS_Employment_Status_Tract_2019'

     2.1. 'CT_Unemployment_Rate' --> R script (.R) containing the calculation of the unemployment rate per CT

     2.2. 'NTA_Average_Unemployment_Rate' --> R script (.R) containing the calculation of the average unemployment rate per NTA
   
----------------------------------------------------------------------------------------------
3. 'ACS_Median_Income_Past_12_Months_Tract_2019'

     3.1. 'CT_Average_Household_Median_Income' --> R script (.R) containing the calculation of the household median income per CT

     3.2. 'NTA_Average_Household_Median_Income' --> R script (.R) containing the calculation of the average household median income per NTA
  
----------------------------------------------------------------------------------------------
4. 'ACS_Poverty_Status_Past_12_Months_Tract_2019'

     4.1. 'CT_Poverty_Rate' --> R script (.R) containing the calculation of the poverty rate per CT

     4.2. 'NTA_Poverty_Rate' --> R script (.R) containing the calculation of the poverty rate per NTA

----------------------------------------------------------------------------------------------
5. 'ACS_Race_&_Ethnicity_Tract_2019'

     5.1. 'CT_Percentage_Black_or_AfricanAmerican_People' --> R script (.R) containing the calculation of the percentage of black or African-American people per CT

     5.2. 'NTA_Percentage_Black_or_AfricanAmerican_People'--> R script (.R) containing the calculation of the percentage of black or African-American people per NTA

----------------------------------------------------------------------------------------------
6. ACS_Total_Population_Tract_2019

     6.1. 'CT_Pop_Density' --> R script (.R) containing the calculation of the population density per CT

     6.2. 'NTA_Pop_Density' --> R script (.R) containing the calculation of the population density per NTA

---------------------------------------------------------------------------------------------
7. 'Crime_Data'

     7.1. 'NYC_Crime_Data' --> R script (.R) containing the steps to acquire the 2019 NYC crime data from the Crime Open Database and the spatial analysis to obtain the crime rate per CT and the crime rate per NTA

----------------------------------------------------------------------------------------------
8. 'Hedonic_Regression_Analysis'

     8.1. 'Hedonic_Regression_Analysis_CT' --> R script (.R) containing the hedonic regression models construction steps with part of the variables disaggregated at the individual property level and the other part of the variables aggregated at the CT level (including 
           testing different functional forms of the variables, testing for normality and linearity, and testing the performance of verious hedonic regression models)
           From this script the best two models were selected in terms of model permormance (adjusted R^2 and other criteria such as distributions of residuals): M3 and M5

     8.2. 'Hedonic_Regression_Analysis_CT_Agg' --> R script (.R) containing the hedonic regression models construction steps with all the variables aggregated at the level of CTs (including testing different functional forms of the variables, testing for normality and 
           linearity, and testing the performance of verious hedonic regression models)
           From this script the best two models were selected in terms of model permormance (adjusted R^2 and other criteria such as distributions of residuals): M4 and M7

     8.3. 'Hedonic_Regression_Analysis_NTA' --> R script (.R) containing the hedonic regression models construction steps with part of the variables disaggregated at the individual property level and the other part of the variables aggregated at the NTA level 
          (including testing different functional forms of the variables, testing for normality and linearity, and testing the performance of verious hedonic regression models)
           From this script the best two models were selected in terms of model permormance (adjusted R^2 and other criteria such as distributions of residuals): M5 and M8

     8.4. 'Hedonic_Regression_Analysis_NTA_Agg' --> R script (.R) containing the hedonic regression models construction steps with all the variables aggregated at the level of NTAs (including testing different functional forms of the variables, testing for normality 
           and linearity, and testing the performance of verious hedonic regression models)
           From this script the best two models were selected in terms of model permormance (adjusted R^2 and other criteria such as distributions of residuals): M4 and M7

     8.5. 'Hedonic_Regression_Models' --> R script (.R) containing the best 8 models selected from the different data aggregation levels (2 per level of aggregation). Of the eight models contained in this script, only four were selected and presented in the article. 
           M1 (= Model 1 in the article), M3 (= Model 2 in the article), M5 (= Model 3 in the article), M7 (= Model 4 in the article). 

----------------------------------------------------------------------------------------------
9. 'Hedonic_Regression_Variables'

     9.1. 'Regression_Variables_Census_Tract_Level' --> R script (.R) in which all the variables calculated separately at the level of the CTs and the structural variables, the accessibility variables and the dependent variable price were imported and combined in a 
           single dataset exported in .csv format (Variables_CT.csv). Furthermore, in the same script, the aggregate average at the CT level of the price, structural and accessibility variables was calculated so that a single dataset containing all the aggregate 
           variables at the level of the CTs could also be exported (Variables_Agg_CT.csv).

     9.2. 'Regression_Variables_NTA_Level' --> R script (.R) in which all the variables calculated separately at the level of the NTAs and the structural variables, the accessibility variables and the dependent variable price were imported and combined in a single 
           dataset exported in .csv format (Variables_NTA.csv). Furthermore, in the same script, the aggregate average at the NTA level of the price, structural and accessibility variables was calculated so that a single dataset containing all the aggregate variables 
           at the level of the NTAs could also be exported (Variables_Agg_NTA.csv).
 
----------------------------------------------------------------------------------------------
10. 'Housing_Transactions_NYC_Counties'

     10.1. 'Address_Adjusted_Sales' --> R script (.R) in which I added and rearranged the columns in the different datasets relating to the rolling sales data in order to report the complete address of each property in a unique column of the dataset (useful for batch 
            geocoding at a later stage).
     
----------------------------------------------------------------------------------------------
11. 'Map'

     11.1. 'Study_Area_Map' --> R script (.R) used to produced the visualisation of the study area

----------------------------------------------------------------------------------------------
12. 'NDVI_Landsat8_OLI_TIRS_C2_L2'

     12.1. 'Mean_NDVI_CT_Processing' --> R script (.R) containing the data processing of the produced NDVI data ('Mean_NDVI_per_CT.csv'), then exported as 'Mean_NDVI_per_Census_Tract.csv'
   
----------------------------------------------------------------------------------------------
13. 'R_Processing_Counties'
    
     13.1. 'eBird_Counties_Spatial_Analysis' 
          13.1.1. 'Bronx_eBird_Spatial_Analysis' --> R script (.R) in which, for the Bronx County, the Species Richness Index (S) and Shannon-Wiener Diversity Index (H) were calculate both per CT and per NTA and then exported to folder 23.9 and folder 23.10
          13.1.2. 'Kings_eBird_Spatial_Analysis' --> R script (.R) in which, for Kings County, the Species Richness Index (S) and Shannon-Wiener Diversity Index (H) were calculate both per CT and per NTA and then exported to folder 23.9 and folder 23.10
          13.1.3. 'NewYork_eBird_Spatial_Analysis' --> R script (.R) in which, for New York County, the Species Richness Index (S) and Shannon-Wiener Diversity Index (H) were calculate both per CT and per NTA and then exported to folder 23.9 and folder 23.10
          13.1.4. 'Queens_eBird_Spatial_Analysis' --> R script (.R) in which, for Queens County, the Species Richness Index (S) and Shannon-Wiener Diversity Index (H) were calculate both per CT and per NTA and then exported to folder 23.9 and folder 23.10
          13.1.5. 'Richmond_eBird_Spatial_Analysis' --> R script (.R) in which, for Richmond County, the Species Richness Index (S) and Shannon-Wiener Diversity Index (H) were calculate both per CT and per NTA and then exported to folder 23.9 and folder 23.10
 
     13.2. 'eBird_Preprocessing'
          13.2.1. 'eBird_Preprocessing_Bronx' --> R script (.R) in which the eBird original data about biodiversity observations in the Bronx County were subjected to pre-processing 
          13.2.2. 'eBird_Preprocessing_Kings' --> R script (.R) in which the eBird original data about biodiversity observations in Kings County were subjected to pre-processing 
          13.2.3. 'eBird_Preprocessing_NewYork'--> R script (.R) in which the eBird original data about biodiversity observations in New York County were subjected to pre-processing 
          13.2.4. 'eBird_Preprocessing_Queens' --> R script (.R) in which the eBird original data about biodiversity observations in Queens County were subjected to pre-processing 
          13.2.5. 'eBird_Preprocessing_Richmond' --> R script (.R) in which the eBird original data about biodiversity observations in Richmond County were subjected to pre-processing 

     13.3. 'Housing_Transaction_Data_Preprocessing'
                         
          13.3.1. 'NYC_Housing_Transactions_Processed' --> R script (.R) in which the first phase of pre-processing of the NYC rolling sales data was performed, including the batch geocoding process using the tidygeocoder R package.
          13.3.2. 'Sales_Variables'--> R script (.R) in which the second phase of pre-processing of the NYC rolling sales data was performed (initially importing in this script the .csv file 23.6.3.
  
     13.4. 'Species_Richness_and_Diversity_per_Census_Tract_per_County' 
          
          13.4.1. 'Biodiversity_Spatial_Distribution_CT' --> R script (.R) in which the visualisations of the eBird observation events throughout the considered NYC counties (presented in the thesis report) and the choropleth maps showing the Species Richness Index 
                   (S) and Shannon-Wiener Diversity Index (H) per CT (presented in the thesis report) were produced.

     13.5. 'Species_Richness_and_Diversity_per_NTA_per_County'
         
          13.5.1. 'Biodiversity_Spatial_Distribution_NTA' --> R script (.R) in which the visualisations of the eBird observation events throughout the considered NYC counties (presented in the thesis report) and the choropleth maps showing the Species Richness Index 
                   (S) and Shannon-Wiener Diversity Index (H) per NTA (presented in the thesis report) were produced.
