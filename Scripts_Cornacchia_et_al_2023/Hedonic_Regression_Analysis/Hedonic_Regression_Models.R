# First level of data aggregation characterized by price, structural variables and proximity variables disaggregated at the level of the 7415 individual properties and the remaining predictors aggregated at the census tract level

# Loading packages

install.packages("stargazer")
install.packages("texreg")
library(stargazer)
library(texreg)
library(dplyr)
library(ggResidpanel)
library(readr)
library(tidyr)
library(data.table)
library(PerformanceAnalytics)
library(rcompanion)
# For eliminating missing values
library(Hmisc)
#For plotting correlation charts
library(psych)
# For a variety of different functions to do with regressions
library(car)
# For normality test Anderson Darling (that unlike the Shapiro-Wilk test can be applied on a sample size greater than 5000 observations)
library(nortest)

# Importing the 'Variables' .csv file

var1 <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_CT.csv", header = TRUE, sep = ",")

# Remove first column

var1$CT_BOR <- NULL

# Summary statistics of the considered variables

summary(var1)

# The minimum value of 'gross square feet' is zero (not possible)

var1[var1$gross_square_feet == 0, ]

# Transform the zero values into NAs

var1$gross_square_feet <- na_if(var1$gross_square_feet, 0)

# renaming columns

names(var1)[names(var1) == "mean_ndvi_ct" ] <- "mean_ndvi"

# Renaming variable (from median income to household median income)

colnames(var1)[colnames(var1) == "med_inc_ct"] <- "hh_med_inc"
colnames(var1)[colnames(var1) == "ue_rt_ct"] <- "ue_rt"
colnames(var1)[colnames(var1) == "pop_den_ct"] <- "pop_den"
colnames(var1)[colnames(var1) == "avg_tt_ct"] <- "avg_tt"
colnames(var1)[colnames(var1) == "crime_rt_ct"] <- "crime_rt"

# In the H column, I have numerous NAs that I transform in zeros

var1$H[which(is.na(var1$H))] <- 0 

# Obtaining a correlation coefficient matrix (to assess the relationship between the ind. var. with each other)

cor(var1, use = "complete.obs")

# Important to keep in mind that regression cannot deal with missing values
# Therefore I need to remove missing values from the dataset

sum(is.na(var1))
var1_noNA <- na.omit(var1)
summary(var1_noNA)

sd(var1_noNA$price)
sd(var1_noNA$land_square_feet)
sd(var1_noNA$gross_square_feet)
sd(var1_noNA$age_property)
sd(var1_noNA$sch_dist)
sd(var1_noNA$wt_dist)
sd(var1_noNA$cbd_dist)

# Split dataset into "training" (80%) and "validation" (20%)
# In this way, later on there can be a cross-validation type of testing of the model

set.seed(2021)
train_size <- 0.8
train_index <- sample.int(length(var1_noNA$price), round(length(var1_noNA$price) * train_size))
train_sample <- var1_noNA[train_index,]
valid_sample <- var1_noNA[-train_index,]

# Correlation matrix

cor(train_sample)

# Checking data

summary(train_sample)

# Scientific notation - turn it off with options(scipen = 999) and back on again with options(scipen = 0) 

options(scipen = 999)

# Building a multiple linear regression changing some functional forms of the predictors (as suggested in the literature and trying to apply the natural log to mean ndvi, S and H because in the previous models these predictors without the log form were discarded by the backward selection or they resulted not significant)

FitAll_1 <- lm(log(price) ~ land_square_feet + I(land_square_feet^2) + gross_square_feet + I(gross_square_feet^2) + 
                 age_property + I(age_property^2) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + hh_med_inc + ue_rt + pct_bk_ppl + pv_rt + pop_den + 
                 avg_tt + crime_rt, data = train_sample)

# Backward selection

step(FitAll_1, direction = "backward")

# The best model is the follow

Model1 <- lm(formula = log(price) ~ land_square_feet + I(land_square_feet^2) + 
               gross_square_feet + I(gross_square_feet^2) + I(age_property^2) + 
               log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
               hh_med_inc + ue_rt + pct_bk_ppl + pv_rt + pop_den + 
               avg_tt + crime_rt, data = train_sample)
summary(Model1)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model1)

# Trying improvements of Model4 (discarding the non significant variable hh_med_inc) and changing some functional forms based on the previous variables plots (and normal histograms)

# I tried different options and discarded avg_tt because not significant

FitAll_2 <- lm(formula = log(price) ~ log(land_square_feet) + gross_square_feet + 
                 log(gross_square_feet) + age_property + sqrt(age_property) + 
                 sqrt(sch_dist) + sqrt(wt_dist) + cbd_dist + log(mean_ndvi) + sqrt(S) +
                 sqrt(ue_rt) + log(pct_bk_ppl+1) + log(pv_rt) + log(pop_den) + 
                 log(crime_rt), data = train_sample)

# Backward selection

step(FitAll_2, direction = "backward")

# Produced model

Model2 <- lm(formula = log(price) ~ log(land_square_feet) + gross_square_feet + 
               log(gross_square_feet) + age_property + sqrt(age_property) + 
               sqrt(sch_dist) + sqrt(wt_dist) + cbd_dist + log(mean_ndvi) + 
               sqrt(S) + sqrt(ue_rt) + log(pct_bk_ppl + 1) + log(pv_rt) + 
               log(pop_den) + log(crime_rt), data = train_sample)
summary(Model2)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model2)

# ---------------------------------------------------------------------------------------------------------------

# Second level of data aggregation characterized by price, structural variables and proximity variables disaggregated at the level of the 7415 individual properties and the remaining predictors aggregated at the neighborhood tabulation area (NTA) level

# Importing the 'Variables' .csv file

var <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_NTA.csv", header = TRUE, sep = ",")

# Renaming variable (from average median income to average household income)

colnames(var)[colnames(var) == "avg_med_inc"] <- "avg_hh_inc"
colnames(var)[colnames(var) == "pop_den_nta"] <- "pop_den"
colnames(var)[colnames(var) == "crime_rt_nta"] <- "crime_rt"

# Summary statistics of the considered variables

summary(var)

# The minimum value of 'gross square feet' is zero (not possible)

var[var$gross_square_feet == 0, ]

# transform the zero values into NAs

var$gross_square_feet <- na_if(var$gross_square_feet, 0)

# Obtaining a correlation coefficient matrix (to assess the relationship between the ind. var. with each other)
# At this level of data aggregation (nta) there seems to be no significant multicollinearity between variables except for the correlation coefficient between poverty rate and average median income with an absolute value of 0.86

cor(var, use = "complete.obs")

# Important to keep in mind that regression cannot deal with missing values
# Therefore I need to remove missing values from the dataset

sum(is.na(var))
var_noNA <- na.omit(var)
summary(var_noNA)

# Split dataset into "training" sample (80%) and "validation" sample (20%)
# In this way, later on there can be a cross-validation type of testing of the model

set.seed(2021)
train_size <- 0.8
train_index_1 <- sample.int(length(var_noNA$price), round(length(var_noNA$price) * train_size))
train_sample_1 <- var_noNA[train_index_1,]
valid_sample_1 <- var_noNA[-train_index_1,]

# Correlation matrix on training sample

cor(train_sample_1)

# Building a multiple linear regression changing some functional forms of the predictors (as suggested in the literature and trying to apply the natural log to mean ndvi, S and H because in the previous models these predictors without the log form were discarded by the backward selection or they resulted not significant)
# I directly discard avg_hh_inc because it is strongly correlated with pv_rt  (Pearson coef. -0.86)

FitAll_3 <- lm(log(price) ~ land_square_feet + I(land_square_feet^2) + gross_square_feet + I(gross_square_feet^2) + 
                 age_property + I(age_property^2) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + avg_ue_rt + pct_bk_ppl + pv_rt + pop_den + 
                 avg_tt + crime_rt, data = train_sample_1)

# Backward selection

step(FitAll_3, direction = "backward")

# The best model is the follow

Model3 <- lm(formula = log(price) ~ land_square_feet + I(land_square_feet^2) + 
               gross_square_feet + I(gross_square_feet^2) + age_property + 
               log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
               log(S) + log(H + 1) + pct_bk_ppl + pv_rt + pop_den + 
               crime_rt, data = train_sample_1)
summary(Model3)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model3)

# Trying improvements of Model7 (discarding the non significant variable gross_square_feet) and changing some functional forms based on the previous variables plots (and normal histograms)

FitAll_4 <- lm(formula = log(price) ~ log(land_square_feet) + 
                 log(gross_square_feet) + age_property + sqrt(age_property) + 
                 log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
                 log(S) + log(H + 1) + log(pct_bk_ppl) + pv_rt + pop_den + 
                 crime_rt, data = train_sample_1)

# Backward selection

step(FitAll_4, direction = "backward")

# Produced model

Model4 <- lm(formula = log(price) ~ log(land_square_feet) + log(gross_square_feet) + 
               age_property + sqrt(age_property) + log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + 
               log(mean_ndvi) + log(S) + log(H + 1) + log(pct_bk_ppl) + pv_rt + pop_den + crime_rt, 
             data = train_sample_1)
summary(Model4)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model4)



# ---------------------------------------------------------------------------------------------------------------

# Third level of data aggregation characterized by the aggregated mean of price, structural variables and proximity variables at the census tract level and the remaining predictors aggregated at the census tract level as for the first level data aggregation models

# Importing the 'Variables' .csv file

var3 <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_Agg_CT.csv", header = TRUE, sep = ",")

# Remove first column

var3$CT_BOR <- NULL

# The minimum value of 'gross square feet' is zero (not possible)

var3[var3$avg_gross_square_feet == 0, ]

# Transform the zero values into NAs

var3$avg_gross_square_feet <- na_if(var3$avg_gross_square_feet, 0)

# renaming columns

names(var3)[names(var3) == "mean_ndvi_ct" ] <- "mean_ndvi"

# Renaming variable (from median income to household median income)

colnames(var3)[colnames(var3) == "med_inc_ct"] <- "hh_med_inc"
colnames(var3)[colnames(var3) == "ue_rt_ct"] <- "ue_rt"
colnames(var3)[colnames(var3) == "pop_den_ct"] <- "pop_den"
colnames(var3)[colnames(var3) == "avg_tt_ct"] <- "avg_tt"
colnames(var3)[colnames(var3) == "crime_rt_ct"] <- "crime_rt"

# Summary statistics of the considered variables

summary(var3)

# Obtaining a correlation coefficient matrix (to assess the relationship between the ind. var. with each other)
# At this level of data aggregation (ct) there seems to be no significant multicollinearity between variables

cor(var3, use = "complete.obs")

# In the H column, I have different NAs that I transform in zeros

var3$H[which(is.na(var3$H))] <- 0 

# Important to keep in mind that regression cannot deal with missing values
# Therefore I need to remove missing values from the dataset

sum(is.na(var3))
var3_noNA <- na.omit(var3)
summary(var3_noNA)

sd(var3_noNA$avg_price)
sd(var3_noNA$avg_land_square_feet)
sd(var3_noNA$avg_gross_square_feet)
sd(var3_noNA$avg_age)
sd(var3_noNA$avg_sch_dist)
sd(var3_noNA$avg_wt_dist)
sd(var3_noNA$avg_cbd_dist)
sd(var3_noNA$mean_ndvi)
sd(var3_noNA$S)
sd(var3_noNA$H)
sd(var3_noNA$hh_med_inc)
sd(var3_noNA$ue_rt)
sd(var3_noNA$pct_bk_ppl)
sd(var3_noNA$pv_rt)
sd(var3_noNA$pop_den)
sd(var3_noNA$avg_tt)
sd(var3_noNA$crime_rt)

# Split dataset into "training" sample (80%) and "validation" sample (20%)
# In this way, later on there can be a cross-validation type of testing of the model

set.seed(2021)
train_size <- 0.8
train_index_2 <- sample.int(length(var3_noNA$avg_price), round(length(var3_noNA$avg_price) * train_size))
train_sample_2 <- var3_noNA[train_index_2,]
valid_sample_2 <- var3_noNA[-train_index_2,]

# Correlation matrix on training sample

cor(train_sample_2)

# Building a multiple linear regression changing some functional forms of the predictors (as suggested in the literature and trying to apply the natural log to mean ndvi, S and H)
# I directly discard pv_rt because it is strongly correlated with avg_hh_inc

FitAll_5 <- lm(log(avg_price) ~ avg_land_square_feet + I(avg_land_square_feet^2) + 
                 avg_gross_square_feet + I(avg_gross_square_feet^2) + avg_age + I(avg_age^2) + 
                 log(avg_sch_dist) + log(avg_wt_dist+1) + log(avg_cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + hh_med_inc + ue_rt + pct_bk_ppl + pop_den + 
                 avg_tt + crime_rt, data = train_sample_2)

# Backward selection

step(FitAll_5, direction = "backward")

# The best model is the follow

Model5 <- lm(formula = log(avg_price) ~ avg_land_square_feet + avg_gross_square_feet + 
               I(avg_gross_square_feet^2) + log(avg_sch_dist) + log(avg_wt_dist + 1) + 
               log(avg_cbd_dist) + log(mean_ndvi) + log(H + 1) + hh_med_inc + 
               ue_rt + pct_bk_ppl + crime_rt, data = train_sample_2)
summary(Model5)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model5)

# Trying improvements of Model6 changing some functional forms based on the previous variables plots (and normal histograms)

FitAll_6 <- lm(log(avg_price) ~ avg_land_square_feet + I(avg_land_square_feet^2) + 
                 avg_gross_square_feet + avg_age + sqrt(avg_age) + 
                 sqrt(avg_sch_dist) + sqrt(avg_wt_dist) + sqrt(avg_cbd_dist) + 
                 mean_ndvi + log(S) + log(H+1) + log(hh_med_inc) + ue_rt + log(pct_bk_ppl+1) + pv_rt + 
                 log(pop_den) + avg_tt + log(crime_rt), data = train_sample_2)

# Backward selection

step(FitAll_6, direction = "backward")

# Produced model

Model6 <- lm(formula = log(avg_price) ~ avg_land_square_feet + avg_gross_square_feet + 
               sqrt(avg_sch_dist) + sqrt(avg_wt_dist) + sqrt(avg_cbd_dist) + 
               log(H + 1) + log(hh_med_inc) + log(pct_bk_ppl + 1) + log(pop_den), 
             data = train_sample_2)
summary(Model6)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model6)

# ---------------------------------------------------------------------------------------------------------------

# Fourth level of data aggregation characterized by the aggregated mean of price, structural variables and proximity variables at the NTA level and the remaining predictors aggregated at the NTA level as for the second level data aggregation models

# Importing the 'Variables' .csv file

var2 <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_Agg_NTA.csv", header = TRUE, sep = ",")

# Remove first column

var2$NTANAME <- NULL

# Renaming variable (from average median income to average household income)

colnames(var2)[colnames(var2) == "avg_med_inc"] <- "avg_hh_inc"
colnames(var2)[colnames(var2) == "pop_den_nta"] <- "pop_den"
colnames(var2)[colnames(var2) == "crime_rt_nta"] <- "crime_rt"

# Summary statistics of the considered variables

summary(var2)

# Obtaining a correlation coefficient matrix (to assess the relationship between the ind. var. with each other)
# At this level of data aggregation (nta) there seems to be no significant multicollinearity between variables except for the correlation coefficient between poverty rate and average median income with an absolute value of 0.86

cor(var2, use = "complete.obs")

# In the H column, I have different NAs that I transform in zeros

var2$H[which(is.na(var2$H))] <- 0 

# Important to keep in mind that regression cannot deal with missing values
# Therefore I need to remove missing values from the dataset

sum(is.na(var2))
var2_noNA <- na.omit(var2)
summary(var2_noNA)

sd(var2_noNA$avg_price)
sd(var2_noNA$avg_land_square_feet)
sd(var2_noNA$avg_gross_square_feet)
sd(var2_noNA$avg_age)
sd(var2_noNA$avg_sch_dist)
sd(var2_noNA$avg_wt_dist)
sd(var2_noNA$avg_cbd_dist)
sd(var2_noNA$mean_ndvi)
sd(var2_noNA$S)
sd(var2_noNA$H)
sd(var2_noNA$avg_hh_inc)
sd(var2_noNA$avg_ue_rt)
sd(var2_noNA$pct_bk_ppl)
sd(var2_noNA$pv_rt)
sd(var2_noNA$pop_den)
sd(var2_noNA$avg_tt)
sd(var2_noNA$crime_rt)

# Split dataset into "training" sample (80%) and "validation" sample (20%)
# In this way, later on there can be a cross-validation type of testing of the model

set.seed(2021)
train_size <- 0.8
train_index_3 <- sample.int(length(var2_noNA$avg_price), round(length(var2_noNA$avg_price) * train_size))
train_sample_3 <- var2_noNA[train_index_3,]
valid_sample_3 <- var2_noNA[-train_index_3,]

# Correlation matrix on training sample

cor(train_sample_3)

# Checking data

summary(train_sample_3)

# Building a multiple linear regression changing some functional forms of the predictors (as suggested in the literature and trying to apply the natural log to mean ndvi, S and H)
# I directly discard pv_rt because it is strongly correlated with avg_hh_inc

FitAll_7 <- lm(log(avg_price) ~ avg_land_square_feet + I(avg_land_square_feet^2) + 
                 avg_gross_square_feet + I(avg_gross_square_feet^2) + avg_age + I(avg_age^2) + 
                 log(avg_sch_dist) + log(avg_wt_dist+1) + log(avg_cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + avg_hh_inc + avg_ue_rt + pct_bk_ppl + pop_den + 
                 avg_tt + crime_rt, data = train_sample_3)

# Backward selection

step(FitAll_7, direction = "backward")

# The best model is the follow

Model7 <- lm(formula = log(avg_price) ~ avg_land_square_feet + avg_gross_square_feet + 
               I(avg_gross_square_feet^2) + log(avg_cbd_dist) + log(mean_ndvi) + 
               log(S) + avg_hh_inc + avg_ue_rt + pct_bk_ppl + pop_den, 
             data = train_sample_3)
summary(Model7)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model7)

# Trying improvements of Model6 changing some functional forms based on the previous variables plots (and normal histograms)

FitAll_8 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + avg_gross_square_feet + 
                 + I(avg_gross_square_feet^2) + avg_age + sqrt(avg_age) + avg_sch_dist + avg_wt_dist + 
                 + log(avg_cbd_dist) + log(mean_ndvi) + log(S) + H + log(avg_hh_inc) + 
                 + log(avg_ue_rt) + sqrt(pct_bk_ppl) + log(pop_den) + log(avg_tt) + 
                 + log(crime_rt), data = train_sample_3)

# Backward selection

step(FitAll_8, direction = "backward")

# Produced model

Model8 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + avg_gross_square_feet + 
               I(avg_gross_square_feet^2) + avg_sch_dist + log(avg_cbd_dist) + 
               H + log(avg_hh_inc) + sqrt(pct_bk_ppl) + log(pop_den), 
             data = train_sample_3)
summary(Model8)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model8)

# Now I want to create tables with the results to be presented in the thesis report

htmlreg(list(Model1, Model2), file = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Analysis/Models_Summary_Tables/First_Lev_Agg")

# Another way to produce summary statistics tables for the models

?stargazer

stargazer(Model1, Model3, Model5, Model7, object.names = TRUE, 
          notes.label = "Significance levels",
          type = "html", 
          out = "C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Analysis/Models_Summary_Tables/M1M3M5M7.htm")
