# Loading packages

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

# Calculating specific summary statistics (mean and standard deviation)

mean(var1$price, na.rm = TRUE)
mean(var1$land_square_feet, na.rm = TRUE)
mean(var1$gross_square_feet, na.rm = TRUE)
mean(var1$age_property, na.rm = TRUE)
mean(var1$sch_dist, na.rm = TRUE)
mean(var1$wt_dist, na.rm = TRUE)
mean(var1$cbd_dist, na.rm = TRUE)
mean(var1$mean_ndvi, na.rm = TRUE)
mean(var1$S, na.rm = TRUE)
mean(var1$H, na.rm = TRUE)
mean(var1$hh_med_inc, na.rm = TRUE)
mean(var1$ue_rt, na.rm = TRUE)
mean(var1$pct_bk_ppl, na.rm = TRUE)
mean(var1$pv_rt, na.rm = TRUE)
mean(var1$pop_den, na.rm = TRUE)
mean(var1$avg_tt, na.rm = TRUE)
mean(var1$crime_rt, na.rm = TRUE)
sd(var1$price, na.rm = TRUE)
sd(var1$land_square_feet, na.rm = TRUE)
sd(var1$gross_square_feet, na.rm = TRUE)
sd(var1$age_property, na.rm = TRUE)
sd(var1$sch_dist, na.rm = TRUE)
sd(var1$wt_dist, na.rm = TRUE)
sd(var1$cbd_dist, na.rm = TRUE)
sd(var1$mean_ndvi, na.rm = TRUE)
sd(var1$S, na.rm = TRUE)
sd(var1$H, na.rm = TRUE)
sd(var1$hh_med_inc, na.rm = TRUE)
sd(var1$ue_rt, na.rm = TRUE)
sd(var1$pct_bk_ppl, na.rm = TRUE)
sd(var1$pv_rt, na.rm = TRUE)
sd(var1$pop_den, na.rm = TRUE)
sd(var1$avg_tt, na.rm = TRUE)
sd(var1$crime_rt, na.rm = TRUE)

# Producing scatterplots

plot(var1$land_square_feet, var1$price, main = "Land Square Feet - Price", las = 1)
plot(var1$gross_square_feet, var1$price, main = "Gross Square Feet - Price", las = 1)
plot(var1$age_property, var1$price, main = "Age Property - Price", las = 1)
plot(var1$sch_dist, var1$price, main = "School Proximity - Price", las = 1)
plot(var1$wt_dist, var1$price, main = "Water Body Proximity - Price", las = 1)
plot(var1$cbd_dist, var1$price, main = "Central Business District Proximity - Price", las = 1)
plot(var1$mean_ndvi, var1$price, main = "Mean NDVI - Price", las = 1)
plot(var1$S, var1$price, main = "S - Price", las = 1)
plot(var1$H, var1$price, main = "H - Price", las = 1)
plot(var1$hh_med_inc, var1$price, main = "Household Median Income - Price", las = 1)
plot(var1$ue_rt, var1$price, main = "Unemployment Rate - Price", las = 1)
plot(var1$pct_bk_ppl, var1$price, main = "Percentage Black People - Price", las = 1)
plot(var1$pv_rt, var1$price, main = "Poverty Rate - Price", las = 1)
plot(var1$pop_den, var1$price, main = "Population Density - Price", las = 1)
plot(var1$avg_tt, var1$price, main = "Average Commuting Time - Price", las = 1)
plot(var1$crime_rt, var1$price, main = "Crime Rate - Price", las = 1)

# Running Pearson Correlation Coefficients (relationship of each ind. var. with the dep. var.)

cor.test(var1$price, var1$land_square_feet)
cor.test(var1$price, var1$gross_square_feet)
cor.test(var1$price, var1$age_property)
cor.test(var1$price, var1$sch_dist)
cor.test(var1$price, var1$wt_dist)
cor.test(var1$price, var1$cbd_dist)
cor.test(var1$price, var1$mean_ndvi)
cor.test(var1$price, var1$S)
cor.test(var1$price, var1$H)
cor.test(var1$price, var1$hh_med_inc)
cor.test(var1$price, var1$ue_rt)
cor.test(var1$price, var1$pct_bk_ppl)
cor.test(var1$price, var1$pv_rt)
cor.test(var1$price, var1$pop_den)
cor.test(var1$price, var1$avg_tt)
cor.test(var1$price, var1$crime_rt)

# Obtaining a correlation coefficient matrix (to assess the relationship between the ind. var. with each other)
# There seems to be no significant multicollinearity between variables except for the correlation coefficient between poverty rate and household median income with an absolute value of 0.86

cor(var1, use = "complete.obs")

# In the H column, I have numerous NAs that I transform in zeros

var1$H[which(is.na(var1$H))] <- 0 

# Important to keep in mind that regression cannot deal with missing values
# Therefore I need to remove missing values from the dataset

sum(is.na(var1))
var1_noNA <- na.omit(var1)
summary(var1_noNA)

# Another quick visualization inspection of the variables (pairwise correlation matrix)

pairs.panels(var1_noNA, col = "red")

# Split dataset into "training" (80%) and "validation" (20%)
# In this way, later on there can be a cross-validation type of testing of the model

set.seed(2021)
train_size <- 0.8
train_index <- sample.int(length(var1_noNA$price), round(length(var1_noNA$price) * train_size))
train_sample <- var1_noNA[train_index,]
valid_sample <- var1_noNA[-train_index,]

# Producing scatterplots and correlation coefficients considering different functional forms

# Land Square Feet

plot(train_sample$land_square_feet, train_sample$price, main = "Land Square Feet - Price", las = 1)
plot(train_sample$land_square_feet, log(train_sample$price), main = "Land Square Feet - Log Price", las = 1)
plot(log(train_sample$land_square_feet), log(train_sample$price), main = "Log Land Square Feet - Log Price", las = 1)
plot((train_sample$land_square_feet + I(train_sample$land_square_feet^2)), log(train_sample$price), main = "Quadratic Land Square Feet - Log Price", las = 1)
plot((train_sample$land_square_feet + log(train_sample$land_square_feet)), log(train_sample$price), main = " LSqrFeet + log(LSqrFeet) - Log Price")

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$land_square_feet)
cor.test(log(train_sample$price), train_sample$land_square_feet)
cor.test(log(train_sample$price), log(train_sample$land_square_feet))
cor.test(log(train_sample$price), (train_sample$land_square_feet + I(train_sample$land_square_feet^2)))
cor.test(log(train_sample$price), (train_sample$land_square_feet + log(train_sample$land_square_feet)))

# Gross Square Feet

plot(train_sample$gross_square_feet, train_sample$price, main = "Gross Square Feet - Price", las = 1)
plot(train_sample$gross_square_feet, log(train_sample$price), main = "Gross Square Feet - Log Price", las = 1)
plot(log(train_sample$gross_square_feet), log(train_sample$price), main = "Log Gross Square Feet - Log Price", las = 1)
plot((train_sample$gross_square_feet + I(train_sample$gross_square_feet^2)), log(train_sample$price), main = "Quadratic Gross Square Feet - Log Price", las = 1)
plot((train_sample$gross_square_feet + log(train_sample$gross_square_feet)), log(train_sample$price), main = " GSqrFeet + log(GSqrFeet) - Log Price")

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$gross_square_feet)
cor.test(log(train_sample$price), train_sample$gross_square_feet)
cor.test(log(train_sample$price), log(train_sample$gross_square_feet))
cor.test(log(train_sample$price), (train_sample$gross_square_feet + I(train_sample$gross_square_feet^2)))
cor.test(log(train_sample$price), (train_sample$gross_square_feet + log(train_sample$gross_square_feet)))

# Age Property

plot(train_sample$age_property, train_sample$price, main = "Age Property - Price", las = 1)
plot(train_sample$age_property, log(train_sample$price), main = "Age Property - Log Price", las = 1)
plot((train_sample$age_property + I(train_sample$age_property^2)), log(train_sample$price), main = "Quadratic Age Property - Log Price", las = 1)
plot((train_sample$age_property + sqrt(train_sample$age_property)), log(train_sample$price), main = " SQRTAge + log(SQRTAge) - Log Price")

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$age_property)
cor.test(log(train_sample$price), train_sample$age_property)
cor.test(log(train_sample$price), (train_sample$age_property + I(train_sample$age_property^2)))
cor.test(log(train_sample$price), (train_sample$age_property + sqrt(train_sample$age_property)))

# School Proximity

plot(train_sample$sch_dist, train_sample$price, main = "School Proximity - Price", las = 1)
plot(train_sample$sch_dist, log(train_sample$price), main = "School Proximity - Log Price", las = 1)
plot(log(train_sample$sch_dist), log(train_sample$price), main = "Log School Proximity - Log Price", las = 1)
plot(sqrt(train_sample$sch_dist), log(train_sample$price), main = "SQRT School Proximity - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$sch_dist)
cor.test(log(train_sample$price), train_sample$sch_dist)
cor.test(log(train_sample$price), log(train_sample$sch_dist))
cor.test(log(train_sample$price), sqrt(train_sample$sch_dist))

# Water Body Proximity

plot(train_sample$wt_dist, train_sample$price, main = "Water Body Proximity - Price", las = 1)
plot(train_sample$wt_dist, log(train_sample$price), main = "Water Body Proximity - Log Price", las = 1)
plot(log(train_sample$wt_dist), log(train_sample$price), main = "Log Water Body Proximity - Log Price", las = 1)
plot(sqrt(train_sample$wt_dist), log(train_sample$price), main = "SQRT Water Body Proximity - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$wt_dist)
cor.test(log(train_sample$price), train_sample$wt_dist)
cor.test(log(train_sample$price), log(train_sample$wt_dist+1))
cor.test(log(train_sample$price), sqrt(train_sample$wt_dist))

# Central Business District Proximity

plot(train_sample$cbd_dist, train_sample$price, main = "Central Business District Proximity - Price", las = 1)
plot(train_sample$cbd_dist, log(train_sample$price), main = "Central Business District Proximity - Log Price", las = 1)
plot(log(train_sample$cbd_dist), log(train_sample$price), main = "Log Central Business District Proximity - Log Price", las = 1)
plot(sqrt(train_sample$cbd_dist), log(train_sample$price), main = "SQRT Central Business District Proximity - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$cbd_dist)
cor.test(log(train_sample$price), train_sample$cbd_dist)
cor.test(log(train_sample$price), log(train_sample$cbd_dist))
cor.test(log(train_sample$price), sqrt(train_sample$cbd_dist))

# Mean NDVI per CT

plot(train_sample$mean_ndvi, train_sample$price, main = "Mean NDVI - Price", las = 1)
plot(train_sample$mean_ndvi, log(train_sample$price), main = "Mean NDVI - Log Price", las = 1)
plot(log(train_sample$mean_ndvi), log(train_sample$price), main = "Log Mean NDVI - Log Price", las = 1)
plot(sqrt(train_sample$mean_ndvi), log(train_sample$price), main = "SQRT Mean NDVI - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$mean_ndvi)
cor.test(log(train_sample$price), train_sample$mean_ndvi)
cor.test(log(train_sample$price), log(train_sample$mean_ndvi))
cor.test(log(train_sample$price), sqrt(train_sample$mean_ndvi))

# Species Richness (S) per CT

plot(train_sample$S, train_sample$price, main = "S - Price", las = 1)
plot(train_sample$S, log(train_sample$price), main = "S - Log Price", las = 1)
plot(log(train_sample$S), log(train_sample$price), main = "Log S - Log Price", las = 1)
plot(sqrt(train_sample$S), log(train_sample$price), main = "SQRT S - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$S)
cor.test(log(train_sample$price), train_sample$S)
cor.test(log(train_sample$price), log(train_sample$S))
cor.test(log(train_sample$price), sqrt(train_sample$S))

# Species Diversity (H) per CT

plot(train_sample$H, train_sample$price, main = "H - Price", las = 1)
plot(train_sample$H, log(train_sample$price), main = "H - Log Price", las = 1)
plot(log(train_sample$H), log(train_sample$price), main = "Log H - Log Price", las = 1)
plot(sqrt(train_sample$H), log(train_sample$price), main = "SQRT H - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$H)
cor.test(log(train_sample$price), train_sample$H)
cor.test(log(train_sample$price), log(train_sample$H+1))
cor.test(log(train_sample$price), sqrt(train_sample$H))

# Household Median Income per CT

plot(train_sample$hh_med_inc, train_sample$price, main = "Household Median Income - Price", las = 1)
plot(train_sample$hh_med_inc, log(train_sample$price), main = "Household Median Income - Log Price", las = 1)
plot(log(train_sample$hh_med_inc), log(train_sample$price), main = "Log Household Median Income - Log Price", las = 1)
plot(sqrt(train_sample$hh_med_inc), log(train_sample$price), main = "SQRT Household Median Income - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$hh_med_inc)
cor.test(log(train_sample$price), train_sample$hh_med_inc)
cor.test(log(train_sample$price), log(train_sample$hh_med_inc))
cor.test(log(train_sample$price), sqrt(train_sample$hh_med_inc))

# Unemployment Rate per CT

plot(train_sample$ue_rt, train_sample$price, main = "Unemployment Rate - Price", las = 1)
plot(train_sample$ue_rt, log(train_sample$price), main = "Unemployment Rate - Log Price", las = 1)
plot(log(train_sample$ue_rt), log(train_sample$price), main = "Log Unemployment Rate - Log Price", las = 1)
plot(sqrt(train_sample$ue_rt), log(train_sample$price), main = "SQRT Unemployment Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$ue_rt)
cor.test(log(train_sample$price), train_sample$ue_rt)
cor.test(log(train_sample$price), log(train_sample$ue_rt))
cor.test(log(train_sample$price), sqrt(train_sample$ue_rt))

# Percentage Black People per CT

plot(train_sample$pct_bk_ppl, train_sample$price, main = "Percentage Black People - Price", las = 1)
plot(train_sample$pct_bk_ppl, log(train_sample$price), main = "Percentage Black People - Log Price", las = 1)
plot(log(train_sample$pct_bk_ppl), log(train_sample$price), main = "Log Percentage Black People - Log Price", las = 1)
plot(sqrt(train_sample$pct_bk_ppl), log(train_sample$price), main = "SQRT Percentage Black People - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$pct_bk_ppl)
cor.test(log(train_sample$price), train_sample$pct_bk_ppl)
cor.test(log(train_sample$price), log(train_sample$pct_bk_ppl))
cor.test(log(train_sample$price), sqrt(train_sample$pct_bk_ppl))

# Poverty Rate per CT

plot(train_sample$pv_rt, train_sample$price, main = "Poverty Rate - Price", las = 1)
plot(train_sample$pv_rt, log(train_sample$price), main = "Poverty Rate - Log Price", las = 1)
plot(log(train_sample$pv_rt), log(train_sample$price), main = "Log Poverty Rate - Log Price", las = 1)
plot(sqrt(train_sample$pv_rt), log(train_sample$price), main = "SQRT Poverty Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$pv_rt)
cor.test(log(train_sample$price), train_sample$pv_rt)
cor.test(log(train_sample$price), log(train_sample$pv_rt))
cor.test(log(train_sample$price), sqrt(train_sample$pv_rt))

# Population Density per CT

plot(train_sample$pop_den, train_sample$price, main = "Population Density - Price", las = 1)
plot(train_sample$pop_den, log(train_sample$price), main = "Population Density - Log Price", las = 1)
plot(log(train_sample$pop_den), log(train_sample$price), main = "Log Population Density - Log Price", las = 1)
plot(sqrt(train_sample$pop_den), log(train_sample$price), main = "SQRT Population Density - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$pop_den)
cor.test(log(train_sample$price), train_sample$pop_den)
cor.test(log(train_sample$price), log(train_sample$pop_den))
cor.test(log(train_sample$price), sqrt(train_sample$pop_den))

# Mean Travel Time to work (min) per CT

plot(train_sample$avg_tt, train_sample$price, main = "Average Commuting Time - Price", las = 1)
plot(train_sample$avg_tt, log(train_sample$price), main = "Average Commuting Time - Log Price", las = 1)
plot(log(train_sample$avg_tt), log(train_sample$price), main = "Log Average Commuting Time - Log Price", las = 1)
plot(sqrt(train_sample$avg_tt), log(train_sample$price), main = "SQRT Average Commuting Time - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$avg_tt)
cor.test(log(train_sample$price), train_sample$avg_tt)
cor.test(log(train_sample$price), log(train_sample$avg_tt))
cor.test(log(train_sample$price), sqrt(train_sample$avg_tt))

# Crime Rate per CT

plot(train_sample$crime_rt, train_sample$price, main = "Crime Rate - Price", las = 1)
plot(train_sample$crime_rt, log(train_sample$price), main = "Crime Rate - Log Price", las = 1)
plot(log(train_sample$crime_rt), log(train_sample$price), main = "Log Crime Rate - Log Price", las = 1)
plot(sqrt(train_sample$crime_rt), log(train_sample$price), main = "SQRT Crime Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$crime_rt)
cor.test(log(train_sample$price), train_sample$crime_rt)
cor.test(log(train_sample$price), log(train_sample$crime_rt))
cor.test(log(train_sample$price), sqrt(train_sample$crime_rt))

# Correlation matrix

cor(train_sample)
pairs.panels(train_sample, lm = TRUE, col = "red")

# I also test the normality of each variable looking at the density plot

par(mfrow = c(1, 1))
plot(density(train_sample$price))
plot(density(train_sample$land_square_feet))
plot(density(train_sample$gross_square_feet))
plot(density(train_sample$age_property))
plot(density(train_sample$sch_dist))
plot(density(train_sample$wt_dist))
plot(density(train_sample$cbd_dist))
plot(density(train_sample$mean_ndvi))
plot(density(train_sample$S))
plot(density(train_sample$H))
plot(density(train_sample$hh_med_inc))
plot(density(train_sample$ue_rt))
plot(density(train_sample$pct_bk_ppl))
plot(density(train_sample$pv_rt))
plot(density(train_sample$pop_den))
plot(density(train_sample$avg_tt))
plot(density(train_sample$crime_rt))

plotNormalHistogram(train_sample$price)
plotNormalHistogram(log(train_sample$price))
plotNormalHistogram(sqrt(train_sample$price))
qqnorm(train_sample$price)
qqline(train_sample$price)
qqnorm(log(train_sample$price))
qqline(log(train_sample$price))
qqnorm(sqrt(train_sample$price))
qqline(sqrt(train_sample$price))

plotNormalHistogram(train_sample$land_square_feet)
plotNormalHistogram(train_sample$land_square_feet^2)
plotNormalHistogram(train_sample$land_square_feet + I(train_sample$land_square_feet^2))
plotNormalHistogram(log(train_sample$land_square_feet))
plotNormalHistogram(train_sample$land_square_feet + log(train_sample$land_square_feet))
plotNormalHistogram(sqrt(train_sample$land_square_feet))
qqnorm(train_sample$land_square_feet)
qqline(train_sample$land_square_feet)
qqnorm(train_sample$land_square_feet^2)
qqline(train_sample$land_square_feet^2)
qqnorm(train_sample$land_square_feet + I(train_sample$land_square_feet^2))
qqline(train_sample$land_square_feet + I(train_sample$land_square_feet^2))
qqnorm(log(train_sample$land_square_feet))
qqline(log(train_sample$land_square_feet))
qqnorm(train_sample$land_square_feet + log(train_sample$land_square_feet))
qqline(train_sample$land_square_feet + log(train_sample$land_square_feet))
qqnorm(sqrt(train_sample$land_square_feet))
qqline(sqrt(train_sample$land_square_feet))

plotNormalHistogram(train_sample$gross_square_feet)
plotNormalHistogram(train_sample$gross_square_feet^2)
plotNormalHistogram(train_sample$gross_square_feet + I(train_sample$gross_square_feet^2))
plotNormalHistogram(log(train_sample$gross_square_feet))
plotNormalHistogram(train_sample$gross_square_feet + log(train_sample$gross_square_feet))
plotNormalHistogram(sqrt(train_sample$gross_square_feet))
qqnorm(train_sample$gross_square_feet)
qqline(train_sample$gross_square_feet)
qqnorm(train_sample$gross_square_feet^2)
qqline(train_sample$gross_square_feet^2)
qqnorm(train_sample$gross_square_feet + I(train_sample$gross_square_feet^2))
qqline(train_sample$gross_square_feet + I(train_sample$gross_square_feet^2))
qqnorm(log(train_sample$gross_square_feet))
qqline(log(train_sample$gross_square_feet))
qqnorm(train_sample$gross_square_feet + log(train_sample$gross_square_feet))
qqline(train_sample$gross_square_feet + log(train_sample$gross_square_feet))
qqnorm(sqrt(train_sample$gross_square_feet))
qqline(sqrt(train_sample$gross_square_feet))

plotNormalHistogram(train_sample$age_property)
plotNormalHistogram(train_sample$age_property^2)
plotNormalHistogram(train_sample$age_property + I(train_sample$age_property^2))
plotNormalHistogram(log(train_sample$age_property))
plotNormalHistogram(train_sample$age_property + log(train_sample$age_property))
plotNormalHistogram(sqrt(train_sample$age_property))
plotNormalHistogram(train_sample$age_property + sqrt(train_sample$age_property))
qqnorm(train_sample$age_property)
qqline(train_sample$age_property)
qqnorm(train_sample$age_property^2)
qqline(train_sample$age_property^2)
qqnorm(train_sample$age_property + I(train_sample$age_property^2))
qqline(train_sample$age_property + I(train_sample$age_property^2))
qqnorm(log(train_sample$age_property))
qqline(log(train_sample$age_property))
qqnorm(train_sample$age_property + log(train_sample$age_property))
qqline(train_sample$age_property + log(train_sample$age_property))
qqnorm(sqrt(train_sample$age_property))
qqline(sqrt(train_sample$age_property))
qqnorm(train_sample$age_property + sqrt(train_sample$age_property))
qqline(train_sample$age_property + sqrt(train_sample$age_property))

plotNormalHistogram(train_sample$sch_dist)
plotNormalHistogram(log(train_sample$sch_dist))
plotNormalHistogram(sqrt(train_sample$sch_dist))
qqnorm(train_sample$sch_dist)
qqline(train_sample$sch_dist)
qqnorm(log(train_sample$sch_dist))
qqline(log(train_sample$sch_dist))
qqnorm(sqrt(train_sample$sch_dist))
qqline(sqrt(train_sample$sch_dist))


plotNormalHistogram(train_sample$wt_dist)
plotNormalHistogram(log(train_sample$wt_dist + 1))
plotNormalHistogram(sqrt(train_sample$wt_dist))
qqnorm(train_sample$wt_dist)
qqline(train_sample$wt_dist)
qqnorm(log(train_sample$wt_dist + 1))
qqline(log(train_sample$wt_dist + 1))
qqnorm(sqrt(train_sample$wt_dist))
qqline(sqrt(train_sample$wt_dist))


plotNormalHistogram(train_sample$cbd_dist)
plotNormalHistogram(log(train_sample$cbd_dist))
plotNormalHistogram(sqrt(train_sample$cbd_dist))
qqnorm(train_sample$cbd_dist)
qqline(train_sample$cbd_dist)
qqnorm(log(train_sample$cbd_dist))
qqline(log(train_sample$cbd_dist))
qqnorm(sqrt(train_sample$cbd_dist))
qqline(sqrt(train_sample$cbd_dist))

plotNormalHistogram(train_sample$mean_ndvi)
plotNormalHistogram(log(train_sample$mean_ndvi))
plotNormalHistogram(sqrt(train_sample$mean_ndvi))
qqnorm(train_sample$mean_ndvi)
qqline(train_sample$mean_ndvi)
qqnorm(log(train_sample$mean_ndvi))
qqline(log(train_sample$mean_ndvi))
qqnorm(sqrt(train_sample$mean_ndvi))
qqline(sqrt(train_sample$mean_ndvi))

plotNormalHistogram(train_sample$S)
plotNormalHistogram(log(train_sample$S))
plotNormalHistogram(sqrt(train_sample$S))
qqnorm(train_sample$S)
qqline(train_sample$S)
qqnorm(log(train_sample$S))
qqline(log(train_sample$S))
qqnorm(sqrt(train_sample$S))
qqline(sqrt(train_sample$S))

plotNormalHistogram(train_sample$H)
plotNormalHistogram(log(train_sample$H + 1))
plotNormalHistogram(sqrt(train_sample$H))
qqnorm(train_sample$H)
qqline(train_sample$H)
qqnorm(log(train_sample$H + 1))
qqline(log(train_sample$H + 1))
qqnorm(sqrt(train_sample$H))
qqline(sqrt(train_sample$H))

plotNormalHistogram(train_sample$hh_med_inc)
plotNormalHistogram(log(train_sample$hh_med_inc))
plotNormalHistogram(sqrt(train_sample$hh_med_inc))
qqnorm(train_sample$hh_med_inc)
qqline(train_sample$hh_med_inc)
qqnorm(log(train_sample$hh_med_inc))
qqline(log(train_sample$hh_med_inc))
qqnorm(sqrt(train_sample$hh_med_inc))
qqline(sqrt(train_sample$hh_med_inc))

plotNormalHistogram(train_sample$ue_rt)
plotNormalHistogram(log(train_sample$ue_rt + 1))
plotNormalHistogram(sqrt(train_sample$ue_rt))
qqnorm(train_sample$ue_rt)
qqline(train_sample$ue_rt)
qqnorm(log(train_sample$ue_rt + 1))
qqline(log(train_sample$ue_rt))
qqnorm(sqrt(train_sample$ue_rt))
qqline(sqrt(train_sample$ue_rt))

plotNormalHistogram(train_sample$pct_bk_ppl)
plotNormalHistogram(train_sample$pct_bk_ppl^2)
plotNormalHistogram(log(train_sample$pct_bk_ppl + 1))
plotNormalHistogram(sqrt(train_sample$pct_bk_ppl))
qqnorm(train_sample$pct_bk_ppl)
qqline(train_sample$pct_bk_ppl)
qqnorm(train_sample$pct_bk_ppl^2)
qqline(train_sample$pct_bk_ppl^2)
qqnorm(log(train_sample$pct_bk_ppl + 1))
qqline(log(train_sample$pct_bk_ppl))
qqnorm(sqrt(train_sample$pct_bk_ppl))
qqline(sqrt(train_sample$pct_bk_ppl))

plotNormalHistogram(train_sample$pv_rt)
plotNormalHistogram(log(train_sample$pv_rt))
plotNormalHistogram(sqrt(train_sample$pv_rt))
qqnorm(train_sample$pv_rt)
qqline(train_sample$pv_rt)
qqnorm(log(train_sample$pv_rt))
qqline(log(train_sample$pv_rt))
qqnorm(sqrt(train_sample$pv_rt))
qqline(sqrt(train_sample$pv_rt))

plotNormalHistogram(train_sample$pop_den)
plotNormalHistogram(log(train_sample$pop_den))
plotNormalHistogram(sqrt(train_sample$pop_den))
qqnorm(train_sample$pop_den)
qqline(train_sample$pop_den)
qqnorm(log(train_sample$pop_den))
qqline(log(train_sample$pop_den))
qqnorm(sqrt(train_sample$pop_den))
qqline(sqrt(train_sample$pop_den))

plotNormalHistogram(train_sample$avg_tt)
plotNormalHistogram(log(train_sample$avg_tt))
plotNormalHistogram(sqrt(train_sample$avg_tt))
qqnorm(train_sample$avg_tt)
qqline(train_sample$avg_tt)
qqnorm(log(train_sample$avg_tt))
qqline(log(train_sample$avg_tt))
qqnorm(sqrt(train_sample$avg_tt))
qqline(sqrt(train_sample$avg_tt))

plotNormalHistogram(train_sample$crime_rt)
plotNormalHistogram(log(train_sample$crime_rt))
plotNormalHistogram(sqrt(train_sample$crime_rt))
qqnorm(train_sample$crime_rt)
qqline(train_sample$crime_rt)
qqnorm(log(train_sample$crime_rt))
qqline(log(train_sample$crime_rt))
qqnorm(sqrt(train_sample$crime_rt))
qqline(sqrt(train_sample$crime_rt))

# Checking data

summary(train_sample)

# Scientific notation - turn it off with options(scipen = 999) and back on again with options(scipen = 0) 

options(scipen = 999)

# -----------------------------------------------------------------------

# Stepwise regression (combining forward and backward selection)

# First I create an intercept-only model

FitStart <- lm(price ~ 1, data = train_sample)
summary(FitStart)

# Now I create a linear model fitted using all the predictor variables

FitAll <- lm(price ~ ., data = train_sample)
summary(FitAll)

# Now I can proceed with the stepwise regression
# It starts from the intercept-only model

step(FitStart, direction = "both", scope = formula(FitAll))

# Produced model

Model1 <- lm(formula = price ~ gross_square_feet + cbd_dist + pct_bk_ppl + 
               land_square_feet + crime_rt + pv_rt + pop_den + wt_dist + 
               ue_rt + sch_dist + avg_tt + hh_med_inc + H, data = train_sample)
summary(Model1)

# ------------------------------------------------------

# Trying to introduce log(price) in the backward selection
# Create a linear model fitted using all the predictor variables

FitAll_logprice <- lm(log(price) ~ ., data = train_sample)
summary(FitAll_logprice)

# backward selection

step(FitAll_logprice, direction = "backward")

# Resulting model from the previous backward selection

Model2 <- lm(formula = log(price) ~ land_square_feet + gross_square_feet + 
               age_property + sch_dist + wt_dist + cbd_dist + mean_ndvi + 
               S + hh_med_inc + ue_rt + pct_bk_ppl + pv_rt + pop_den + 
               avg_tt + crime_rt, data = train_sample)
summary(Model2)

# ----------------------------------------------------

# Building a multiple linear regression changing some functional forms of the predictors (as suggested in the literature and trying to apply the natural log to mean ndvi, S and H because in the previous models these predictors without the log form were discarded by the backward selection or they resulted not significant)

FitAll_2 <- lm(log(price) ~ land_square_feet + I(land_square_feet^2) + gross_square_feet + I(gross_square_feet^2) + 
                 age_property + I(age_property^2) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + hh_med_inc + ue_rt + pct_bk_ppl + pv_rt + pop_den + 
                 avg_tt + crime_rt, data = train_sample)

# Backward selection

step(FitAll_2, direction = "backward")

# The best model is the follow

Model3 <- lm(formula = log(price) ~ land_square_feet + I(land_square_feet^2) + 
               gross_square_feet + I(gross_square_feet^2) + I(age_property^2) + 
               log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
               hh_med_inc + ue_rt + pct_bk_ppl + pv_rt + pop_den + 
               avg_tt + crime_rt, data = train_sample)
summary(Model3)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model3)

# --------------------------------------------------

# Now I am going to produce a model changing some functional forms

FitAll_3 <- lm(log(price) ~ land_square_feet + log(land_square_feet) + gross_square_feet + log(gross_square_feet) + 
                 age_property + sqrt(age_property) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + hh_med_inc + ue_rt + pct_bk_ppl + pv_rt + 
                 pop_den + avg_tt + crime_rt, data = train_sample)

# Backward selection

step(FitAll_3, direction = "backward")

# Produced model

Model4 <- lm(formula = log(price) ~ log(land_square_feet) + gross_square_feet + 
               log(gross_square_feet) + age_property + sqrt(age_property) + 
               log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
               hh_med_inc + ue_rt + pct_bk_ppl + pv_rt + pop_den + 
               avg_tt + crime_rt, data = train_sample)
summary(Model4)

# --------------------------------------------------------

# Trying improvements of Model4 (discarding hh_med_inc because highly correlated with pv_rt) and changing some functional forms based on the previous variables plots (and normal histograms)

# I tried different options and discarded avg_tt_ct because not significant

FitAll_4 <- lm(formula = log(price) ~ log(land_square_feet) + gross_square_feet + 
                 log(gross_square_feet) + age_property + sqrt(age_property) + 
                 sqrt(sch_dist) + sqrt(wt_dist+1) + cbd_dist + log(mean_ndvi) + sqrt(S) +
                 sqrt(ue_rt) + log(pct_bk_ppl+1) + log(pv_rt) + log(pop_den) + 
                 log(crime_rt), data = train_sample)

# Backward selection

step(FitAll_4, direction = "backward")

# Produced model

Model5 <- lm(formula = log(price) ~ log(land_square_feet) + gross_square_feet + 
               log(gross_square_feet) + age_property + sqrt(age_property) + 
               sqrt(sch_dist) + sqrt(wt_dist + 1) + cbd_dist + log(mean_ndvi) + 
               sqrt(S) + sqrt(ue_rt) + log(pct_bk_ppl + 1) + log(pv_rt) + 
               log(pop_den) + log(crime_rt), data = train_sample)
summary(Model5)

# ----------------------------------------------------

# log-log linear model (where NDVI, S and H are not significant)

FitAll_5 <- lm(log(price) ~ log(land_square_feet) + log(gross_square_feet) + 
                 log(age_property) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + log(hh_med_inc) + log(ue_rt+1) + 
                 log(pct_bk_ppl+1) + log(pv_rt) + log(pop_den) + log(avg_tt) + 
                 log(crime_rt), data = train_sample)

# Backward

step(FitAll_5, direction = "backward")

# Produced model

Model6 <- lm(formula = log(price) ~ log(land_square_feet) + log(gross_square_feet) + 
               log(age_property) + log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + 
               log(mean_ndvi) + log(S) + log(H + 1) + log(med_inc) + 
               log(ue_rt + 1) + log(pct_bk_ppl + 1) + log(pv_rt) + log(pop_den) + 
               log(avg_tt) + log(crime_rt), data = train_sample)

summary(Model6)

# ------------------------------------------------------------------------------------------------

# Now I evaluate the final linear models
# Find all predicted values for both training set and validation set

train_sample$pred_price_M3 <- predict(Model3, newdata = subset(train_sample, 
                                                               select = c(price, land_square_feet, 
                                                                          gross_square_feet, age_property, 
                                                                          sch_dist, wt_dist, cbd_dist, mean_ndvi, 
                                                                          hh_med_inc, ue_rt, pct_bk_ppl, pv_rt, 
                                                                          pop_den, avg_tt, crime_rt)))
valid_sample$pred_price_M3 <- predict(Model3, newdata = subset(valid_sample, 
                                                               select = c(price, land_square_feet, 
                                                                          gross_square_feet, age_property, 
                                                                          sch_dist, wt_dist, cbd_dist, mean_ndvi, 
                                                                          hh_med_inc, ue_rt, pct_bk_ppl, pv_rt, 
                                                                          pop_den, avg_tt, crime_rt)))
train_sample$pred_price_M5 <- predict(Model5, newdata = subset(train_sample, 
                                                               select = c(price, land_square_feet, 
                                                                          gross_square_feet, age_property, 
                                                                          sch_dist, wt_dist, cbd_dist, mean_ndvi, 
                                                                          S, ue_rt, pct_bk_ppl, pv_rt, pop_den, 
                                                                          crime_rt)))
valid_sample$pred_price_M5 <- predict(Model5, newdata = subset(valid_sample, 
                                                               select = c(price, land_square_feet, 
                                                                          gross_square_feet, age_property, 
                                                                          sch_dist, wt_dist, cbd_dist, mean_ndvi, 
                                                                          S, ue_rt, pct_bk_ppl, pv_rt, pop_den, 
                                                                          crime_rt)))

# The theoretical model performance is defined here as R-Squared
summary(Model3)
summary(Model5)

# Let's see whether the predicted price and the real price are in any way correlated to that degree
# Ideally if I look at the training sample, the correlation between prediction and price should be more or less in the level of magnitude of the Adjusted R-squared of Model 3 and Model 5

# Check how good are the models on the training sample looking at R-squared (correlation^2), root mean square (RMSE) difference between the predicted price and the real price for the training sample, and mean absolute error (MAE) for the same two vectors

train_corr_M3 <- round(cor(train_sample$pred_price_M3, train_sample$price), 2)
train_RMSE_M3 <- round(sqrt(mean((train_sample$pred_price_M3 - train_sample$price)^2)))
train_MAE_M3 <- round(mean(abs(train_sample$pred_price_M3 - train_sample$price)))
c(train_corr_M3^2, train_RMSE_M3, train_MAE_M3)
# 0.3969 706857.0000 668471.0000

train_corr_M5 <- round(cor(train_sample$pred_price_M5, train_sample$price), 2)
train_RMSE_M5 <- round(sqrt(mean((train_sample$pred_price_M5 - train_sample$price)^2)))
train_MAE_M5 <- round(mean(abs(train_sample$pred_price_M5 - train_sample$price)))
c(train_corr_M5^2, train_RMSE_M5, train_MAE_M5)
# 0.3969 706857.0000 668471.0000

# Check how good are the models on the validation sample - correlation^2, RMSE, MAE

valid_corr_M3 <- round(cor(valid_sample$pred_price_M3, valid_sample$price), 2)
valid_RMSE_M3 <- round(sqrt(mean((valid_sample$pred_price_M3 - valid_sample$price)^2)))
valid_MAE_M3 <- round(mean(abs(valid_sample$pred_price_M3 - valid_sample$price)))
c(valid_corr_M3^2, valid_RMSE_M3, valid_MAE_M3)
# 0.3969 722545.0000 682638.0000

valid_corr_M5 <- round(cor(valid_sample$pred_price_M5, valid_sample$price), 2)
valid_RMSE_M5 <- round(sqrt(mean((valid_sample$pred_price_M5 - valid_sample$price)^2)))
valid_MAE_M5 <- round(mean(abs(valid_sample$pred_price_M5 - valid_sample$price)))
c(valid_corr_M5^2, valid_RMSE_M5, valid_MAE_M5)
# 0.3969 722545.0000 682638.0000

