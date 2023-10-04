# Loading packages

install.packages("Hmisc")
install.packages("psych")
install.packages("car")
install.packages("PerformanceAnalytics") 
install.packages(("nortest"))
install.packages("MASS")
install.packages("boot")
install.packages("rcompanion")
install.packages("ggResidpanel")
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

# Calculating specific summary statistics (mean and standard deviation)

mean(var$price, na.rm = TRUE)
mean(var$land_square_feet, na.rm = TRUE)
mean(var$gross_square_feet, na.rm = TRUE)
mean(var$age_property, na.rm = TRUE)
mean(var$sch_dist, na.rm = TRUE)
mean(var$wt_dist, na.rm = TRUE)
mean(var$cbd_dist, na.rm = TRUE)
mean(var$mean_ndvi, na.rm = TRUE)
mean(var$S, na.rm = TRUE)
mean(var$H, na.rm = TRUE)
mean(var$avg_hh_inc, na.rm = TRUE)
mean(var$avg_ue_rt, na.rm = TRUE)
mean(var$pct_bk_ppl, na.rm = TRUE)
mean(var$pv_rt, na.rm = TRUE)
mean(var$pop_den, na.rm = TRUE)
mean(var$avg_tt, na.rm = TRUE)
mean(var$crime_rt, na.rm = TRUE)
sd(var$price, na.rm = TRUE)
sd(var$land_square_feet, na.rm = TRUE)
sd(var$gross_square_feet, na.rm = TRUE)
sd(var$age_property, na.rm = TRUE)
sd(var$sch_dist, na.rm = TRUE)
sd(var$wt_dist, na.rm = TRUE)
sd(var$cbd_dist, na.rm = TRUE)
sd(var$mean_ndvi, na.rm = TRUE)
sd(var$S, na.rm = TRUE)
sd(var$H, na.rm = TRUE)
sd(var$avg_hh_inc, na.rm = TRUE)
sd(var$avg_ue_rt, na.rm = TRUE)
sd(var$pct_bk_ppl, na.rm = TRUE)
sd(var$pv_rt, na.rm = TRUE)
sd(var$pop_den, na.rm = TRUE)
sd(var$avg_tt, na.rm = TRUE)
sd(var$crime_rt, na.rm = TRUE)

# Producing scatterplots

plot(var$land_square_feet, var$price, main = "Land Square Feet - Price", las = 1)
plot(var$gross_square_feet, var$price, main = "Gross Square Feet - Price", las = 1)
plot(var$age_property, var$price, main = "Age Property - Price", las = 1)
plot(var$sch_dist, var$price, main = "School Proximity - Price", las = 1)
plot(var$wt_dist, var$price, main = "Water Body Proximity - Price", las = 1)
plot(var$cbd_dist, var$price, main = "Central Business District Proximity - Price", las = 1)
plot(var$mean_ndvi, var$price, main = "Mean NDVI - Price", las = 1)
plot(var$S, var$price, main = "S - Price", las = 1)
plot(var$H, var$price, main = "H - Price", las = 1)
plot(var$avg_hh_inc, var$price, main = "Average Household Income - Price", las = 1)
plot(var$avg_ue_rt, var$price, main = "Average Unemployment Rate - Price", las = 1)
plot(var$pct_bk_ppl, var$price, main = "Percentage Black People - Price", las = 1)
plot(var$pv_rt, var$price, main = "Poverty Rate - Price", las = 1)
plot(var$pop_den, var$price, main = "Population Density - Price", las = 1)
plot(var$avg_tt, var$price, main = "Average Commuting Time (min) - Price", las = 1)
plot(var$crime_rt, var$price, main = "Crime Rate - Price", las = 1)

# Running Pearson Correlation Coefficients 

cor.test(var$price, var$land_square_feet)
cor.test(var$price, var$gross_square_feet)
cor.test(var$price, var$age_property)
cor.test(var$price, var$sch_dist)
cor.test(var$price, var$wt_dist)
cor.test(var$price, var$cbd_dist)
cor.test(var$price, var$mean_ndvi)
cor.test(var$price, var$S)
cor.test(var$price, var$H)
cor.test(var$price, var$avg_hh_inc)
cor.test(var$price, var$avg_ue_rt)
cor.test(var$price, var$pct_bk_ppl)
cor.test(var$price, var$pv_rt)
cor.test(var$price, var$pop_den)
cor.test(var$price, var$avg_tt)
cor.test(var$price, var$crime_rt)

# Obtaining a correlation coefficient matrix (to assess the relationship between the ind. var. with each other)
# At this level of data aggregation (nta) there seems to be no significant multicollinearity between variables except for the correlation coefficient between poverty rate and average median income with an absolute value of 0.86

cor(var, use = "complete.obs")

# Important to keep in mind that regression cannot deal with missing values
# Therefore I need to remove missing values from the dataset

sum(is.na(var))
var_noNA <- na.omit(var)
summary(var_noNA)

# Another quick visualization inspection of the variables (pairwise correlation matrix)

pairs.panels(var_noNA, col = "red")

# Split dataset into "training" sample (80%) and "validation" sample (20%)
# In this way, later on there can be a cross-validation type of testing of the model

set.seed(2021)
train_size <- 0.8
train_index <- sample.int(length(var_noNA$price), round(length(var_noNA$price) * train_size))
train_sample <- var_noNA[train_index,]
valid_sample <- var_noNA[-train_index,]

# Producing scatterplots and correlation coefficients considering different functional forms (to try dealing with nonlinearity)

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
plot(log(train_sample$wt_dist+1), log(train_sample$price), main = "Log Water Body Proximity - Log Price", las = 1)
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

# Mean NDVI per NTA

plot(train_sample$mean_ndvi, train_sample$price, main = "Mean NDVI - Price", las = 1)
plot(train_sample$mean_ndvi, log(train_sample$price), main = "Mean NDVI - Log Price", las = 1)
plot(log(train_sample$mean_ndvi), log(train_sample$price), main = "Log Mean NDVI - Log Price", las = 1)
plot(sqrt(train_sample$mean_ndvi), log(train_sample$price), main = "SQRT Mean NDVI - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$mean_ndvi)
cor.test(log(train_sample$price), train_sample$mean_ndvi)
cor.test(log(train_sample$price), log(train_sample$mean_ndvi))
cor.test(log(train_sample$price), sqrt(train_sample$mean_ndvi))

# Species Richness (S) per NTA

plot(train_sample$S, train_sample$price, main = "S - Price", las = 1)
plot(train_sample$S, log(train_sample$price), main = "S - Log Price", las = 1)
plot(log(train_sample$S), log(train_sample$price), main = "Log S - Log Price", las = 1)
plot(sqrt(train_sample$S), log(train_sample$price), main = "SQRT S - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$S)
cor.test(log(train_sample$price), train_sample$S)
cor.test(log(train_sample$price), log(train_sample$S))
cor.test(log(train_sample$price), sqrt(train_sample$S))

# Species Diversity (H) per NTA

plot(train_sample$H, train_sample$price, main = "H - Price", las = 1)
plot(train_sample$H, log(train_sample$price), main = "H - Log Price", las = 1)
plot(log(train_sample$H), log(train_sample$price), main = "Log H - Log Price", las = 1)
plot(sqrt(train_sample$H), log(train_sample$price), main = "SQRT H - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$H)
cor.test(log(train_sample$price), train_sample$H)
cor.test(log(train_sample$price), log(train_sample$H+1))
cor.test(log(train_sample$price), sqrt(train_sample$H))

# Average Household Income per NTA

plot(train_sample$avg_hh_inc, train_sample$price, main = "Average Household Income - Price", las = 1)
plot(train_sample$avg_hh_inc, log(train_sample$price), main = "Average Household Income - Log Price", las = 1)
plot(log(train_sample$avg_hh_inc), log(train_sample$price), main = "Log Average Household Income - Log Price", las = 1)
plot(sqrt(train_sample$avg_hh_inc), log(train_sample$price), main = "SQRT Average Household Income - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$avg_hh_inc)
cor.test(log(train_sample$price), train_sample$avg_hh_inc)
cor.test(log(train_sample$price), log(train_sample$avg_hh_inc))
cor.test(log(train_sample$price), sqrt(train_sample$avg_hh_inc))

# Average Unemployment Rate per NTA

plot(train_sample$avg_ue_rt, train_sample$price, main = "Average Unemployment Rate - Price", las = 1)
plot(train_sample$avg_ue_rt, log(train_sample$price), main = "Average Unemployment Rate - Log Price", las = 1)
plot(log(train_sample$avg_ue_rt), log(train_sample$price), main = "Log Average Unemployment Rate - Log Price", las = 1)
plot(sqrt(train_sample$avg_ue_rt), log(train_sample$price), main = "SQRT Average Unemployment Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$avg_ue_rt)
cor.test(log(train_sample$price), train_sample$avg_ue_rt)
cor.test(log(train_sample$price), log(train_sample$avg_ue_rt))
cor.test(log(train_sample$price), sqrt(train_sample$avg_ue_rt))

# Percentage Black People per NTA

plot(train_sample$pct_bk_ppl, train_sample$price, main = "Percentage Black People - Price", las = 1)
plot(train_sample$pct_bk_ppl, log(train_sample$price), main = "Percentage Black People - Log Price", las = 1)
plot(log(train_sample$pct_bk_ppl), log(train_sample$price), main = "Log Percentage Black People - Log Price", las = 1)
plot(sqrt(train_sample$pct_bk_ppl), log(train_sample$price), main = "SQRT Percentage Black People - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$pct_bk_ppl)
cor.test(log(train_sample$price), train_sample$pct_bk_ppl)
cor.test(log(train_sample$price), log(train_sample$pct_bk_ppl))
cor.test(log(train_sample$price), sqrt(train_sample$pct_bk_ppl))

# Poverty Rate per NTA

plot(train_sample$pv_rt, train_sample$price, main = "Poverty Rate - Price", las = 1)
plot(train_sample$pv_rt, log(train_sample$price), main = "Poverty Rate - Log Price", las = 1)
plot(log(train_sample$pv_rt), log(train_sample$price), main = "Log Poverty Rate - Log Price", las = 1)
plot(sqrt(train_sample$pv_rt), log(train_sample$price), main = "SQRT Poverty Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$pv_rt)
cor.test(log(train_sample$price), train_sample$pv_rt)
cor.test(log(train_sample$price), log(train_sample$pv_rt))
cor.test(log(train_sample$price), sqrt(train_sample$pv_rt))

# Population Density per NTA

plot(train_sample$pop_den, train_sample$price, main = "Population Density - Price", las = 1)
plot(train_sample$pop_den, log(train_sample$price), main = "Population Density - Log Price", las = 1)
plot(log(train_sample$pop_den), log(train_sample$price), main = "Log Population Density - Log Price", las = 1)
plot(sqrt(train_sample$pop_den), log(train_sample$price), main = "SQRT Population Density - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$pop_den)
cor.test(log(train_sample$price), train_sample$pop_den)
cor.test(log(train_sample$price), log(train_sample$pop_den))
cor.test(log(train_sample$price), sqrt(train_sample$pop_den))

# Mean Travel Time to work (min) per NTA

plot(train_sample$avg_tt, train_sample$price, main = "Average Commuting Time - Price", las = 1)
plot(train_sample$avg_tt, log(train_sample$price), main = "Average Commuting Time - Log Price", las = 1)
plot(log(train_sample$avg_tt), log(train_sample$price), main = "Log Average Commuting Time - Log Price", las = 1)
plot(sqrt(train_sample$avg_tt), log(train_sample$price), main = "SQRT Average Commuting Time - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$avg_tt)
cor.test(log(train_sample$price), train_sample$avg_tt)
cor.test(log(train_sample$price), log(train_sample$avg_tt))
cor.test(log(train_sample$price), sqrt(train_sample$avg_tt))

# Crime Rate per NTA

plot(train_sample$crime_rt, train_sample$price, main = "Crime Rate - Price", las = 1)
plot(train_sample$crime_rt, log(train_sample$price), main = "Crime Rate - Log Price", las = 1)
plot(log(train_sample$crime_rt), log(train_sample$price), main = "Log Crime Rate - Log Price", las = 1)
plot(sqrt(train_sample$crime_rt), log(train_sample$price), main = "SQRT Crime Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$price, train_sample$crime_rt)
cor.test(log(train_sample$price), train_sample$crime_rt)
cor.test(log(train_sample$price), log(train_sample$crime_rt))
cor.test(log(train_sample$price), sqrt(train_sample$crime_rt))

# Correlation matrix and pairs panels on training sample

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
plot(density(train_sample$avg_hh_inc))
plot(density(train_sample$avg_ue_rt))
plot(density(train_sample$pct_bk_ppl))
plot(density(train_sample$pv_rt))
plot(density(train_sample$pop_den))
plot(density(train_sample$avg_tt))
plot(density(train_sample$crime_rt))

# Test normality by comparing data distribution to a normal distribution and also by visualizing a "theoretical", by default normal, quantile-quantile plot

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

plotNormalHistogram(train_sample$avg_hh_inc)
plotNormalHistogram(log(train_sample$avg_hh_inc))
plotNormalHistogram(sqrt(train_sample$avg_hh_inc))
qqnorm(train_sample$avg_hh_inc)
qqline(train_sample$avg_hh_inc)
qqnorm(log(train_sample$avg_hh_inc))
qqline(log(train_sample$avg_hh_inc))
qqnorm(sqrt(train_sample$avg_hh_inc))
qqline(sqrt(train_sample$avg_hh_inc))

plotNormalHistogram(train_sample$avg_ue_rt)
plotNormalHistogram(log(train_sample$avg_ue_rt))
plotNormalHistogram(sqrt(train_sample$avg_ue_rt))
qqnorm(train_sample$avg_ue_rt)
qqline(train_sample$avg_ue_rt)
qqnorm(log(train_sample$avg_ue_rt))
qqline(log(train_sample$avg_ue_rt))
qqnorm(sqrt(train_sample$avg_ue_rt))
qqline(sqrt(train_sample$avg_ue_rt))

plotNormalHistogram(train_sample$pct_bk_ppl)
plotNormalHistogram(log(train_sample$pct_bk_ppl))
plotNormalHistogram(sqrt(train_sample$pct_bk_ppl))
qqnorm(train_sample$pct_bk_ppl)
qqline(train_sample$pct_bk_ppl)
qqnorm(log(train_sample$pct_bk_ppl))
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

Model1 <- lm(formula = price ~ gross_square_feet + cbd_dist + crime_rt + 
                land_square_feet + pct_bk_ppl + pop_den + sch_dist + 
                age_property + mean_ndvi + avg_ue_rt + wt_dist + S + avg_hh_inc, 
             data = train_sample)
summary(Model1)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model1)

# Usually backward selection is preferred, so I also try to run this
# Here, differently from the previous procedure, it starts with a model fitted using all the predictors

step(FitAll, direction = "backward")

# Resulting model from the previous backward selection 

Model2 <- lm(formula = price ~ land_square_feet + gross_square_feet + age_property + 
                sch_dist + wt_dist + cbd_dist + mean_ndvi + S + avg_hh_inc + 
                avg_ue_rt + pct_bk_ppl + pop_den + crime_rt, data = train_sample)
summary(Model2)

# None of the produced models included H
# Therefore I need to check different functional forms

# ---------------------------------------------------------------

# Trying to introduce log(price) in the backward selection
# Create a linear model fitted using all the predictor variables

FitAll_logprice <- lm(log(price) ~ ., data = train_sample)
summary(FitAll_logprice)

# backward selection

step(FitAll_logprice, direction = "backward")

# Resulting model from the previous backward selection (log model)

Model3 <- lm(formula = log(price) ~ land_square_feet + gross_square_feet + 
                age_property + sch_dist + wt_dist + cbd_dist + mean_ndvi + 
                S + avg_hh_inc + avg_ue_rt + pct_bk_ppl + pv_rt + pop_den + 
                crime_rt, data = train_sample)
summary(Model3)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model3)


# -------------------------------------------------------------

# I try to produce a model with log form only of S and H

FitAll_1 <- lm(log(price) ~ land_square_feet + gross_square_feet + age_property + sch_dist + wt_dist + cbd_dist + 
                  mean_ndvi + log(S) + log(H + 1) + avg_hh_inc + avg_ue_rt + pct_bk_ppl + pv_rt + pop_den + 
                  avg_tt + crime_rt, data = train_sample)

# backward selection

step(FitAll_1, direction = "backward")

# The produce model is the follow

Model4 <- lm(formula = log(price) ~ land_square_feet + gross_square_feet + 
                age_property + sch_dist + wt_dist + cbd_dist + mean_ndvi + 
                log(S) + log(H + 1) + avg_hh_inc + avg_ue_rt + pct_bk_ppl + 
                pv_rt + pop_den + crime_rt, data = train_sample)
summary(Model4)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model4)

# If I want to have all the four plots appear together

par(mfrow = c(2, 2))

# If I want to go back visualizing one plot at a time

par(mfrow = c(1, 1))

# Looking at the residuals of Model2

resid(Model4)

# I create a dataset with the residuals

res1 <- resid(Model4)

# Now I test the residuals for normality with a normal probability (QQ) plot
# If the dots follow the trend line, that would suggest that the dataset is normal distributed
# But in this case, the Normal Q-Q Plot present a trend (curve) that tends to flatten

?qqnorm
qqnorm(res1)
qqline(res1)

# Anderson Darling normality test

?ad.test
ad.test(res1)

# --------------------------------------------------------------

# Building a multiple linear regression changing some functional forms of the predictors (as suggested in the literature and trying to apply the natural log to mean ndvi, S and H because in the previous models these predictors without the log form were discarded by the backward selection or they resulted not significant)
# I directly discard avg_hh_inc because it is strongly correlated with pv_rt  (Pearson coef. -0.86)

FitAll_2 <- lm(log(price) ~ land_square_feet + I(land_square_feet^2) + gross_square_feet + I(gross_square_feet^2) + 
                  age_property + I(age_property^2) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                  log(mean_ndvi) + log(S) + log(H+1) + avg_ue_rt + pct_bk_ppl + pv_rt + pop_den + 
                  avg_tt + crime_rt, data = train_sample)

# Backward selection

step(FitAll_2, direction = "backward")

# The best model is the follow

Model5 <- lm(formula = log(price) ~ land_square_feet + I(land_square_feet^2) + 
                gross_square_feet + I(gross_square_feet^2) + age_property + 
                log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
                log(S) + log(H + 1) + pct_bk_ppl + pv_rt + pop_den + 
                crime_rt, data = train_sample)
summary(Model5)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model5)


# -------------------------------------------------------------

# I want also to check the resulting model using a stepwise regression (using both forward and backward)

# First I create an intercept-only model

FitStart_1 <- lm(log(price) ~ 1, data = train_sample)

# Now using the stepwise regression

step(FitStart_1, direction = "both", scope = formula(FitAll_2))

# Selected model

Model6 <- lm(formula = log(price) ~ gross_square_feet + crime_rt + 
                log(cbd_dist) + land_square_feet + I(land_square_feet^2) + 
                pct_bk_ppl + log(mean_ndvi) + log(wt_dist + 1) + I(gross_square_feet^2) + 
                log(S) + log(sch_dist) + age_property + pop_den + pv_rt + 
                log(H + 1), data = train_sample)
summary(Model6)

# Saem results of Model5
# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model6)

# -------------------------------------------------------------

# Now I am going to produce a model changing some functional forms

FitAll_3 <- lm(log(price) ~ land_square_feet + log(land_square_feet) + gross_square_feet + log(gross_square_feet) + 
                 age_property + sqrt(age_property) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + avg_ue_rt + pct_bk_ppl + pv_rt + 
                  pop_den + avg_tt + crime_rt, data = train_sample)

# Backward selection

step(FitAll_3, direction = "backward")

# Produced model

Model7 <- lm(formula = log(price) ~ log(land_square_feet) + gross_square_feet + 
                log(gross_square_feet) + age_property + sqrt(age_property) + 
                log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
                log(S) + log(H + 1) + pct_bk_ppl + pv_rt + pop_den + 
                crime_rt, data = train_sample)
summary(Model7)

# --------------------------------------------------------------------------------------

# Trying improvements of Model7 (discarding the non significant variable gross_square_feet) and changing some functional forms based on the previous variables plots (and normal histograms)

FitAll_4 <- lm(formula = log(price) ~ log(land_square_feet) + 
                  log(gross_square_feet) + age_property + sqrt(age_property) + 
                  log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + log(mean_ndvi) + 
                  log(S) + log(H + 1) + log(pct_bk_ppl) + pv_rt + pop_den + 
                  crime_rt, data = train_sample)

# Backward selection

step(FitAll_4, direction = "backward")

# Produced model

Model8 <- lm(formula = log(price) ~ log(land_square_feet) + log(gross_square_feet) + 
                age_property + sqrt(age_property) + log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + 
                log(mean_ndvi) + log(S) + log(H + 1) + log(pct_bk_ppl) + pv_rt + pop_den + crime_rt, 
             data = train_sample)
summary(Model8)

# -----------------------------------------------------------

# log-log linear model

FitAll_5 <- lm(log(price) ~ log(land_square_feet) + log(gross_square_feet) + 
                  log(age_property) + log(sch_dist) + log(wt_dist+1) + log(cbd_dist) + 
                  log(mean_ndvi) + log(S) + log(H+1) + log(avg_ue_rt) + log(pct_bk_ppl) + 
                  log(pv_rt) + log(pop_den) + log(avg_tt) + log(crime_rt), data = train_sample)

# Backward

step(FitAll_5, direction = "backward")

Model9 <- lm(formula = log(price) ~ log(land_square_feet) + log(gross_square_feet) + 
                log(age_property) + log(sch_dist) + log(wt_dist + 1) + log(cbd_dist) + 
                log(mean_ndvi) + log(S) + log(H + 1) + log(avg_ue_rt) + log(pct_bk_ppl) + 
                log(pv_rt) + log(pop_den) + log(avg_tt) + log(crime_rt), 
             data = train_sample)

summary(Model9)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model9)

# ------------------------------------------------------------------------------------------------

# Now I evaluate the final linear models
# Find all predicted values for both a training set and validation set

train_sample$pred_price_M5 <- predict(Model5, newdata = subset(train_sample, 
                                                               select = c(price, land_square_feet, gross_square_feet, 
                                                                          age_property, sch_dist, wt_dist, cbd_dist, 
                                                                          mean_ndvi, S, H, pct_bk_ppl, pv_rt, pop_den, 
                                                                          crime_rt)))
valid_sample$pred_price_M5 <- predict(Model5, newdata = subset(valid_sample, 
                                                               select = c(price, land_square_feet, gross_square_feet, 
                                                                          age_property, sch_dist, wt_dist, cbd_dist, 
                                                                          mean_ndvi, S, H, pct_bk_ppl, pv_rt, pop_den, 
                                                                          crime_rt)))
train_sample$pred_price_M8 <- predict(Model8, newdata = subset(train_sample, 
                                                               select = c(price, land_square_feet, gross_square_feet, 
                                                                          age_property, sch_dist, wt_dist, cbd_dist, 
                                                                          mean_ndvi, S, H, pct_bk_ppl, pv_rt, 
                                                                          pop_den, crime_rt)))
valid_sample$pred_price_M8 <- predict(Model8, newdata = subset(valid_sample, 
                                                               select = c(price, land_square_feet, gross_square_feet, 
                                                                          age_property, sch_dist, wt_dist, cbd_dist, 
                                                                          mean_ndvi, S, H, pct_bk_ppl, pv_rt, 
                                                                          pop_den, crime_rt)))

# The theoretical model performance is defined here as R-Squared
summary(Model5)
summary(Model8)

# Let's see whether the predicted price and the real price are in any way correlated to that degree
# Ideally if I look at the training sample, the correlation between prediction and price should be more or less in the level of magnitude of the Adjusted R-squared of Model 5 and Model 8

# Check how good are the models on the training sample looking at R-squared (correlation^2), root mean square (RMSE) difference between the predicted price and the real price for the training sample, and mean absolute error (MAE) for the same two vectors

train_corr_M5 <- round(cor(train_sample$pred_price_M5, train_sample$price), 2)
train_RMSE_M5 <- round(sqrt(mean((train_sample$pred_price_M5 - train_sample$price)^2)))
train_MAE_M5 <- round(mean(abs(train_sample$pred_price_M5 - train_sample$price)))
c(train_corr_M5^2, train_RMSE_M5, train_MAE_M5)
# 0.4356 712530.0000 673545.0000

train_corr_M8 <- round(cor(train_sample$pred_price_M8, train_sample$price), 2)
train_RMSE_M8 <- round(sqrt(mean((train_sample$pred_price_M8 - train_sample$price)^2)))
train_MAE_M8 <- round(mean(abs(train_sample$pred_price_M8 - train_sample$price)))
c(train_corr_M8^2, train_RMSE_M8, train_MAE_M8)
# 0.4225 712530.0000 673545.0000

# Check how good are the models on the validation sample - correlation^2, RMSE, MAE

valid_corr_M5 <- round(cor(valid_sample$pred_price_M5, valid_sample$price), 2)
valid_RMSE_M5 <- round(sqrt(mean((valid_sample$pred_price_M5 - valid_sample$price)^2)))
valid_MAE_M5 <- round(mean(abs(valid_sample$pred_price_M5 - valid_sample$price)))
c(valid_corr_M5^2, valid_RMSE_M5, valid_MAE_M5)
# 0.1681 711909.0000 673127.0000

valid_corr_M8 <- round(cor(valid_sample$pred_price_M8, valid_sample$price), 2)
valid_RMSE_M8 <- round(sqrt(mean((valid_sample$pred_price_M8 - valid_sample$price)^2)))
valid_MAE_M8 <- round(mean(abs(valid_sample$pred_price_M8 - valid_sample$price)))
c(valid_corr_M8^2, valid_RMSE_M8, valid_MAE_M8)
# 0.1444 711909.0000 673127.0000