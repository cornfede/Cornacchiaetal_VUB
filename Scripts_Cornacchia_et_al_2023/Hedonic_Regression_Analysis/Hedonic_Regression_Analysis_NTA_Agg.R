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

var2 <- read.csv("C:/Files/ESA/MSc Thesis Project/R_&_ArcGIS/NYC/Hedonic_Regression_Variables/Variables_Agg_NTA.csv", header = TRUE, sep = ",")

# Remove first column

var2$NTANAME <- NULL

# Renaming variable (from average median income to average household income)

colnames(var2)[colnames(var2) == "avg_med_inc"] <- "avg_hh_inc"
colnames(var2)[colnames(var2) == "pop_den_nta"] <- "pop_den"
colnames(var2)[colnames(var2) == "crime_rt_nta"] <- "crime_rt"

# Summary statistics of the considered variables

summary(var2)

# Calculating specific summary statistics (mean and standard deviation)

mean(var2$avg_price, na.rm = TRUE)
mean(var2$avg_land_square_feet, na.rm = TRUE)
mean(var2$avg_gross_square_feet, na.rm = TRUE)
mean(var2$avg_age, na.rm = TRUE)
mean(var2$avg_sch_dist, na.rm = TRUE)
mean(var2$avg_wt_dist, na.rm = TRUE)
mean(var2$avg_cbd_dist, na.rm = TRUE)
mean(var2$mean_ndvi, na.rm = TRUE)
mean(var2$S, na.rm = TRUE)
mean(var2$H, na.rm = TRUE)
mean(var2$avg_hh_inc, na.rm = TRUE)
mean(var2$avg_ue_rt, na.rm = TRUE)
mean(var2$pct_bk_ppl, na.rm = TRUE)
mean(var2$pv_rt, na.rm = TRUE)
mean(var2$pop_den, na.rm = TRUE)
mean(var2$avg_tt, na.rm = TRUE)
mean(var2$crime_rt, na.rm = TRUE)
sd(var2$avg_price, na.rm = TRUE)
sd(var2$avg_land_square_feet, na.rm = TRUE)
sd(var2$avg_gross_square_feet, na.rm = TRUE)
sd(var2$avg_age, na.rm = TRUE)
sd(var2$avg_sch_dist, na.rm = TRUE)
sd(var2$avg_wt_dist, na.rm = TRUE)
sd(var2$avg_cbd_dist, na.rm = TRUE)
sd(var2$mean_ndvi, na.rm = TRUE)
sd(var2$S, na.rm = TRUE)
sd(var2$H, na.rm = TRUE)
sd(var2$avg_hh_inc, na.rm = TRUE)
sd(var2$avg_ue_rt, na.rm = TRUE)
sd(var2$pct_bk_ppl, na.rm = TRUE)
sd(var2$pv_rt, na.rm = TRUE)
sd(var2$pop_den, na.rm = TRUE)
sd(var2$avg_tt, na.rm = TRUE)
sd(var2$crime_rt, na.rm = TRUE)

# Producing scatterplots

plot(var2$avg_land_square_feet, var2$avg_price, main = "Average Land Square Feet - Average Price", las = 1)
plot(var2$avg_gross_square_feet, var2$avg_price, main = "Average Gross Square Feet - Average Price", las = 1)
plot(var2$avg_age, var2$avg_price, main = "Average Age Property - Average Price", las = 1)
plot(var2$avg_sch_dist, var2$avg_price, main = "Average School Proximity - Average Price", las = 1)
plot(var2$avg_wt_dist, var2$avg_price, main = "Average Water Body Proximity - Average Price", las = 1)
plot(var2$avg_cbd_dist, var2$avg_price, main = "Average Central Business District Proximity - Average Price", las = 1)
plot(var2$mean_ndvi, var2$avg_price, main = "Mean NDVI - Average Price", las = 1)
plot(var2$S, var2$avg_price, main = "S - Average Price", las = 1)
plot(var2$H, var2$avg_price, main = "H - Average Price", las = 1)
plot(var2$avg_hh_inc, var2$avg_price, main = "Average Household Income - Average Price", las = 1)
plot(var2$avg_ue_rt, var2$avg_price, main = "Average Unemployment Rate - Average Price", las = 1)
plot(var2$pct_bk_ppl, var2$avg_price, main = "Percentage Black People - Average Price", las = 1)
plot(var2$pv_rt, var2$avg_price, main = "Poverty Rate - Average Price", las = 1)
plot(var2$pop_den, var2$avg_price, main = "Population Density - Average Price", las = 1)
plot(var2$avg_tt, var2$avg_price, main = "Average Commuting Time (min) - Average Price", las = 1)
plot(var2$crime_rt, var2$avg_price, main = "Crime Rate - Average Price", las = 1)

# Running Pearson Correlation Coefficients 

cor.test(var2$avg_price, var2$avg_land_square_feet)
cor.test(var2$avg_price, var2$avg_gross_square_feet)
cor.test(var2$avg_price, var2$avg_age)
cor.test(var2$avg_price, var2$avg_sch_dist)
cor.test(var2$avg_price, var2$avg_wt_dist)
cor.test(var2$avg_price, var2$avg_cbd_dist)
cor.test(var2$avg_price, var2$mean_ndvi)
cor.test(var2$avg_price, var2$S)
cor.test(var2$avg_price, var2$H)
cor.test(var2$avg_price, var2$avg_hh_inc)
cor.test(var2$avg_price, var2$avg_ue_rt)
cor.test(var2$avg_price, var2$pct_bk_ppl)
cor.test(var2$avg_price, var2$pv_rt)
cor.test(var2$avg_price, var2$pop_den)
cor.test(var2$avg_price, var2$avg_tt)
cor.test(var2$avg_price, var2$crime_rt)

# Obtaining a correlation coefficient matrix (to assess the relationship between the ind. var. with each other)
# At this level of data aggregation (nta) there seems to be no significant multicollinearity between variables except for the correlation coefficient between poverty rate and average median income with an absolute value of 0.86

cor(var2, use = "complete.obs")

# In the H column, I have different NAs that I transform in zeros

var2$H[which(is.na(var2$H))] <- 0 

# Important to keep in mind that regression cannot deal with missing values
# Therefore I need to remove missing values from the dataset

sum(is.na(var2))
var_noNA <- na.omit(var2)
summary(var_noNA)

# Another quick visualization inspection of the variables (pairwise correlation matrix)

pairs.panels(var_noNA, col = "red")

# Split dataset into "training" sample (80%) and "validation" sample (20%)
# In this way, later on there can be a cross-validation type of testing of the model

set.seed(2021)
train_size <- 0.8
train_index <- sample.int(length(var_noNA$avg_price), round(length(var_noNA$avg_price) * train_size))
train_sample <- var_noNA[train_index,]
valid_sample <- var_noNA[-train_index,]

# Producing scatterplots and correlation coefficients considering different functional forms (to try dealing with nonlinearity)

# Land Square Feet 

plot(train_sample$avg_land_square_feet, train_sample$avg_price, main = "Average Land Square Feet - Average Price", las = 1)
plot(train_sample$avg_land_square_feet, log(train_sample$avg_price), main = "Average Land Square Feet - Log Average Price", las = 1)
plot(log(train_sample$avg_land_square_feet), log(train_sample$avg_price), main = "Log Average Land Square Feet - Log Average Price", las = 1)
plot((train_sample$avg_land_square_feet + I(train_sample$avg_land_square_feet^2)), log(train_sample$avg_price), main = "Quadratic Average Land Square Feet - Log Average Price", las = 1)
plot((train_sample$avg_land_square_feet + log(train_sample$avg_land_square_feet)), log(train_sample$avg_price), main = " Average LSqrFeet + log(Average LSqrFeet) - Log Average Price")

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_land_square_feet)
cor.test(log(train_sample$avg_price), train_sample$avg_land_square_feet)
cor.test(log(train_sample$avg_price), log(train_sample$avg_land_square_feet))
cor.test(log(train_sample$avg_price), (train_sample$avg_land_square_feet + I(train_sample$avg_land_square_feet^2)))
cor.test(log(train_sample$avg_price), (train_sample$avg_land_square_feet + log(train_sample$avg_land_square_feet)))

# Gross Square Feet

plot(train_sample$avg_gross_square_feet, train_sample$avg_price, main = "Average Gross Square Feet - Average Price", las = 1)
plot(train_sample$avg_gross_square_feet, log(train_sample$avg_price), main = "Average Gross Square Feet - Log Average Price", las = 1)
plot(log(train_sample$avg_gross_square_feet), log(train_sample$avg_price), main = "Log Average Gross Square Feet - Log Average Price", las = 1)
plot((train_sample$avg_gross_square_feet + I(train_sample$avg_gross_square_feet^2)), log(train_sample$avg_price), main = "Quadratic Average Gross Square Feet - Log Average Price", las = 1)
plot((train_sample$avg_gross_square_feet + log(train_sample$avg_gross_square_feet)), log(train_sample$avg_price), main = " Average GSqrFeet + log(Average GSqrFeet) - Log Average Price")

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_gross_square_feet)
cor.test(log(train_sample$avg_price), train_sample$avg_gross_square_feet)
cor.test(log(train_sample$avg_price), log(train_sample$avg_gross_square_feet))
cor.test(log(train_sample$avg_price), (train_sample$avg_gross_square_feet + I(train_sample$avg_gross_square_feet^2)))
cor.test(log(train_sample$avg_price), (train_sample$avg_gross_square_feet + log(train_sample$avg_gross_square_feet)))

# Age Property

plot(train_sample$avg_age, train_sample$avg_price, main = "Average Age Property - Average Price", las = 1)
plot(train_sample$avg_age, log(train_sample$avg_price), main = "Average Age Property - Log Average Price", las = 1)
plot((train_sample$avg_age + I(train_sample$avg_age^2)), log(train_sample$avg_price), main = "Quadratic Average Age Property - Log Average Price", las = 1)
plot((train_sample$avg_age + sqrt(train_sample$avg_age)), log(train_sample$avg_price), main = " Average Age + SQRT(Average Age) - Log Average Price")

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_age)
cor.test(log(train_sample$avg_price), train_sample$avg_age)
cor.test(log(train_sample$avg_price), (train_sample$avg_age + I(train_sample$avg_age^2)))
cor.test(log(train_sample$avg_price), (train_sample$avg_age + sqrt(train_sample$avg_age)))

# School Proximity

plot(train_sample$avg_sch_dist, train_sample$avg_price, main = "Average School Proximity - Average Price", las = 1)
plot(train_sample$avg_sch_dist, log(train_sample$avg_price), main = "Average School Proximity - Log Average Price", las = 1)
plot(log(train_sample$avg_sch_dist), log(train_sample$avg_price), main = "Log Average School Proximity - Log Average Price", las = 1)
plot(sqrt(train_sample$avg_sch_dist), log(train_sample$avg_price), main = "SQRT Average School Proximity - Log Average Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_sch_dist)
cor.test(log(train_sample$avg_price), train_sample$avg_sch_dist)
cor.test(log(train_sample$avg_price), log(train_sample$avg_sch_dist))
cor.test(log(train_sample$avg_price), sqrt(train_sample$avg_sch_dist))

# Water Body Proximity

plot(train_sample$avg_wt_dist, train_sample$avg_price, main = "Average Water Body Proximity - Average Price", las = 1)
plot(train_sample$avg_wt_dist, log(train_sample$avg_price), main = "Average Water Body Proximity - Log Average Price", las = 1)
plot(log(train_sample$avg_wt_dist+1), log(train_sample$avg_price), main = "Log Average Water Body Proximity - Log Average Price", las = 1)
plot(sqrt(train_sample$avg_wt_dist), log(train_sample$avg_price), main = "SQRT Average Water Body Proximity - Log Average Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_wt_dist)
cor.test(log(train_sample$avg_price), train_sample$avg_wt_dist)
cor.test(log(train_sample$avg_price), log(train_sample$avg_wt_dist+1))
cor.test(log(train_sample$avg_price), sqrt(train_sample$avg_wt_dist))

# Central Business District Proximity

plot(train_sample$avg_cbd_dist, train_sample$avg_price, main = "Average Central Business District Proximity - Average Price", las = 1)
plot(train_sample$avg_cbd_dist, log(train_sample$avg_price), main = "Average Central Business District Proximity - Log Average Price", las = 1)
plot(log(train_sample$avg_cbd_dist), log(train_sample$avg_price), main = "Log Average Central Business District Proximity - Log Average Price", las = 1)
plot(sqrt(train_sample$avg_cbd_dist), log(train_sample$avg_price), main = "SQRT Average Central Business District Proximity - Log Average Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_cbd_dist)
cor.test(log(train_sample$avg_price), train_sample$avg_cbd_dist)
cor.test(log(train_sample$avg_price), log(train_sample$avg_cbd_dist))
cor.test(log(train_sample$avg_price), sqrt(train_sample$avg_cbd_dist))

# Mean NDVI per NTA

plot(train_sample$mean_ndvi, train_sample$avg_price, main = "Mean NDVI - Price", las = 1)
plot(train_sample$mean_ndvi, log(train_sample$avg_price), main = "Mean NDVI - Log Price", las = 1)
plot(log(train_sample$mean_ndvi), log(train_sample$avg_price), main = "Log Mean NDVI - Log Price", las = 1)
plot(sqrt(train_sample$mean_ndvi), log(train_sample$avg_price), main = "SQRT Mean NDVI - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$mean_ndvi)
cor.test(log(train_sample$avg_price), train_sample$mean_ndvi)
cor.test(log(train_sample$avg_price), log(train_sample$mean_ndvi))
cor.test(log(train_sample$avg_price), sqrt(train_sample$mean_ndvi))

# Species Richness (S) per NTA

plot(train_sample$S, train_sample$avg_price, main = "S - Price", las = 1)
plot(train_sample$S, log(train_sample$avg_price), main = "S - Log Price", las = 1)
plot(log(train_sample$S), log(train_sample$avg_price), main = "Log S - Log Price", las = 1)
plot(sqrt(train_sample$S), log(train_sample$avg_price), main = "SQRT S - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$S)
cor.test(log(train_sample$avg_price), train_sample$S)
cor.test(log(train_sample$avg_price), log(train_sample$S))
cor.test(log(train_sample$avg_price), sqrt(train_sample$S))

# Species Diversity (H) per NTA

plot(train_sample$H, train_sample$avg_price, main = "H - Price", las = 1)
plot(train_sample$H, log(train_sample$avg_price), main = "H - Log Price", las = 1)
plot(log(train_sample$H), log(train_sample$avg_price), main = "Log H - Log Price", las = 1)
plot(sqrt(train_sample$H), log(train_sample$avg_price), main = "SQRT H - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$H)
cor.test(log(train_sample$avg_price), train_sample$H)
cor.test(log(train_sample$avg_price), log(train_sample$H+1))
cor.test(log(train_sample$avg_price), sqrt(train_sample$H))

# Average Household Income per NTA

plot(train_sample$avg_hh_inc, train_sample$avg_price, main = "Average Household Income - Price", las = 1)
plot(train_sample$avg_hh_inc, log(train_sample$avg_price), main = "Average Household Income - Log Price", las = 1)
plot(log(train_sample$avg_hh_inc), log(train_sample$avg_price), main = "Log Average Household Income - Log Price", las = 1)
plot(sqrt(train_sample$avg_hh_inc), log(train_sample$avg_price), main = "SQRT Average Household Income - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_hh_inc)
cor.test(log(train_sample$avg_price), train_sample$avg_hh_inc)
cor.test(log(train_sample$avg_price), log(train_sample$avg_hh_inc))
cor.test(log(train_sample$avg_price), sqrt(train_sample$avg_hh_inc))

# Average Unemployment Rate per NTA

plot(train_sample$avg_ue_rt, train_sample$avg_price, main = "Average Unemployment Rate - Price", las = 1)
plot(train_sample$avg_ue_rt, log(train_sample$avg_price), main = "Average Unemployment Rate - Log Price", las = 1)
plot(log(train_sample$avg_ue_rt), log(train_sample$avg_price), main = "Log Average Unemployment Rate - Log Price", las = 1)
plot(sqrt(train_sample$avg_ue_rt), log(train_sample$avg_price), main = "SQRT Average Unemployment Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_ue_rt)
cor.test(log(train_sample$avg_price), train_sample$avg_ue_rt)
cor.test(log(train_sample$avg_price), log(train_sample$avg_ue_rt))
cor.test(log(train_sample$avg_price), sqrt(train_sample$avg_ue_rt))

# Percentage Black People per NTA

plot(train_sample$pct_bk_ppl, train_sample$avg_price, main = "Percentage Black People - Price", las = 1)
plot(train_sample$pct_bk_ppl, log(train_sample$avg_price), main = "Percentage Black People - Log Price", las = 1)
plot(log(train_sample$pct_bk_ppl), log(train_sample$avg_price), main = "Log Percentage Black People - Log Price", las = 1)
plot(sqrt(train_sample$pct_bk_ppl), log(train_sample$avg_price), main = "SQRT Percentage Black People - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$pct_bk_ppl)
cor.test(log(train_sample$avg_price), train_sample$pct_bk_ppl)
cor.test(log(train_sample$avg_price), log(train_sample$pct_bk_ppl))
cor.test(log(train_sample$avg_price), sqrt(train_sample$pct_bk_ppl))

# Poverty Rate per NTA

plot(train_sample$pv_rt, train_sample$avg_price, main = "Poverty Rate - Price", las = 1)
plot(train_sample$pv_rt, log(train_sample$avg_price), main = "Poverty Rate - Log Price", las = 1)
plot(log(train_sample$pv_rt), log(train_sample$avg_price), main = "Log Poverty Rate - Log Price", las = 1)
plot(sqrt(train_sample$pv_rt), log(train_sample$avg_price), main = "SQRT Poverty Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$pv_rt)
cor.test(log(train_sample$avg_price), train_sample$pv_rt)
cor.test(log(train_sample$avg_price), log(train_sample$pv_rt))
cor.test(log(train_sample$avg_price), sqrt(train_sample$pv_rt))

# Population Density per NTA

plot(train_sample$pop_den, train_sample$avg_price, main = "Population Density - Price", las = 1)
plot(train_sample$pop_den, log(train_sample$avg_price), main = "Population Density - Log Price", las = 1)
plot(log(train_sample$pop_den), log(train_sample$avg_price), main = "Log Population Density - Log Price", las = 1)
plot(sqrt(train_sample$pop_den), log(train_sample$avg_price), main = "SQRT Population Density - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$pop_den)
cor.test(log(train_sample$avg_price), train_sample$pop_den)
cor.test(log(train_sample$avg_price), log(train_sample$pop_den))
cor.test(log(train_sample$avg_price), sqrt(train_sample$pop_den))

# Mean Travel Time to work (min) per NTA

plot(train_sample$avg_tt, train_sample$avg_price, main = "Average Commuting Time - Price", las = 1)
plot(train_sample$avg_tt, log(train_sample$avg_price), main = "Average Commuting Time - Log Price", las = 1)
plot(log(train_sample$avg_tt), log(train_sample$avg_price), main = "Log Average Commuting Time - Log Price", las = 1)
plot(sqrt(train_sample$avg_tt), log(train_sample$avg_price), main = "SQRT Average Commuting Time - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$avg_tt)
cor.test(log(train_sample$avg_price), train_sample$avg_tt)
cor.test(log(train_sample$avg_price), log(train_sample$avg_tt))
cor.test(log(train_sample$avg_price), sqrt(train_sample$avg_tt))

# Crime Rate per NTA

plot(train_sample$crime_rt, train_sample$avg_price, main = "Crime Rate - Price", las = 1)
plot(train_sample$crime_rt, log(train_sample$avg_price), main = "Crime Rate - Log Price", las = 1)
plot(log(train_sample$crime_rt), log(train_sample$avg_price), main = "Log Crime Rate - Log Price", las = 1)
plot(sqrt(train_sample$crime_rt), log(train_sample$avg_price), main = "SQRT Crime Rate - Log Price", las = 1)

# Pearson Correlation Coefficient

cor.test(train_sample$avg_price, train_sample$crime_rt)
cor.test(log(train_sample$avg_price), train_sample$crime_rt)
cor.test(log(train_sample$avg_price), log(train_sample$crime_rt))
cor.test(log(train_sample$avg_price), sqrt(train_sample$crime_rt))

# Correlation matrix and pairs panels on training sample

cor(train_sample)
pairs.panels(train_sample, lm = TRUE, col = "red")

# I also test the normality of each variable looking at the density plot

par(mfrow = c(1, 1))
plot(density(train_sample$avg_price))
plot(density(train_sample$avg_land_square_feet))
plot(density(train_sample$avg_gross_square_feet))
plot(density(train_sample$avg_age))
plot(density(train_sample$avg_sch_dist))
plot(density(train_sample$avg_wt_dist))
plot(density(train_sample$avg_cbd_dist))
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

plotNormalHistogram(train_sample$avg_price)
plotNormalHistogram(log(train_sample$avg_price))
plotNormalHistogram(sqrt(train_sample$avg_price))
qqnorm(train_sample$avg_price)
qqline(train_sample$avg_price)
qqnorm(log(train_sample$avg_price))
qqline(log(train_sample$avg_price))
qqnorm(sqrt(train_sample$avg_price))
qqline(sqrt(train_sample$avg_price))

plotNormalHistogram(train_sample$avg_land_square_feet)
plotNormalHistogram(train_sample$avg_land_square_feet^2)
plotNormalHistogram(train_sample$avg_land_square_feet + I(train_sample$avg_land_square_feet^2))
plotNormalHistogram(log(train_sample$avg_land_square_feet))
plotNormalHistogram(train_sample$avg_land_square_feet + log(train_sample$avg_land_square_feet))
plotNormalHistogram(sqrt(train_sample$avg_land_square_feet))
qqnorm(train_sample$avg_land_square_feet)
qqline(train_sample$avg_land_square_feet)
qqnorm(train_sample$avg_land_square_feet^2)
qqline(train_sample$avg_vland_square_feet^2)
qqnorm(train_sample$avg_land_square_feet + I(train_sample$avg_land_square_feet^2))
qqline(train_sample$avg_land_square_feet + I(train_sample$avg_land_square_feet^2))
qqnorm(log(train_sample$avg_land_square_feet))
qqline(log(train_sample$avg_land_square_feet))
qqnorm(train_sample$avg_land_square_feet + log(train_sample$avg_land_square_feet))
qqline(train_sample$avg_land_square_feet + log(train_sample$avg_land_square_feet))
qqnorm(sqrt(train_sample$avg_land_square_feet))
qqline(sqrt(train_sample$avg_land_square_feet))

plotNormalHistogram(train_sample$avg_gross_square_feet)
plotNormalHistogram(train_sample$avg_gross_square_feet^2)
plotNormalHistogram(train_sample$avg_gross_square_feet + I(train_sample$avg_gross_square_feet^2))
plotNormalHistogram(log(train_sample$avg_gross_square_feet))
plotNormalHistogram(train_sample$avg_gross_square_feet + log(train_sample$avg_gross_square_feet))
plotNormalHistogram(sqrt(train_sample$avg_gross_square_feet))
qqnorm(train_sample$avg_gross_square_feet)
qqline(train_sample$avg_gross_square_feet)
qqnorm(train_sample$avg_gross_square_feet^2)
qqline(train_sample$avg_gross_square_feet^2)
qqnorm(train_sample$avg_gross_square_feet + I(train_sample$avg_gross_square_feet^2))
qqline(train_sample$avg_gross_square_feet + I(train_sample$avg_gross_square_feet^2))
qqnorm(log(train_sample$avg_gross_square_feet))
qqline(log(train_sample$avg_gross_square_feet))
qqnorm(train_sample$avg_gross_square_feet + log(train_sample$avg_gross_square_feet))
qqline(train_sample$avg_gross_square_feet + log(train_sample$avg_gross_square_feet))
qqnorm(sqrt(train_sample$avg_gross_square_feet))
qqline(sqrt(train_sample$avg_gross_square_feet))

plotNormalHistogram(train_sample$avg_age)
plotNormalHistogram(train_sample$avg_age^2)
plotNormalHistogram(train_sample$avg_age + I(train_sample$avg_age^2))
plotNormalHistogram(log(train_sample$avg_age))
plotNormalHistogram(train_sample$avg_age + log(train_sample$avg_age))
plotNormalHistogram(sqrt(train_sample$avg_age))
plotNormalHistogram(train_sample$avg_age + sqrt(train_sample$avg_age))
qqnorm(train_sample$avg_age)
qqline(train_sample$avg_age)
qqnorm(train_sample$avg_age^2)
qqline(train_sample$avg_age^2)
qqnorm(train_sample$avg_age + I(train_sample$avg_age^2))
qqline(train_sample$avg_age + I(train_sample$avg_age^2))
qqnorm(log(train_sample$avg_age))
qqline(log(train_sample$avg_age))
qqnorm(train_sample$avg_age + log(train_sample$avg_age))
qqline(train_sample$avg_age + log(train_sample$avg_age))
qqnorm(sqrt(train_sample$avg_age))
qqline(sqrt(train_sample$avg_age))
qqnorm(train_sample$avg_age + sqrt(train_sample$avg_age))
qqline(train_sample$avg_age + sqrt(train_sample$avg_age))

plotNormalHistogram(train_sample$avg_sch_dist)
plotNormalHistogram(log(train_sample$avg_sch_dist))
plotNormalHistogram(sqrt(train_sample$avg_sch_dist))
qqnorm(train_sample$avg_sch_dist)
qqline(train_sample$avg_sch_dist)
qqnorm(log(train_sample$avg_sch_dist))
qqline(log(train_sample$avg_sch_dist))
qqnorm(sqrt(train_sample$avg_sch_dist))
qqline(sqrt(train_sample$avg_sch_dist))

plotNormalHistogram(train_sample$avg_wt_dist)
plotNormalHistogram(log(train_sample$avg_wt_dist + 1))
plotNormalHistogram(sqrt(train_sample$avg_wt_dist))
qqnorm(train_sample$avg_wt_dist)
qqline(train_sample$avg_wt_dist)
qqnorm(log(train_sample$avg_wt_dist + 1))
qqline(log(train_sample$avg_wt_dist + 1))
qqnorm(sqrt(train_sample$avg_wt_dist))
qqline(sqrt(train_sample$avg_wt_dist))

plotNormalHistogram(train_sample$avg_cbd_dist)
plotNormalHistogram(log(train_sample$avg_cbd_dist))
plotNormalHistogram(sqrt(train_sample$avg_cbd_dist))
qqnorm(train_sample$avg_cbd_dist)
qqline(train_sample$avg_cbd_dist)
qqnorm(log(train_sample$avg_cbd_dist))
qqline(log(train_sample$avg_cbd_dist))
qqnorm(sqrt(train_sample$avg_cbd_dist))
qqline(sqrt(train_sample$avg_cbd_dist))

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

# Stepwise regression (backward selection)

# Now I create a linear model fitted using all the predictor variables

FitAll <- lm(avg_price ~ ., data = train_sample)
summary(FitAll)

# Usually backward selection is preferred

step(FitAll, direction = "backward")

# Resulting model from the previous backward selection 

Model1 <- lm(formula = avg_price ~ avg_land_square_feet + avg_sch_dist + 
               avg_cbd_dist + mean_ndvi + H + avg_hh_inc + avg_ue_rt + pct_bk_ppl + 
               pv_rt + pop_den + avg_tt, data = train_sample)
summary(Model1)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model1)

# S was discarded
# Need to check different functional forms

# ---------------------------------------------------------------

# Trying to introduce log(price) in the backward selection
# Create a linear model fitted using all the predictor variables

FitAll_logprice <- lm(log(avg_price) ~ ., data = train_sample)
summary(FitAll_logprice)

# backward selection

step(FitAll_logprice, direction = "backward")

# Resulting model from the previous backward selection (log model)

Model2 <- lm(formula = log(avg_price) ~ avg_land_square_feet + avg_gross_square_feet + 
               avg_sch_dist + avg_cbd_dist + H + avg_hh_inc + pct_bk_ppl + 
               pv_rt + pop_den, data = train_sample)
summary(Model2)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model2)


# -------------------------------------------------------------

# I remove pv-rt because is too highly correlated with avg_hh_inc
# log-log linear model

FitAll_1 <- lm(log(avg_price) ~ log(avg_land_square_feet) + log(avg_gross_square_feet) + 
                 log(avg_age) + log(avg_sch_dist) + log(avg_wt_dist+1) + log(avg_cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + log(avg_hh_inc) + log(avg_ue_rt) + 
                 log(pct_bk_ppl) + log(pop_den) + log(avg_tt) + log(crime_rt), 
               data = train_sample)

# Backward

step(FitAll_1, direction = "backward")

Model3 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + log(avg_gross_square_feet) + 
               log(avg_sch_dist) + log(avg_cbd_dist) + log(S) + log(avg_hh_inc) + 
               log(avg_ue_rt) + log(pct_bk_ppl) + log(pop_den), data = train_sample)

summary(Model3)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model3)

# --------------------------------------------------------------

# Building a multiple linear regression changing some functional forms of the predictors (as suggested in the literature and trying to apply the natural log to mean ndvi, S and H)
# I directly discard pv_rt because it is strongly correlated with avg_hh_inc

FitAll_2 <- lm(log(avg_price) ~ avg_land_square_feet + I(avg_land_square_feet^2) + 
                 avg_gross_square_feet + I(avg_gross_square_feet^2) + avg_age + I(avg_age^2) + 
                 log(avg_sch_dist) + log(avg_wt_dist+1) + log(avg_cbd_dist) + 
                 log(mean_ndvi) + log(S) + log(H+1) + avg_hh_inc + avg_ue_rt + pct_bk_ppl + pop_den + 
                 avg_tt + crime_rt, data = train_sample)

# Backward selection

step(FitAll_2, direction = "backward")

# The best model is the follow

Model4 <- lm(formula = log(avg_price) ~ avg_land_square_feet + avg_gross_square_feet + 
               I(avg_gross_square_feet^2) + log(avg_cbd_dist) + log(mean_ndvi) + 
               log(S) + avg_hh_inc + avg_ue_rt + pct_bk_ppl + pop_den, 
             data = train_sample)
summary(Model4)

# Check the regression diagnostic plots for this model (this function returns 'Residual vs Fitted' graph and the QQ plot)

plot(Model4)

# -------------------------------------------------------------

# Now I am going to produce a model changing some functional forms (based on the previous normality tests)

FitAll_3 <- lm(log(avg_price) ~ avg_land_square_feet + log(avg_land_square_feet) + avg_gross_square_feet + 
                 log(avg_gross_square_feet) + avg_age + sqrt(avg_age) + log(avg_sch_dist) + 
                 log(avg_wt_dist+1) + log(avg_cbd_dist) + log(mean_ndvi) + log(S) + log(H+1) +  avg_hh_inc + 
                 avg_ue_rt + pct_bk_ppl + pop_den + avg_tt + crime_rt, data = train_sample)

# Backward selection

step(FitAll_3, direction = "backward")

# Produced model

Model5 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + avg_gross_square_feet + 
               log(avg_gross_square_feet) + log(avg_cbd_dist) + log(mean_ndvi) + 
               log(S) + avg_hh_inc + avg_ue_rt + pct_bk_ppl + pop_den, 
             data = train_sample)
summary(Model5)

# --------------------------------------------------------------------------------------

# Trying improvements of Model3 changing some functional forms based on the previous variables plots (and normal histograms)

FitAll_4 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + avg_gross_square_feet + 
                 I(avg_gross_square_feet^2) + avg_sch_dist + log(avg_cbd_dist) + log(S) + log(avg_hh_inc) + 
                 log(avg_ue_rt) + sqrt(pct_bk_ppl) + log(pop_den), data = train_sample)

# Backward selection

step(FitAll_4, direction = "backward")

# Produced model

Model6 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + avg_gross_square_feet + 
               I(avg_gross_square_feet^2) + avg_sch_dist + log(avg_cbd_dist) + 
               log(S) + log(avg_hh_inc) + sqrt(pct_bk_ppl) + log(pop_den), 
             data = train_sample)
summary(Model6)

# -----------------------------------------------------------

# Trying improvements of Model6 changing some functional forms based on the previous variables plots (and normal histograms)

FitAll_5 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + avg_gross_square_feet + 
                 + I(avg_gross_square_feet^2) + avg_age + sqrt(avg_age) + avg_sch_dist + avg_wt_dist + 
                 + log(avg_cbd_dist) + log(mean_ndvi) + log(S) + H + log(avg_hh_inc) + 
                 + log(avg_ue_rt) + sqrt(pct_bk_ppl) + log(pop_den) + log(avg_tt) + 
                 + log(crime_rt), data = train_sample)

# Backward selection

step(FitAll_5, direction = "backward")

# Produced model

Model7 <- lm(formula = log(avg_price) ~ log(avg_land_square_feet) + avg_gross_square_feet + 
               I(avg_gross_square_feet^2) + avg_sch_dist + log(avg_cbd_dist) + 
               H + log(avg_hh_inc) + sqrt(pct_bk_ppl) + log(pop_den), 
             data = train_sample)
summary(Model7)

# ------------------------------------------------------------------------------------------------

# Now I evaluate the final linear models
# Find all predicted values for both training set and validation set

train_sample$pred_price_M4 <- predict(Model4, newdata = subset(train_sample, 
                                                               select = c(avg_price, avg_land_square_feet, 
                                                                          avg_gross_square_feet, avg_cbd_dist, 
                                                                          mean_ndvi, S, avg_hh_inc, avg_ue_rt, 
                                                                          pct_bk_ppl, pop_den)))
valid_sample$pred_price_M4 <- predict(Model4, newdata = subset(valid_sample, 
                                                               select = c(avg_price, avg_land_square_feet, 
                                                                          avg_gross_square_feet, avg_cbd_dist, 
                                                                          mean_ndvi, S, avg_hh_inc, avg_ue_rt, 
                                                                          pct_bk_ppl, pop_den)))
train_sample$pred_price_M7 <- predict(Model7, newdata = subset(train_sample, 
                                                               select = c(avg_price, avg_land_square_feet, 
                                                                          avg_gross_square_feet, avg_sch_dist, 
                                                                          avg_cbd_dist, H, avg_hh_inc, 
                                                                          pct_bk_ppl, pop_den)))
valid_sample$pred_price_M7 <- predict(Model7, newdata = subset(valid_sample, 
                                                               select = c(avg_price, avg_land_square_feet, 
                                                                          avg_gross_square_feet, avg_sch_dist, 
                                                                          avg_cbd_dist, H, avg_hh_inc, 
                                                                          pct_bk_ppl, pop_den)))

# The theoretical model performance is defined here as R-Squared
summary(Model4)
summary(Model7)

# Let's see whether the predicted price and the real price are in any way correlated to that degree
# Ideally if I look at the training sample, the correlation between prediction and price should be more or less in the level of magnitude of the Adjusted R-squared of Model 4 and Model 7

# Check how good are the models on the training sample looking at R-squared (correlation^2), root mean square (RMSE) difference between the predicted price and the real price for the training sample, and mean absolute error (MAE) for the same two vectors

train_corr_M4 <- round(cor(train_sample$pred_price_M4, train_sample$avg_price), 2)
train_RMSE_M4 <- round(sqrt(mean((train_sample$pred_price_M4 - train_sample$avg_price)^2)))
train_MAE_M4 <- round(mean(abs(train_sample$pred_price_M4 - train_sample$avg_price)))
c(train_corr_M4^2, train_RMSE_M4, train_MAE_M4)
# 0.5776 748871.0000 715260.0000

train_corr_M7 <- round(cor(train_sample$pred_price_M7, train_sample$avg_price), 2)
train_RMSE_M7 <- round(sqrt(mean((train_sample$pred_price_M7 - train_sample$avg_price)^2)))
train_MAE_M7 <- round(mean(abs(train_sample$pred_price_M7 - train_sample$avg_price)))
c(train_corr_M7^2, train_RMSE_M7, train_MAE_M7)
# 0.64 748871.00 715260.00

# Check how good are the models on the validation sample - correlation^2, RMSE, MAE

valid_corr_M4 <- round(cor(valid_sample$pred_price_M4, valid_sample$avg_price), 2)
valid_RMSE_M4 <- round(sqrt(mean((valid_sample$pred_price_M4 - valid_sample$avg_price)^2)))
valid_MAE_M4 <- round(mean(abs(valid_sample$pred_price_M4 - valid_sample$avg_price)))
c(valid_corr_M4^2, valid_RMSE_M4, valid_MAE_M4)
# 0.5329 741474.0000 709397.0000

valid_corr_M7 <- round(cor(valid_sample$pred_price_M7, valid_sample$avg_price), 2)
valid_RMSE_M7 <- round(sqrt(mean((valid_sample$pred_price_M7 - valid_sample$avg_price)^2)))
valid_MAE_M7 <- round(mean(abs(valid_sample$pred_price_M7 - valid_sample$avg_price)))
c(valid_corr_M7^2, valid_RMSE_M7, valid_MAE_M7)
# 0.6241 741474.0000 709397.0000
