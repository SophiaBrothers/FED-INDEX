install.packages("tidyverse")
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
install.packages("skimr")
library(stats)

fed.reserve = read.csv("C:/Users/snfra/OneDrive/Documents/RAW DATA/FedReserveIndex.csv",header = TRUE, sep = ",")

# my observations
# 1 seems to be the most frequent day
# 'year', 'month', 'day' should be factors

head(fed.reserve)
tail(fed.reserve)
str(fed.reserve)
class(fed.reserve)
dim(fed.reserve)    # rows and columns
colnames(fed.reserve)
glimpse(fed.reserve)
summary(fed.reserve)
skimr::skim(fed.reserve)

# BASIC OBSERVATIONS
hist(fed.reserve$Year)
hist(fed.reserve$Month)
hist(fed.reserve$Day)
hist(fed.reserve$Unemployment.Rate)
hist(fed.reserve$Inflation.Rate)


# to check dataset for special characters
grepl('[^[:punct:]]', fed.reserve)
sum(grepl('[^[:punct:]]', fed.reserve))
# will result in TRUE or FALSE for each value in your vector. You can add sum() to 
# the beginning of the statement to get the total number of these cases

grepl('[^[:alnum:]]', fed.reserve)
# Can also use this. It will check for any value that is not a letter or a number.

# find the amount of missing data and put them in a table format
missing_values <- fed.reserve %>%
  map_df(function(i) sum(is.na(i))) %>%
  gather(feature, total_null_count) %>%
  arrange(desc(total_null_count))

missing_values

# CLEANING
fed.reserve$Year <- as.factor(fed.reserve$Year)
fed.reserve$Month <- as.factor(fed.reserve$Month)
fed.reserve$Day <- as.factor(fed.reserve$Day)

# Too much missing data to just remove. perhaps drop variables 'Federal.Funds.Upper Target' 
# and 'Federal.Funds.Lower.Target'
# replace the others with the median numbers?

targets <- c("Federal.Funds.Upper.Target", "Federal.Funds.Lower.Target")
fed.reduced <- fed.reserve[,!(names(fed.reserve) %in% targets)]
names(fed.reduced)


# Calculate Outlier Scores
install.packages("DMwR")
library(DMwR)
# remove categorical columns
reserve.num <- fed.reserve[,4:10]
outlier.scores <- lofactor(reserve.num, k=5)
plot(density(outlier.scores))

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)
