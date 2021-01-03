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

# Rename long Variables
fed.reserve <- plyr::rename(fed.reserve, c("Effective.Federal.Funds.Rate"="EFFR",
                                           "Federal.Funds.Target.Rate"="FFTR",
                                           "Federal.Funds.Upper.Target" = "FFUT", 
                                           "Federal.Funds.Lower.Target"="FFLT", 
                                           "Real.GDP..Percent.Change." = "GDP",
                                           "Unemployment.Rate" ="UR",
                                           "Inflation.Rate"= "IR")) 

# to check dataset for special characters
grepl('[^[:punct:]]', fed.reserve)
sum(grepl('[^[:punct:]]', fed.reserve))
# will result in TRUE or FALSE for each value in your vector. You can add sum() to 
# the beginning of the statement to get the total number of these cases

grepl('[^[:alnum:]]', fed.reserve)
# Can also use this. It will check for any value that is not a letter or a number.

# TO SEE MISSING DATA IN THE DATAFRAME
is.na(fed.reserve)

# TO SEE MISSIG DATA IN A SPECIFIC COLUMN
is.na(fed.reserve$FFTR)

# find the amount of missing data and put them in a table format
missing_values <- fed.reserve %>%
  map_df(function(i) sum(is.na(i))) %>%
  gather(feature, total_null_count) %>%
  arrange(desc(total_null_count))

missing_values


# recode missing values with the mean
fed.reserve$FFTR[is.na(fed.reserve$FFTR)] <- mean(fed.reserve$FFTR, na.rm = TRUE)
fed.reserve$FFUT[is.na(fed.reserve$FFUT)] <- mean(fed.reserve$FFUT, na.rm = TRUE)
fed.reserve$FFLT[is.na(fed.reserve$FFLT)] <- mean(fed.reserve$FFLT, na.rm = TRUE)
fed.reserve$EFFR[is.na(fed.reserve$EFFR)] <- mean(fed.reserve$EFFR, na.rm = TRUE)
fed.reserve$GDP[is.na(fed.reserve$GDP)] <- mean(fed.reserve$GDP, na.rm = TRUE)
fed.reserve$UR[is.na(fed.reserve$UR)] <- mean(fed.reserve$UR, na.rm = TRUE)
fed.reserve$IR[is.na(fed.reserve$IR)] <- mean(fed.reserve$IR, na.rm = TRUE)

# these next steps create 2 new versions of the dataset

# combine the dates. 
FR.Date <-unite(fed.reserve, Date, Year, Month, Day, sep="-")   # Date is the name of the new column


#Organize Date by Year
ordered <- fed.reserve %>%
  group_by(Year) %>%
  summarise_all(funs(mean),na.rm=TRUE)

#Eliminate Month and Day Variables from Yearly Data
year.only <- ordered[, -c(2:3)]

# CLEANING
#fed.reserve$Year <- as.factor(fed.reserve$Year)
#fed.reserve$Month <- as.factor(fed.reserve$Month)
#fed.reserve$Day <- as.factor(fed.reserve$Day)


# HOW TO SELECT TIME FRAMES
# ex: the great recession December 2007 - June 2009
install.packages("DT")
library(DT)  # Used to create Datatables

GR <- fed.reserve %>%
  unite(Date, Year, Month, Day, sep="-") %>%
  filter(Date>"2007-12-1")%>%
  filter(Date<"2009-6-1")
datatable(GR)


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



# VISUALIZE

# 3D SCATTER PLOT
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(year.only$GDP, year.only$UR, xlab = "GDP", ylab = "Unemplyment Rate")
