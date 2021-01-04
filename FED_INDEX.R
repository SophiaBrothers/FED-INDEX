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
# 'year', 'month', 'day' could be factors independently or as a date

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


# Too much missing data to just remove. perhaps drop variables 'FFUT' AND 'FFLT'
# replace the others with the mean numbers?

# drop 'FFUT' AND 'FFLT'
omitted <- c("FFUT", "FFLT")
fed.reduced <- fed.reserve[ ,!(names(fed.reserve) %in% omitted)]
names(fed.reduced)

# recode missing values with the mean
fed.reduced$FFTR[is.na(fed.reduced$FFTR)] <- mean(fed.reduced$FFTR, na.rm = TRUE)
fed.reduced$EFFR[is.na(fed.reduced$EFFR)] <- mean(fed.reduced$EFFR, na.rm = TRUE)
fed.reduced$GDP[is.na(fed.reduced$GDP)] <- mean(fed.reduced$GDP, na.rm = TRUE)
fed.reduced$UR[is.na(fed.reduced$UR)] <- mean(fed.reduced$UR, na.rm = TRUE)
fed.reduced$IR[is.na(fed.reduced$IR)] <- mean(fed.reduced$IR, na.rm = TRUE)

# Now recheck if missing values
is.na(fed.reduced)


# these next steps create 3 new versions of the dataset

# combine the dates. 
FR.Date <-unite(fed.reduced, Date, Year, Month, Day, sep="-")   # Date is the name of the new column


#Organize Date by Year
FR.Ordered <- fed.reduced %>%
  group_by(Year) %>%
  summarise_all(funs(mean),na.rm=TRUE)

# convert month and day back to integers
FR.Ordered$Month <- as.integer(FR.Ordered$Month)
FR.Ordered$Day <- as.integer(FR.Ordered$Day)

#Eliminate Month and Day Variables from ordered Data
FR.Year <- FR.Ordered[, -c(2:3)]

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



# VISUALIZE

# CORRELATION  
# (>0.7 Indicates multicollinearity.)

# PLOTTING WITH GGPAIRS----
install.packages("GGally")
library(GGally)
ggpairs(FR.Year[,])   # all rows and all columns

# strong correlaton between IR and EFFR

# PLOTTING WITH CORRPLOT----
library(corrplot)
correlations <- cor(FR.Year[,])
corrplot(correlations, method = "circle")

install.packages("hrbrthemes")
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(streamgraph)
library(viridis)
library(DT)
library(plotly)

#just to check the months
month.factor <- as.factor(FR.Ordered$Month)
levels(month.factor)

# LINE CHART
ggplot(FR.Ordered, aes(x=Year, y= GDP, group= as.factor(Month), color=as.factor(Month))) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("A spaghetti chart of GDP") +
  theme_ipsum()


# BOXPLOT
FR.Ordered %>%
  ggplot( aes(x=IR, y=as.factor(Month), fill=as.factor(Month))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("GDP Boxplot") +
  xlab("")


# Create a color palette
install.packages("paletteer")
library(paletteer)
# All the palettes can be accessed from the 3 functions paletteer_c(), paletteer_d() and
# paletteer_dynamic() using the by using the syntax packagename::palettename.
# https://cran.r-project.org/web/packages/paletteer/readme/README.html

nColor <- 20
colors <- paletteer_c("viridis::inferno", n = nColor)

# Transform the numeric variable in bins
rank <- as.factor( as.numeric( cut(FR.Ordered$IR, nColor)))

# Scatter plot with color gradient
plot(
  x = FR.Ordered$EFFR, 
  y = FR.Ordered$IR,
  bg = colors[ rank ],
  cex = 3,
  pch=21
)



# Calculate Outlier Scores
install.packages("DMwR")
library(DMwR)
# remove categorical columns if any
# reserve.num <- fed.reserve[,4:10]  # would do this if I were using the original dataset
outlier.scores <- lofactor(FR.Year, k=5)
plot(density(outlier.scores))

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)




# 3D SCATTER PLOT
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(FR.Year$GDP, FR.Year$UR, xlab = "GDP", ylab = "Unemplyment Rate")
