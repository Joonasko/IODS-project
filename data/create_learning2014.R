# Joonas Kolstela - 9.11.2022 20:09 - Script for reading and editing the learning2014 dataset

# read packages
library(dplyr)
library(GGally)
library(ggplot2)

# reading the data from the online source
data_in <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", 
                      sep = "\t", header = TRUE)

# this dataset consists of international survey results of Approaches to Learning from the course Introduction to Social Statistics.
# more information about creating further variables can be found in: http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS2-meta.txt


print(dim(data_in))
print(class(data_in))
print(str(data_in))

# the dataset is a data.frame with 183 rows and 60 columns.
# gender column values are character type, others are integer values.

# add the attitude column (Attitude/10)
data_in["attitude"] <- data_in$Attitude/10

# creating the deep/surface/strategic question column name lists
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# creating the deep/surf/stra columns, which contain the row means for the question types
data_in$deep <- rowMeans(select(data_in, one_of(deep_questions)))
data_in$surf <- rowMeans(select(data_in, one_of(surface_questions)))
data_in$stra <- rowMeans(select(data_in, one_of(strategic_questions)))

# creating a list of column names to keep and using it to select data
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(data_in,all_of(keep_columns))

# printing the column names and renaming Age -> age and Points -> points
print(names(learning2014))
colnames(learning2014)[2] <- "age"
learning2014 <- rename(learning2014, points = Points)

# filtering out students with 0 points
learning2014 <- learning2014[learning2014$points > 0,]

# check dimensions = 166 rows 7 columns
print(dim(learning2014))

# set the working directory to the IODS-project dir and save the data to data folder
setwd("D:/R/IODS-project/")
write.csv(learning2014, "data/learning2014.csv", row.names = FALSE)

# read the data back to the R session and check data structure
learning2014 <- read.csv("data/learning2014.csv")
print(str(learning2014))
print(head(learning2014))

# plotting the data

# doing an overview of the data and it's correlations with a matrix plot
pairs(learning2014[-1])
p <- ggpairs(learning2014, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
print(p)

# There where almost double the amount of women compared to men
# Most of the student were under 30 years old
# attitude values resemble a bell curve, having a sharp drop at 4
# most students scored well in the deep question set
# the mean values for strategic questions was around 3
# on average students performed the worst on the surface level questions when compared to the deep and strategic ones
# the amount of points do not seem to be normally distributed. The mean leans more on the higher end of points and there are two areas of interest at the ~12 point mark and ~27 point mark.

# I find a couple of things quite interesting from the data:
# age and attitude have almost no correlation, I would have thought that older students would have been more motivated
# attitude has a strong positive correlation to the points (which makes sence now that I think about it).

# fitting a regression model for points variable
# using the attitude, age and strategic question variables as explanatory variables
my_model <- lm(points ~ attitude + age  + stra , data = learning2014)
summary(my_model)

# attitude has the highest significance, while age has the lowest

# removing age as a variable and trying again
my_model <- lm(points ~ attitude + stra, data = learning2014)
summary(my_model)

# strategic questions are not significant, trying again without them
my_model <- lm(points ~ attitude, data = learning2014)
summary(my_model)

# attitude has an excellent significance of 4.12e-09.
# however, the multiple R-squared has a quite low value of 0.1906
# to my understanding, this may partly be explained by the correlation value of 0.437, which is positive but not really strong
# conclusion: while the attitude has the highest correlation with the points of all of the variables, there are some unfound variables that could explain the variation.

# plotting the residuals vs fitted values, normal Q-Q-plot and Residual vs Leverage
plot(my_model)

# residuals vs fitted values plot thoughts:
# there seems to be 3 outlier values: 35, 56 and 145
# the relationship seems to be linear
# residuals range mainly in between -10 to 10, but the model seems to overestimate the fitted values especially at values over 24

# normal Q-Q-plot
# again the points 35, 56 and 145 are outliers
# the data points are mostly on the y=x line, meaning that the data is most likely normally distributed
# however there is some deviation in the upper right corner of the data (as was seen in the data overview plot)

# residuals vs leverage plot
# this time the outlier points are 35, 56 and 71
# all points are outside Cook's distance, meaning that none of the points are influential












