# Joonas Kolstela, 17.11.2022 18:53, script for datawrangling and analysis of the UCI machine learning student performance data.
# Data source: https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(dplyr)
library(ggplot2)
library(GGally)

setwd("D:/R/IODS-project/")

# reading the data
student_mat <- read.csv("data/student-mat.csv", sep = ";")
student_por <- read.csv("data/student-por.csv", sep = ";")

# check structure and dimensions
str(student_mat) # 395 rows, 33 columns containing character and integer variables
str(student_por) # 649 rows, 33 columns containing character and integer variables

# join the data sets without "failures", "paid", "absences", "G1", "G2", "G3" 
# keep only students found in both data sets

free_cols <- c("failures","paid", "absences", "G1", "G2", "G3")
join_cols <- setdiff(colnames(student_por), free_cols)

# join the two data sets by the selected identifiers
math_por <- inner_join(student_mat, student_por, by = join_cols)
head(math_por)
glimpse(math_por)

alc <- select(math_por, all_of(join_cols))
for(col_name in free_cols) {
  # select two columns from 'math_por' with the same original name
  two_cols <- select(math_por, starts_with(col_name))
  # select the first column vector of those two columns
  first_col <- select(two_cols, 1)[[1]]
  
  # then, enter the if-else structure!
  # if that first column vector is numeric...
  if(is.numeric(first_col)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[col_name] <- round(rowMeans(two_cols))
  } else { # else (if the first column vector was not numeric)...
    # add the first column vector to the alc data frame
    alc[col_name] <- two_cols[1]
  }
}

head(alc)


alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# Initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use))

# Define the plot as a bar plot and draw it
g1 + geom_bar()

# Define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# Initialize a plot of 'high_use'
g2 <- ggplot(data = alc, aes(x = sex, y = high_use))
g2 + geom_bar(stat = "identity")

str(alc)

write.csv(alc, "data/alc.csv", row.names = FALSE)

################################################################################

# Analysis

alc <- read.csv("data/alc.csv")

head(alc)

print(colnames(alc))

# 1 - G3 + sex -> men drink more, worse scores
# 2 - famrel + sex -> quality of family relationship -> good relationships limits drinking
# 3 - goout + sex -> going out with friends -> going out to bars etc.
# 4 - Medu + sex -> higher education could limit drinking in both parents and children

p <- ggpairs(valtocheck, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
print(p)

# Correlation for everything else except mothers level of education.

colnames(alc)

# High use and gender.
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))
g1 + geom_boxplot()

# Students who did not have high use had a better final grade than those who did.
# However, men who had high use had had worse final grades than any other group.
# I find it quite interesting that the effect of high alcohol use in women to their final grade is much lower than what it is on men.

# High use and family relation.
g1 <- ggplot(alc, aes(x = high_use, y = famrel, col = sex))
g1 + geom_boxplot() + ylab("family relation")

# Better family relations seem to protect from higher alcohol use as suspected.
# The importance seems a bit higher for men than women.

# High use and going out.
g1 <- ggplot(alc, aes(x = high_use, y = goout, col = sex))
g1 + geom_boxplot() + ylab("going out")

# As suspected, people who go out more have a higher chance of having high use.
# However, this is not so clear for women. Unlike men, women who go out more do not necessarily have high use.

# High use and mothers education.
g1 <- ggplot(alc, aes(x = high_use, y = Medu, col = sex))
g1 + geom_boxplot() + ylab("mothers education")

# Mothers education does not seem to have any major effect on the high use.
# Effects are almost identical between men and women.
# My hypothesis seems to have been wrong.

# Had to compare to effect of fathers level of education:
g1 <- ggplot(alc, aes(x = high_use, y = Fedu, col = sex))
g1 + geom_boxplot() + ylab("fathers education")

# The fathers level of education has a much higher variance compared to the mothers.
# This seems to mostly have an effect on women, for men the effect is not as clear.


m <- glm(high_use ~ sex+famrel+goout+Medu+Fedu, data = alc, family = "binomial")
coefficients(m)
summary(m)

# 1 - Yes, this was statistifically significant (P-score of 8.89e-05).
# 2 - Yes, also significant (P-score of 0.00166).
# 3 - Yes, also significant (P-score of 6.06e-11).
# 4 - Not significant (P-score of 0.35346), neither is the fathers level of education (P-score of 0.68414).
# Other than the mothers level of education, my hyphothesis was correct.
# I would have thought that the mothers education level would have been a bit more significant.

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m)

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

# Being male or going out more increase the risk, while family relation and mothers education lower it.
# Being male results in ~2.7 times more likely to having high consumption.
# Going out more results in ~2.23 times more likely to having high comsumption.


# Fitting the model with significant variables.
m <- glm(high_use ~ sex+famrel+goout, data = alc, family = "binomial")

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)
plot(table(high_use = alc$high_use, prediction = alc$prediction))
# The model correctly predicted every case?


loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

# On average, we will get ~22 % of the predictions wrong in the training data

