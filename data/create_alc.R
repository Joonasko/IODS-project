# Joonas Kolstela, 17.11.2022 18:53, script for datawrangling and analysis of the UCI machine learning student performance data.
# Data source: https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(dplyr)
library(ggplot2)

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
head(mat_por)
glimpse(mat_por)

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

test <- mean(alc$Dalc, alc$Walc)


alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(data = alc, aes(x = sex, y = high_use))
g2 + geom_bar(stat = "identity")

str(alc)

write.csv(alc, "data/alc.csv")

