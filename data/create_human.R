# Joonas Kolstela, 28.11.2022 14:29, script for datawrangling of chapter 5 human development and gender inequality data.
# Data sources:
# Human development: "https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv"
# Inequality: "https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv"

library(dplyr)
library(stringr)
library(tidyr)

# Reading the data in.
hd <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv")
gii <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv", na = "..")

# Exploring the datasets.
str(hd)
str(gii)
dim(hd)
dim(gii)

# Renaming columns
colnames(hd)
hd <- rename(hd, GNI = Gross.National.Income..GNI..per.Capita,
           Life.Exp = Life.Expectancy.at.Birth,
           Edu.Exp = Expected.Years.of.Education,
           HDI = Human.Development.Index..HDI.,
           Edu.mean = Mean.Years.of.Education,
           GNI.Minus.HDI = GNI.per.Capita.Rank.Minus.HDI.Rank)

colnames(gii)
gii <- rename(gii, Mat.Mor = Maternal.Mortality.Ratio,
       Ado.Birth = Adolescent.Birth.Rate,
       GII = Gender.Inequality.Index..GII.,
       Mat.Mor = Maternal.Mortality.Ratio,
       Edu2.F = Population.with.Secondary.Education..Female.,
       Edu2.M = Population.with.Secondary.Education..Male.,
       Labo.F = Labour.Force.Participation.Rate..Female.,
       Labo.M = Labour.Force.Participation.Rate..Male.,
       Parli.F = Percent.Representation.in.Parliament,
       )

# Adding the female to male ratios of secondary education and labor force participation.
gii <- gii %>% 
  mutate(Edu2.FM = Edu2.F / Edu2.M,
         Labo.FM = Labo.F / Labo.M)

# Combining the datasets and writing to disc.
human <- inner_join(hd, gii, by = "Country")
dim(human)

write.csv(human, "data/human.csv", row.names = FALSE)

# CHAPTER 5 data wrangling

# Reading the dataset.
human <- read.csv("data/human.csv")

# The human dataset is part of the United Nations Development Programme.
# It contains variables related to the Human Development Index (HDI).
# The dataset consists of 195 rows and 19 columns.
# The shortened variable names are are:
# GNI = Gross national income per capita
# Life.Exp = Life expectancy at birth
# Edu.Exp = Expected years of education
# HDI = Human development index
# Edu.mean = Mean years of education
# GNI.Minus.HDI = GNI per capita rank minus HDI rank
# Mat.Mor = Maternal mortality ratio
# Ado.Birth = Adolescent birth rate
# GII = Gender Inequality Index
# Mat.Mor = Maternal mortality ratio
# Edu2.F = Female population with secondary education
# Edu2.M = Male population with secondary education
# Labo.F = Female labour force participation rate
# Labo.M = Male labour force participation rate
# Parli.F = Proportion of females in parliament.
# Edu2.FM = Female to male ratio of secondary education
# Labo.FM = Female to male labour force participation rate

# Changing the GNI variable to numeric

human$GNI <- str_replace_all(human$GNI,",",".")

# Changing the GNI variable to numeric
human <- human %>% 
  mutate(GNI = as.numeric(GNI))

is.numeric(human$GNI)

# Dropping unecessary variables
human <- human %>% select("Country","Edu2.FM","Labo.FM","Edu.Exp","Life.Exp","GNI","Mat.Mor","Ado.Birth","Parli.F")


# Removing rows with missing values
human <- drop_na(human)

# Removing rows relating to regions and not countries
human$Country
human <- human %>% filter(!grepl("Arab States|East Asia and the Pacific|Europe and Central Asia|Latin America and the Caribbean|South Asia|Sub-Saharan Africa|World",Country))


# Defining rownames by courty names and dropping the country name column
dim(human)
row.names(human) <- human$Country
human <- select(human,-Country)

write.csv(human,"data/human.csv", row.names = FALSE)
human <- read.csv("data/human.csv")
dim(human)
