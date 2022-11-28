# Joonas Kolstela, 28.11.2022 14:29, script for datawrangling of chapter 5 human development and gender inequality data.
# Data sources:
# Human development: "https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv"
# Inequality: "https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv"

library(dplyr)

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
           life.Exp = Life.Expectancy.at.Birth,
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
       Percent.Rep.Parliament = Percent.Representation.in.Parliament,
       )

# Adding the female to male ratios of secondary education and labor force participation.
gii <- gii %>% 
  mutate(Edu2.FM.Ratio = Edu2.F / Edu2.M,
         Lab.FM.Ratio = Labo.F / Labo.M)

# Combining the datasets and writing to disc.
human <- inner_join(hd, gii, by = "Country")
dim(human)

write.csv(human, "data/human.csv")




