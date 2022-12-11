# Joonas Kolstela - 8.12.2022 - Chapter 6 data wrangling

# Loading packages.
library(tidyr)
library(dplyr)
# Reading the bprs dataset and setting colnames.
bprs <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt")#, sep = "/",col.names = FALSE)#, skip = 1)
my.names <- bprs[1,]
colnames(bprs) <- my.names
bprs <- bprs[-1,]

# Reading the rats dataset and setting colnames.
rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt")

# Writing the datasets on disc.
write.csv(bprs, "data/bprs.csv", row.names = FALSE)
write.csv(rats, "data/rats.csv", row.names = FALSE)

# Re-reading dataset.
bprs <- read.csv("data/bprs.csv")
rats <- read.csv("data/rats.csv")

# Checking data structure and dimensions.
str(bprs)
dim(bprs)

# The bprs data set consists of 40 rows and 11 columns, all of the integer datatype.
# Variables depict the the received treatment, subject numbers and BPRS values for each week.

str(rats)
dim(rats)

# The rats data set consists of 16 rows and 13 columns, all of the integer datatype.
# Variables depict the ID and group numbers of the rats, and finally the WD variables depicting rat weights.

# Changing to categorial variables to factor 
bprs$treatment <- factor(bprs$treatment)
bprs$subject <- factor(bprs$subject)

rats$Group <- factor(rats$Group)
rats$ID <- factor(rats$ID)

# Converting data sets to long form.

bprs_long <- bprs %>% pivot_longer(matches("week0|week1|week2|week3|week4|week5|week6|week7|week8"),
                                names_to = "week",
                                values_to = "bprs")

# Cleaning week variable names and changing to integer.
bprs_long$week <- as.integer(substr(bprs_long$week, 5,6))


rats_long <- rats %>% pivot_longer(matches("WD1|WD8|WD15|WD22|WD29|WD36|WD43|WD44|WD50|WD57|WD64"),
                              names_to = "Time",
                              values_to = "weight")

# Cleaning Time variables and changing to integer.
rats_long$Time <- as.integer(substr(rats_long$Time, 3,4))

# Looking at the data sets and comparing the data sets.

names(bprs)
names(bprs_long)

str(bprs)
str(bprs_long)

glimpse(bprs)
glimpse(bprs_long)

names(rats)
names(rats_long)

str(rats)
str(rats_long)

glimpse(rats)
glimpse(rats_long)

# Write the new long format data.
write.csv(bprs_long, "data/bprs_long.csv",row.names = FALSE)
write.csv(rats_long, "data/rats_long.csv",row.names = FALSE)
