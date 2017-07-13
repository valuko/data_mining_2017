# Data Mining Homework 1 solution in R

# Read the data from the abalone csv file
abalone = read.csv("abalone.csv")
# Now read the column headers
colnames(abalone)

# Read the number of rows
nrow(abalone)

# Print the first 4 lines
abalone[1:4,]

# Print the values of the Feature Rings observations
abalone$Rings[1:4]

# Extract the last 3 rows of the data frame
uppr = nrow(abalone)-2
lowr = nrow(abalone)
last_3_rows = abalone[uppr:lowr,]

# Weight of the last 3 abalones
last_3_rows$Weight

# the value of diameter in the row 755?
abalone[755,]$Diameter

# Number of missing values in the height column
nrow(abalone[!complete.cases(abalone$Height),])

# the mean of the height column
mean(abalone$Height[complete.cases(abalone$Height)])

# Extract the subset of rows of the data frame where gender is M and weight values are below 0.75
subset1 = subset(abalone, Gender == 'M' & Weight < 0.75)

# the mean of diameter in this subset
mean(subset1$Diameter)

# the most frequent rings value
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(abalone$Rings)
# getmode() cited from [TutorialsPoint](https://www.tutorialspoint.com/r/r_mean_median_mode.htm)

# the minimum of length when rings is equal to 18
subset2 = subset(abalone, Rings == 18)
min(subset2$Height, na.rm = T)


