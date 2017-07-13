
# Exercise 1
adults = read.csv("adult_refined.csv")

CleanData <- function(rw) {
  gend <- ifelse(!is.na(rw[12]), "Female", ifelse(!is.na(rw[11]), "Male", NA))
  return(data.frame(age=as.numeric(rw[2]),
                    workclass=trimws(as.character(rw[3])),
                    education=trimws(as.character(rw[4])),
                    occupation=trimws(as.character(rw[5])),
                    capital.change=ifelse(as.numeric(rw[6]) > 0, as.numeric(rw[6]), ifelse(as.numeric(rw[7]) > 0, (-1 * as.numeric(rw[7])), 0)),
                    native.country=trimws(as.character(rw[8])),
                    salaries=as.numeric(rw[9]),
                    jobsatisfaction=as.numeric(rw[10]),
                    gender=as.character(gend)
  ))
}
refined_adults <- do.call(rbind, apply(adults, 1, CleanData))



# Exercise 2
# Age
#  Mean
mean(refined_adults$age[complete.cases(refined_adults$age)])
#  Median
median(refined_adults$age[complete.cases(refined_adults$age)])
#  Max
max(refined_adults$age[complete.cases(refined_adults$age)])
#  Min
min(refined_adults$age[complete.cases(refined_adults$age)])
#  Missing Records
nrow(refined_adults[!complete.cases(refined_adults$age),])
#  SD
sd(refined_adults[!complete.cases(refined_adults$age),])

# Capital.Change
#  Mean
mean(refined_adults$capital.change)
#  Median
median(refined_adults$capital.change)
#  Max
max(refined_adults$capital.change)
#  Min
min(refined_adults$capital.change)
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$capital.change),])
#  SD
sd(refined_adults$capital.change)


# Salaries
#  Mean
mean(refined_adults$salaries)
#  Median
median(refined_adults$salaries)
#  Max
max(refined_adults$salaries)
#  Min
min(refined_adults$salaries)
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$salaries),])
#  SD
sd(refined_adults$salaries)

# Job Satisfaction
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$jobsatisfaction),])
#  Mean
mean(refined_adults$jobsatisfaction[complete.cases(refined_adults$jobsatisfaction)])
#  Min 
min(refined_adults$jobsatisfaction[complete.cases(refined_adults$jobsatisfaction)])
#  Max
max(refined_adults$jobsatisfaction[complete.cases(refined_adults$jobsatisfaction)])
#  Median
median(refined_adults$jobsatisfaction[complete.cases(refined_adults$jobsatisfaction)])
#  SD
sd(refined_adults$jobsatisfaction[complete.cases(refined_adults$jobsatisfaction)])


# Frequency Tables
# WorkClass
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$workclass),])
#  Frequency table
table(refined_adults$workclass)

# Education
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$education),])
#  Frequency table
table(refined_adults$education)

# Occupation
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$occupation),])
#  Frequency table
table(refined_adults$occupation)

# Native Country
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$native.country),])
#  Frequency table
table(refined_adults$native.country)

# Gender
#  Missing
nrow(refined_adults[!complete.cases(refined_adults$gender),])
#  Frequency table
table(refined_adults$gender)


# Plots

plot(table(refined_adults$occupation), type = "l", xlab = "Occupation", ylab = 'Frequencies', main = 'Occupation Plot')
plot(table(refined_adults$age), type = "l", xlab = "Age", ylab = 'Frequencies', main = 'Age Plot')
plot(table(refined_adults$jobsatisfaction), type = "l", xlab = "Job Satisfaction", ylab = 'Frequencies', main = 'Job Satisfaction Plot')
plot(table(refined_adults$gender), type = "l", xlab = "Gender", ylab = 'Frequencies', main = 'Gender Plot')
plot(table(refined_adults$native.country), type = "p",  xlab = "Native Country", ylab = 'Frequencies', main = 'Native Country Plot')
