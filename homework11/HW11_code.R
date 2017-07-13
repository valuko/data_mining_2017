# Homework 11
# Victor Aluko

library(ggplot2)

setwd("C:/devstore/DM/hw11")

companies2 <- read.csv("companies_2.csv", header = TRUE, sep = ",")
companiesKmean <- kmeans(companies2[,4:6], centers = 5)

companiesKmean$cluster <- as.factor(companiesKmean$cluster)
ggplot(companies2, aes(CompanyName, EmployeeTax, color = companiesKmean$cluster)) + geom_point()

# Exercise 5

companyPca = prcomp(scale(companies2[4:6]))
companyScores = companyPca$x

# Using the first 2 PC
lmCompany = lm(formula = companies2$Employees ~ companyScores[,1:2])

summary(lmCompany)

plot(companies2$Employees, predict(lmCompany))