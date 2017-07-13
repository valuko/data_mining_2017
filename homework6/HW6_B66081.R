
# Homeworj 6 submission. 
# Victor Aluko

library(ggplot2)
library(caret, quietly = TRUE)

setwd("C:/devstore/data_mining/homework6/")
xydata <- read.delim("HW6_exercise1.txt")

Calc_funcs <- function(a, b) {
  data.frame(x = a, y = b, 
             yx = a, y15x = 1.5 * a,
             y2x = 2 * a)
}

generated_Data <- do.call(rbind, apply(xydata, 1, function(ab) Calc_funcs(ab['x'], ab['y'])))


plot(generated_Data$x, generated_Data$y2x, type = "p", main = "Plot of functions of x")
points(generated_Data$x, generated_Data$y15x, col=2)
points(generated_Data$x, generated_Data$yx, col=3)

RMSE_yx <- sqrt(mean((generated_Data$yx - generated_Data$y)^2))
RMSE_y15x <- sqrt(mean((generated_Data$y15x - generated_Data$y)^2))
RMSE_y2x <- sqrt(mean((generated_Data$y2x - generated_Data$y)^2))

# Exercise 2 code
winequality <- read.table("winequality.csv", header = TRUE, stringsAsFactors = F, sep = ";")
fit_fa <- lm(formula =  quality ~ fixed.acidity, data = winequality)
summary(fit_fa)

fit_va <- lm(formula =  quality ~ volatile.acidity, data = winequality)
summary(fit_va)

fit_ca <- lm(formula =  quality ~ citric.acid, data = winequality)
summary(fit_ca)

fit_rs <- lm(formula =  quality ~ residual.sugar, data = winequality)
summary(fit_rs)

fit_cl <- lm(formula =  quality ~ chlorides, data = winequality)
summary(fit_cl)

fit_fso <- lm(formula =  quality ~ free.sulfur.dioxide, data = winequality)
summary(fit_fso)

fit_tso <- lm(formula =  quality ~ total.sulfur.dioxide, data = winequality)
summary(fit_tso)

fit_d <- lm(formula =  quality ~ density, data = winequality)
summary(fit_d)

fit_ph <- lm(formula =  quality ~ pH, data = winequality)
summary(fit_ph)

fit_sl <- lm(formula =  quality ~ sulphates, data = winequality)
summary(fit_sl)

fit_al <- lm(formula =  quality ~ alcohol, data = winequality)
summary(fit_al)

# Exercise 3

data("iris")
iris_subset <- subset(iris, Species == "setosa" | Species == "versicolor")
iris_setosa = subset(iris, Species == "setosa")
iris_versicolor = subset(iris, Species == "versicolor")
t_result <- t.test(iris_setosa$Petal.Length, iris_versicolor$Petal.Length, alternative = "greater")

# Manual calculation
setosa_t.value = (mean(iris_setosa$Petal.Length) - 10) / (sd(iris_setosa$Petal.Length) / sqrt(length(iris_setosa$Petal.Length))) 
setosa_p.value = 2*pt(-abs(setosa_t.value), df=length(iris_setosa$Petal.Length)-1) 

versicolor_t.value = (mean(iris_versicolor$Petal.Length) - 10) / (sd(iris_versicolor$Petal.Length) / sqrt(length(iris_versicolor$Petal.Length))) 
versicolor_p.value = 2*pt(-abs(versicolor_t.value), df=length(iris_versicolor$Petal.Length)-1) 

petal_lengths <- rbind(iris_setosa, iris_versicolor)
ggplot(petal_lengths$Petal.Length, aes(length, fill = Petal.Length)) + geom_density(alpha = 0.2)

