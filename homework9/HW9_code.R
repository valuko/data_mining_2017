

install.packages('RWeka')
install.packages('e1071')
install.packages('rJava')
install.packages('randomForest')
install.packages('plotrix')

library(rJava)
library(RWeka)
library(e1071)
library(randomForest)
library(plotrix)

## Exercise 3
distance_calc <- function(x, y, xc=5, yc=5) {
  sqrt(((x - xc)^2) + ((y-yc)^2))
}

distance_score <- function (x, y, radius=3) {
  distance <- distance_calc(x, y)
  distance - radius
}

## Exercise 4-5
generatePoints <- function(num_of_points, minVal=0, maxVal=10) {
  x_list <- round(runif(num_of_points, minVal, maxVal), digits = 1)
  y_list <- round(runif(num_of_points, minVal, maxVal), digits = 1)
  return(data.frame(x=x_list, y=y_list))
}

labelPoints <- function(rw) {
  d_score = distance_score(rw[1], rw[2])
  label <- ifelse(d_score > 0, "negative", "positive")
  #color <- ifelse(d_score > 0, "red", "blue")
  return(data.frame(x=rw[1], y=rw[2], score=d_score, label=label))
}

getTestData <- function() {
  cnt = 1
  datalist = list()
  for (i in seq(0.5, 10, by = 0.5)) {
    for (j in seq(0.5, 10, by = 0.5)) {
      d_score = distance_score(i, j)
      dat <- data.frame(x = i, y = j, score = d_score) 
      cnt = cnt + 1
      datalist[[cnt]] <- dat
    }
  }
  
  test_data <- do.call(rbind, datalist)
}

trainAndTestJ48 <- function(train_data) {
  # Train the classifier
  j48_model <- J48(label~., data=train_data)
  j48_classifier <- evaluate_Weka_classifier(j48_model, numFolds = 5, class = TRUE)
  test_data <- getTestData()
  j48_test_predict <- predict(j48_model, test_data)
  test_data[,'label'] = j48_test_predict
  return(test_data)
}

trainAndTestRF <- function(train_data) {
  rf_model <- randomForest(label ~ ., data=train_data)
  test_data <- getTestData()
  rf_test_predict <- predict(rf_model, test_data)
  test_data[,'label'] = rf_test_predict
  return(test_data)
}

trainAndTestSvmPoly <- function(train_data) {
  svm_model <- svm(label ~ ., data=train_data, kernel="polynomial",gama=1,cost=1)
  test_data <- getTestData()
  svm_test_predict <- predict(svm_model, test_data)
  test_data[,'label'] = svm_test_predict
  return(test_data)
}

trainAndTestSvmLinear <- function(train_data) {
  svm_model <- svm(label ~ ., data=train_data, kernel="linear")
  test_data <- getTestData()
  svm_test_predict <- predict(svm_model, test_data)
  test_data[,'label'] = svm_test_predict
  return(test_data)
}

points_20 <- generatePoints(20)
points_50 <- generatePoints(50)
points_100 <- generatePoints(100)

labelled_points_20 <- do.call(rbind, apply(points_20, 1, labelPoints))
labelled_points_50 <- do.call(rbind, apply(points_50, 1, labelPoints))
labelled_points_100 <- do.call(rbind, apply(points_100, 1, labelPoints))

j48_label_points_20 = trainAndTestJ48(labelled_points_20)
j48_label_points_50 = trainAndTestJ48(labelled_points_50)
j48_label_points_100 = trainAndTestJ48(labelled_points_100)

rf_label_points_20 = trainAndTestRF(labelled_points_20)
rf_label_points_50 = trainAndTestRF(labelled_points_50)
rf_label_points_100 = trainAndTestRF(labelled_points_100)

svm_poly_label_points_20 = trainAndTestSvmPoly(labelled_points_20)
svm_poly_label_points_50 = trainAndTestSvmPoly(labelled_points_50)
svm_poly_label_points_100 = trainAndTestSvmPoly(labelled_points_100)

svm_linear_label_points_20 = trainAndTestSvmLinear(labelled_points_20)
svm_linear_label_points_50 = trainAndTestSvmLinear(labelled_points_50)
svm_linear_label_points_100 = trainAndTestSvmLinear(labelled_points_100)

# after specifying custom palette
palette(c("red","blue"))
plot(j48_label_points_20$x, j48_label_points_20$y, col=j48_label_points_20$label, pch=19, xlab = "X value", ylab = "Y value")


