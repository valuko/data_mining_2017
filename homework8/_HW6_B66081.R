#
# Homework 8
# Machine Learning II
# Victor Aluko
#
install.packages('RWeka')
install.packages('e1071')
install.packages('rJava')

library(rJava)
library(RWeka)
library(e1071)

setwd("C:/devstore/data_mining/homework8/")
bankData <- read.table("bank-full.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

bankData[,"train"] <- ifelse(runif(nrow(bankData))<0.80,1,0)
bankTrainColNum <- grep("train",names(bankData))
trainBankData <- bankData[bankData$train==1,-bankTrainColNum]
testBankData <- bankData[bankData$train==0,-bankTrainColNum]

NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

j48_weka_model = J48(loan ~ ., data=trainBankData, na.action = NULL)
j48_classifier <- evaluate_Weka_classifier(j48_weka_model, numFolds = 5, class = TRUE)
 
nb_weka_model <- NB(loan ~ ., data=trainBankData, na.action = NULL)
nb_classifier <- evaluate_Weka_classifier(nb_weka_model, numFolds = 5, class = TRUE)

loan_col_num <- grep("loan", bankData)


nb_test_predict <- predict(nb_weka_model ,testBankData[,-1])

j48_test_predict <- predict(j48_weka_model,testBankData[,-1])

