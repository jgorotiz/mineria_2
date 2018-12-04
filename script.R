library(caret)
library(e1071)
library(forecast)
setwd("C:\\Users\\JONATHAN\\Desktop\\2018-2T\\mineria")
#Load data
train_data = read.csv("cancer_train.csv")
test_data = read.csv("cancer_test.csv")
View(train_data)
View(test_data)

#Data treatment
train_data$radio <- scale(train_data$radio)
train_data$simetria <- scale(train_data$simetria)
test_data$radio <- scale(test_data$radio)
test_data$simetria <- scale(test_data$simetria)
train_data$tipo <- sapply(train_data$tipo,switch,"M" = 1, "B" = 0)
test_data$tipo <- sapply(test_data$tipo,switch,"M" = 1, "B" = 0)

#Logistic regression
mylogit <- glm(tipo ~ radio + simetria, data = train_data, family = "binomial")
summary(mylogit)

#Prediction
train_data$prediction <- predict(mylogit, newdata = train_data, type = "response" )
test_data$prediction  <- predict(mylogit, newdata = test_data , type = "response" )

test_data$cutoff.1 <- ifelse(test_data$prediction>0.1, 1, 0)
test_data$cutoff.5 <- ifelse(test_data$prediction>0.5, 1, 0)
test_data$cutoff.9 <- ifelse(test_data$prediction>0.9, 1, 0)

#Confusion matrix
con.mat <- confusionMatrix(factor(test_data$tipo),factor(test_data$cutoff.1))
con.mat$table
str(con.mat)
con.mat$overall
con.mat$byClass

con.mat <- confusionMatrix(factor(test_data$tipo),factor(test_data$cutoff.5))
con.mat$table
str(con.mat)
con.mat$overall
con.mat$byClass

con.mat <- confusionMatrix(factor(test_data$tipo),factor(test_data$cutoff.9))
con.mat$table
str(con.mat)
con.mat$overall
con.mat$byClass



accuracy(train_data$prediction,train_data$tipo)
accuracy(test_data$prediction,test_data$tipo)
