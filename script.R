library(caret)
library(e1071)
library(forecast)
setwd("C:\\Users\\JONATHAN\\Desktop\\2018-2T\\mineria")
#Load data
train = read.csv("cancer_train.csv")
test = read.csv("cancer_test.csv")
View(train)
View(test)

#Data treatment
train$radio <- scale(train$radio)
train$simetria <- scale(train$simetria)
test$radio <- scale(test$radio)
test$simetria <- scale(test$simetria)
train$tipo <- sapply(train$tipo,switch,"M" = 1, "B" = 0)
test$tipo <- sapply(test$tipo,switch,"M" = 1, "B" = 0)

#Logistic regression
mylogit <- glm(tipo ~ radio + simetria, data = train, family = "binomial")
summary(mylogit)

#Prediction
train$prediction <- predict(mylogit, newdata = train, type = "response" )
test$prediction  <- predict(mylogit, newdata = test , type = "response" )

#Apply cutoffs
test$cutoff.1 <- ifelse(test$prediction>0.1, 1, 0)
test$cutoff.5 <- ifelse(test$prediction>0.5, 1, 0)
test$cutoff.9 <- ifelse(test$prediction>0.9, 1, 0)

#Confusion matrix
#Cutoff 0.1
con.mat <- confusionMatrix(factor(test$tipo),factor(test$cutoff.1))
con.mat$table
str(con.mat)
con.mat$overall
con.mat$byClass

#Cutoff 0.5
con.mat <- confusionMatrix(factor(test$tipo),factor(test$cutoff.5))
con.mat$table
str(con.mat)
con.mat$overall
con.mat$byClass

#Cutoff 0.9
con.mat <- confusionMatrix(factor(test$tipo),factor(test$cutoff.9))
con.mat$table
str(con.mat)
con.mat$overall
con.mat$byClass

#Train and test errors
accuracy(train$prediction,train$tipo)
accuracy(test$prediction,test$tipo)
