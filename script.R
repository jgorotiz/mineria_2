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

test_data$cutoff.1 <- ifelse(test_data$prediction > 0.1, 1, 0)
test_data$cutoff.5 <- ifelse(test_data$prediction > 0.5, 1, 0)
test_data$cutoff.9 <- ifelse(test_data$prediction > 0.9, 1, 0)

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





#tema 2
library(ggplot2)
library(reshape)
accuracy.vector <- c()
sensitivity.vector <- c()
specificity.vector <- c()
cutoff.vector <- c()

for (cutoff in 1:99){
  cutoff_real <- cutoff/100
  cutoff.vector <- c( cutoff.vector, cutoff_real )
  fitted.results.temp <- ifelse(test_data$prediction > cutoff_real ,1,0)
  misClasificError.temp <- mean(fitted.results.temp != test_data$tipo)
  accuracy.vector <- c(accuracy.vector, 1 - misClasificError.temp)
  u <- union(fitted.results.temp, test_data$tipo)
  confMatrix.temp <- table(factor(fitted.results.temp, u),factor(test_data$tipo, u))
  sensitivity.temp <- sensitivity(confMatrix.temp)
  specificity.temp <- specificity(confMatrix.temp)
  sensitivity.vector <- c(sensitivity.vector, sensitivity.temp)
  specificity.vector <- c(specificity.vector, specificity.temp)
}


table <- data.frame("Acc" =  accuracy.vector, "Sensitivity" = sensitivity.vector , "Specificity"= specificity.vector)
row.names(table) <- cutoff.vector


graph <- melt(table, id.vars = 0, variable_name = 'Metric')
graph$cutoff <- as.numeric(as.character(row.names(table)))
graph$value <- as.numeric(as.character(graph$value))
print(graph)



ggplot(data = graph, aes(x = as.numeric(cutoff), y = as.numeric(value), colour = Metric)) +
  ggtitle("Cutoff Comparison") +
  geom_line(aes(colour = Metric)) +
  ylab("Value (0-1)")+xlab("Cutoff") + 
  theme(panel.spacing = unit(1.5, "lines")) +
  theme(plot.title = element_text(size=15), text = element_text(size=11),legend.text=element_text(size=11), axis.title.x=element_text(size=12), axis.title.y =element_text(size=12))




#tema 2 b
library(pROC)
rocA1C <- with(telde,roc(DM,A1C))
plot(rocA1C, col="red", print.auc=TRUE)
