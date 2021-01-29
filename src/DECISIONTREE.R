rm(list=ls()) # clear the list of objects
graphics.off() # clear the list of graphs
options(digits = 3) # number of digits to display
options(scipen=999)

# Directorio 

#install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
#install.packages("MLmetrics")
library(MLmetrics)
#install.packages("e1071")
library(e1071)
library(rpart.plot)
library(rpart)

data <- read.csv("../data/calidad_vino.csv",sep=";")
set.seed(3033)
intrain <- createDataPartition(y = data$quality, p= 0.7, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

anyNA(data)

summary(data)

training[["quality"]]=as.factor(training[["quality"]])
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) # train control




tree_fit <- train(as.numeric(quality) ~., data = training, method = "rpart",
                  parms=list(split="information"),
                  trControl=trctrl,
                  tuneLength = 10)
tree_fit
prp(tree_fit$finalModel, box.palette = "Reds", tweak = 1.2)


my_tree <- rpart(quality ~ .,data=training,method="class")
my_tree


## Reportes

printcp(my_tree) # display the results 
plotcp(my_tree) # visualize cross-validation results 
summary(my_tree) # detailed summary of 

# plot tree 
plot(my_tree, uniform=TRUE, 
     main="Classification Tree ")
text(my_tree, use.n=TRUE, all=TRUE, cex=.8)


  #install.packages("rattle")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(my_tree, caption = NULL)



## Métricas de Decision Tree

test_pred2 <- predict(tree_fit, newdata = testing)
test_pred2
testing$quality

## Reporta sensitivity, specifity
confusionMatrix(test_pred2, (testing$quality))

## Reporta Recall, Precision y F1
confusionMatrix(as.factor(round(test_pred2)), factor(testing$quality), mode="prec_recall")

recall(test_pred2, factor(testing$quality))

precision(data = test_pred2,reference = factor(testing$class))

F_meas(test_pred2,factor(testing$class))

