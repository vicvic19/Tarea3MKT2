rm(list=ls()) # clear the list of objects
graphics.off() # clear the list of graphs
options(digits = 3) # number of digits to display
options(scipen=999)

# Directorio 
setwd("C:/Users/vsald/Desktop/T3MKT")

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

data <- read.csv("calidad_vino.csv",sep=";")
set.seed(3033)
intrain <- createDataPartition(y = data$quality, p= 0.7, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

anyNA(data)

summary(data)

training[["quality"]]=as.factor(training[["quality"]])
data[["quality"]]=as.factor(data[["quality"]])
###



trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # train control
set.seed(3333)
knn_fit <- train(quality ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_fit

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # train control
set.seed(3333)
tree_fit <- train(quality ~., data = data, method = "rpart",
                 parms=list(split="information"),
                 trControl=trctrl,
                 tuneLength = 10)


tree_fit
#install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
prp(tree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

test_pred <- predict(knn_fit, newdata = testing)
test_pred

confusionMatrix(test_pred, factor(testing$quality))
confusionMatrix(test_pred, factor(testing$quality), mode="prec_recall")


############
#recall(test_pred, factor(testing$quality))
#prSummary(training,lev = levels(training$quality))

#recall(test_pred,reference = as.factor(testing$quality))

#precision(data = test_pred,reference = factor(testing$quality),relevant = levels(3,4,5,6,6,7,8))
############################





my_tree <- rpart(quality ~ .,data=data,method="class",parms = list(split="information"))
my_tree
fancyRpartPlot(my_tree, caption = NULL)

######################
########
######## 2 CLASES
########
######################
data$clase <- 1

i <- 1
for ( i in 1:nrow(data)){
  if ( data$quality[i] <= 5) { data$clase[i]=1}
  else if (data$quality[i] >5){data$clase[i]=2}
}



training2 <- data[intrain,]
testing2 <- data[-intrain,]
training2 <- training2[,-12]
testing2 <- testing2[,-12]

## Como factor

training2$clase <- as.factor(training2$clase)
trctrl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # train control
set.seed(3333)
knn_fit2 <- train(clase ~., data = training2, method = "knn",
                 trControl=trctrl2,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_fit2




test_pred2 <- predict(knn_fit2, newdata = testing2)
test_pred2

confusionMatrix(test_pred, factor(testing$quality))
confusionMatrix(test_pred, factor(testing$quality), mode="prec_recall")


recall(test_pred2, factor(testing2$clase))
#prSummary(training2,lev = levels(training2$clase))

#recall(test_pred,reference = as.factor(testing$quality))

precision(data = test_pred2,reference = factor(testing2$clase))

F_meas(test_pred2,factor(testing2$clase))

############################

