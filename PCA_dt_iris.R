library(datasets)
data("iris")
str(iris)

summary(iris)

#We partition the data set into training and testing data sets

library(caret)
set.seed(100)
ind <- createDataPartition(iris$Species,p=0.80,list = F)
train <- iris[ind,]
test <- iris[-ind,]

dim(train)
dim(test)

#We explore the data set by means of scatter plots and correlation coefficients.
library(psych)
pairs.panels(train[,-5],gap=0,bg=c("red","blue","yellow")[train$Species],
             pch=21)

pc <- prcomp(train[,-5],center = T,scale. = T)
pc

summary(pc)

pred <- predict(pc,train)
train_1 <- data.frame(pred,train[5])
pred1 <- predict(pc,test)
test_1 <- data.frame(pred1,test[5])

library(nnet)
set.seed(100)
mymodel <- multinom(Species~PC1 +PC2,data = train_1)

summary(mymodel)
library(caret)
prd <- predict(mymodel,train_1)
confusionMatrix(prd,train_1$Species)

prt <- predict(mymodel,test_1)
confusionMatrix(prt,test_1$Species)