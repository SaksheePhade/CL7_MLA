
#install.packages("languageserver")
#setwd("E:/BE/CL7_MLA/CL7-MLA-codes")
icecream = read.csv("IceCreamData.csv")
summary(icecream)
head(icecream)

lmTemp = lm(icecream$Revenue~icecream$Temperature, data = icecream) #Create the linear regression model
lmTemp
summary(lmTemp) #Review the results

plot(icecream, pch = 16, col = "blue") #dataset
abline(lmTemp) #Add a regression line

plot(lmTemp$residuals, pch = 16, col = "red")

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(icecream), 0.8*nrow(icecream))  # row indices for training data
trainingRowIndexsample
trainingData <- icecream[trainingRowIndex, ]  # model training data
testData  <- icecream[-trainingRowIndex, ]   # test data

head(trainingData)
head(testData)

# Build the model on training data
lmMod <- lm(Revenue ~ Temperature, data=trainingData)  # build the model
Pred <- predict(lmMod, testData)  # predict revenue
summary (lmMod)  

actuals_preds <- data.frame(cbind(actuals=testData$Revenue, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 
print(correlation_accuracy)
head(actuals_preds)

#install.packages("DAAG")
library(DAAG)


#install.packages("DAAG")

##cross validation with different number of folds (k value)

out10 <- cv.lm(data = icecream, form.lm = Revenue ~ Temperature, m = 10)
out4 <- cv.lm(data = icecream, form.lm = Revenue ~ Temperature, m = 4)
out3 <- cv.lm(data = icecream, form.lm = Revenue ~ Temperature, m = 3)
out2 <- cv.lm(data = icecream, form.lm = Revenue ~ Temperature, m = 2)

attr(out10, 'ms')
attr(out4, 'ms')
attr(out3, 'ms')
attr(out2, 'ms')

# Create Training and Test data with different sizes

##removed set seed, will produce test and train sets with random sampling and different sizes every time program is run

trainingRowIndex <- sample(1:nrow(icecream), 0.6*nrow(icecream))  # row indices for training data
trainingData <- icecream[trainingRowIndex, ]  # model training data
testData  <- icecream[-trainingRowIndex, ]   # test data
head(trainingData)
head(testData)
# Build the model on training data
lmMod <- lm(Revenue ~ Temperature, data=trainingData)  # build the model
Pred <- predict(lmMod, testData)  # predict revenue
summary (lmMod)  
actuals_preds <- data.frame(cbind(actuals=testData$Revenue, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 
print(correlation_accuracy)
head(actuals_preds)