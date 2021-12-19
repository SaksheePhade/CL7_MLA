#Name - Sakshee Phade
#Roll No - 43244
#Batch - R10
#Problem Statement - Create Association Rules for the Market Basket Analysis for the given Threshold. (Using R) 


library(arules)
library(arulesViz)
library(datasets)

# read data
setwd('/Users/sphade/Documents/College/CL7-MLA1')
dataset <- read.csv('breadbasket.csv')
summary(dataset)

#remove unnecessary columns
AggPosData <- split(dataset$`Item`, dataset$`Transaction`)
Txns <- as(AggPosData, 'transactions')

# item frequency plot
itemFrequencyPlot(Txns, topN=20, type="absolute")

association.rules <- apriori( Txns, parameter = list(supp = 0.001, conf = 0.1, minlen =2,  maxlen = 10 ))
inspect(association.rules[1:10])

shorter.association.rules <- apriori( Txns, parameter = list(supp = 0.001, conf = 0.1, minlen =2,  maxlen = 3 ))
inspect(shorter.association.rules[1:10])

#
bread.association.rules <- apriori( Txns, parameter = list(supp = 0.001, conf = 0.1), appearance = list(rhs = "Bread"))
inspect(bread.association.rules[1:10]) 

top10bread <- head(bread.association.rules, n=10, by = "confidence")
plot(top10bread, method = "graph", engine = "htmlwidget")