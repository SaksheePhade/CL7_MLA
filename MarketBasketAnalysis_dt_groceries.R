#Name - Sakshee Phade
#Roll No - 43244
#Batch - R10
#Problem Statement - Create Association Rules for the Market Basket Analysis for the given Threshold. (Using R) 

# installing/loading the latest installr package:
#install.packages("installr"); library(installr) # install+load installr

#updateR() # updating R.

# Load the libraries
library('arules')
library('datasets')
library('arulesViz')
library('RColorBrewer')

#install.packages('arules')

#load the dataset
data('Groceries')

summary(Groceries)

#Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries, topN =  20, col=brewer.pal(8,'Pastel2'), type = 'absolute')

#We set the minimum support to 0.001
# We set the minimum confidence of 0.8
rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
View(rules)

#Show the top 5 rules, but only 2 digits 
options(digits = 2)
inspect(rules[1:5])

#Sorting Rules by confidence
rules = sort(rules, by = "confidence", decreasing = TRUE)
rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen = 3))


#removing subset rules
subset.matrix = is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant = colSums(subset.matrix, na.rm = T) >= 1
rules.pruned = rules[!redundant]
rules = rules.pruned



#Create association rules for "whole milk"
rules = apriori(data = Groceries, parameter = list(supp = 0.001, conf = 0.08),
                appearance = list(default = "lhs", rhs = "whole milk"),
                control = list(verbose = F))
rules = sort(rules, decreasing = TRUE, by = "confidence")
inspect(rules[1:5])

rules = apriori(data = Groceries, parameter = list(supp = 0.001, conf = 0.15, minlen = 2),
                appearance = list(default = "rhs", lhs = "whole milk"),
                control = list(verbose = F))
rules = sort(rules, decreasing = TRUE, by = "confidence")
inspect(rules[1:5])

#Visualization
plot(rules, method = "graph",  engine = "htmlwidget")



#Create association rules for "tea"
rules = apriori(data = Groceries, parameter = list(supp = 0.001, conf = 0.08),
                appearance = list(default = "lhs", rhs = "tea"),
                control = list(verbose = F))
rules = sort(rules, decreasing = TRUE, by = "confidence")
inspect(rules[1:5])

rules = apriori(data = Groceries, parameter = list(supp = 0.001, conf = 0.15, minlen = 2),
                appearance = list(default = "rhs", lhs = "tea"),
                control = list(verbose = F))
rules = sort(rules, decreasing = TRUE, by = "confidence")
inspect(rules[1:5])

#Visualization
plot(rules, method = "graph",  engine = "htmlwidget")
