library(tidyverse)
library(arules)  
library(arulesViz)

# Reading in as a sparse matrix
grocery_list <- read.transactions("C:\\Users\\dhruv\\Desktop\\James Predictive Modeling\\STA380\\exam_data\\groceries.txt", sep=',')
summary(grocery_list)

# Looking at items in first 5 transactions
inspect(grocery_list[1:5])
# Checking frequencies of first 4 items
itemFrequency(grocery_list[, 1:4])
# Visualizing item frequencies of items that have atleast 6% support
itemFrequencyPlot(grocery_list, support = 0.06)
itemFrequencyPlot(grocery_list, topN = 25)

# Running the apriori rule
rules <- apriori(grocery_list, parameter = list(support = 0.003, confidence = 0.25, minlen = 2))
summary(rules)


inspect(rules[1:10])


inspect(sort(rules, by = 'lift')[1:20])

int.rules <- subset(rules, subset= lift > 4.5)

# inspecting rules for 'ham'
ham.rules <- subset(rules, items %in% 'ham')
inspect(ham.rules)

plot(int.rules, method="graph", control=list(type="items"))

plotly_arules(rules)
plotly_arules(rules, measure = c("support", "lift"), shading = "confidence")
plotly_arules(rules, method = "two-key plot")
# add jitter, change color and markers and add a title
plotly_arules(rules, jitter = 10,
              marker = list(opacity = .7, size = 10, symbol = 1),
              colors = c("blue", "green"))

plot(rules, method = "two-key plot")

saveAsGraph(sort(int.rules, by = 'lift')[1:19], file = 'groceryGraph20.graphml')
