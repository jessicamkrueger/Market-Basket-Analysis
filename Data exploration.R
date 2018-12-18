#import dataset

library(arules)
library(arulesViz)

trans <- read.transactions("transactions.csv", format = "basket", rm.duplicates = TRUE, sep = ",", cols = 2)

inspect(trans)
length(trans)
size(trans)

rules <- apriori(trans, parameter = list(support=0.001))

rules_by_lift <- sort(rules, by = "lift")
inspect(rules_by_lift)
rules_by_lift
plot(rules, measure = c("support", "lift"), shading = "confidence")


#frequent items - to sattisfy the minimum support threshold
itemFrequencyPlot(trans, support = 0.01)
summary(trans)


#rule generation - exctracting the rules with the highest confidence from the frequent items
rules <- apriori(trans, parameter = list(support=0.0015), )

##ploting rules ()
rules_by_lift <- sort(rules, by = "lift")
inspect(rules_by_lift)
rules_by_lift
plot(rules, measure = c("support", "lift"), shading = "confidence")
summary(rules_by_lift)

##checking for redundant rules
is.redundant(rules)



