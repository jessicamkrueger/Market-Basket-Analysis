#load packages
library(arules)
library(arulesViz)
library(shiny)

#upload transaction data
setwd("~/Ubiqum/Project 2/Task 4 - Market Basket Analysis/Original Data Sets")
trans <- read.transactions("ElectronidexTransactions2017.csv", 
                           format = "basket", sep=",", rm.duplicates=TRUE)

#quick summary stats of the matrix
summary(trans)

inspect(trans[1:4]) # You can view the transactions. Is there a way to see a certain # of transactions?
length(trans) # Number of transactions.
size(trans) # Number of items per transaction
LIST(trans) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(trans)# To see the item labels

#plot the 10 most frequent items
mostFreq <- itemFrequencyPlot(trans, topN=20)

#make list of most frequent items
sortedTOP <- sort(frequencies, decreasing = TRUE)
sortedTOP[1:20]

frequencies <- itemFrequency(trans)
sorted <- sort(frequencies, decreasing = FALSE)
sorted[1:10] #10 least frequent products
allunder005 <- sorted[sorted<.01]
allunder005
plot(allunder005)
summary(sorted)
hist(allunder005)

hist(frequencies) #shows frequencies of all items as histogram
summary(frequencies)

#plot items with support over .1
over10 <- itemFrequencyPlot(trans, support=0.01)

#plot the transactions
image(trans) #too much data

#plot random sample of X transactions
image(sample(trans, 20))
image(sample(trans, 150))
image(sample(trans, 700))

#plot transactions that are con
image(trans, ylim = 10)

#create rules
rules1 <- apriori (trans, parameter = list(supp = 0.025, conf = 0.25, minlen = 2))

rules2 <- apriori (trans, parameter = list(supp = 0.001, conf = 0.10, minlen = 2))
summary(rules2)
rules2

#inspect rules
inspect(rules1) #shows all rules
rules1 #shows number of rules resulting

#show only rules iwth iMac
iMacRules <- subset(rules1, items %in% "iMac")
inspect(sort(iMacRules, by = "lift")) #sort by list measurement and inspect
iMacRules

#any redundant rules?
is.redundant(rules1) #apparently, no
