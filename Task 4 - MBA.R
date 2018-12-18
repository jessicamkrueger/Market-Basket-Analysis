#load packages
library(arules)
library(arulesViz)
library(shiny)
library(pdftools)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(readr)

#upload transaction data
setwd("~/Ubiqum/Project 2/Task 4 - Market Basket Analysis/Original Data Sets")
trans <- read.transactions("ElectronidexTransactions2017.csv", 
                           format = "basket", sep=",", rm.duplicates=TRUE)

#quick summary stats of the matrix
summary(trans)

inspect(trans[1:4]) # You can view the transactions. Is there a way to see a certain # of transactions?
length(trans) # Number of transactions.
transSize <-size(trans) # Number of items per transaction
LIST(trans) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(trans)# To see the item labels

#histogram of # items per transaction
m1 <- matrix(transSize, ncol=1, byrow=TRUE)
d1 <- as.data.frame(m1, stringsAsFactors=FALSE)

ggplot(d1) +
  geom_histogram(aes(x=V1), binwidth = 1, 
                 color = "black", fill = "white") +
  scale_x_continuous(breaks = seq(1, 30, 1)) +
  scale_y_continuous(breaks = seq(0, 2500, 250)) +
  labs(title = "Electronidex Data: Items per Transaction",
       y = "Number of Transactions",
       x = "Number of Items in Transacation")


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

#find items with similar frequencies to compare 
summary(frequencies)
plot(frequencies)
plot(sorted, main = "Electronidex Item Support Distribution", 
     ylab = "Item Frequency", xlab = "Item Number")
#median of item frequency is .017692, let's look at all items around that frequency
similar <- sorted[sorted <.018]
similar <- similar[similar > .017]
similar
#Koss Home Headphones      Rii LED Gaming Keyboard & Mouse Combo 
#0.01708185                                 0.01738688 
#JBL Splashproof Portable Bluetooth Speaker                      Brother Printer Toner 
#0.01759024                                 0.01769192 
#Smart Light Bulb              Microsoft Basic Optical Mouse 
#0.01769192                                 0.01789527 


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
rules1 <- apriori (trans, parameter = list(supp = 0.0, conf = 0.25, minlen = 2))

rules2 <- apriori (trans, parameter = list(supp = 0.001, conf = 0.1, minlen = 2))
summary(rules2)
ruleExplorer(rules2)

#pair down rules using means of rules2 (41000 rules)
rules3 <- apriori (trans, parameter = list(supp = 0.002017, conf = .4302, minlen = 2))
#results in 2090 rules
#show rules sorted by lift
inspect(head(sort(rules3, by = "lift")))
ruleExplorer(rules3)

#add different interest measures to rules
quality(rules) <- cbind(quality(rules), interestMeasure(rules, measure = c("allConfidence", "hyperLift"), trans = trans))


#search for redundant rules
is.redundant(rules3)
#inspect redundant rules
inspect(rules3[is.redundant(rules3)])
#remove redundant rules and view in ruleExplorer
rules4 <- rules3[!is.redundant(rules3)]
ruleExplorer(rules4)

#include only signficant rules
rules5 <- rules4[is.significant(rules4, transactions = trans,  method = "fisher", 
                               alpha = 0.01, adjust = "bonferroni")]
#View rules
ruleExplorer(rules5)

#inspect rules
inspect(rules1) #shows all rules
rules1 #shows number of rules resulting

#plot the rules
plot(rules3)
#two-key plot of rules3
plot(rules3, method = "two-key plot", interactive = TRUE)
#interactive allows for zooming and selecting rules

#plot matrix visualization with a subset of high confidence rules
rules4 <- rules3[quality(rules3)$confidence > 0.5]
plot(rules4, method = "matrix", measure = "lift")
#"matrix3D" makes a 3D chart

#plot grouped rules matrix
plot(rules3, method = "grouped") #interactive = TRUE)

#plot graph of rules
rules5 <- rules3[quality(rules3)$support > .01]
plot(rules5, method = "graph")




#show only rules iwth iMac
iMacRules <- subset(rules1, items %in% "iMac")
inspect(sort(iMacRules, by = "lift")) #sort by list measurement and inspect
iMacRules

#any redundant rules?
is.redundant(rules3) #apparently, no

#import PDF list of products and types
prodList <- pdf_text("ElectronidexItems2017.pdf")
prodList1 <- strsplit(prodList, "\r\n")
head(prodList1)
write.csv(prodList1, file="ProductList.csv", row.names = FALSE)



transCSV <- read.csv("ElectronidexTransactions2017.csv", header = FALSE)
transCSV1 <- data.frame(lapply(transCSV, function(x) {
              gsub("Acer Aspire", "Laptops", x)}))

#item frequency for Acer Aspire
itemFrequency(trans[,"Acer Aspire"])*9835

#what Blackwell products perform best in terms of profit?
setwd("~/Ubiqum/Project 2/Task 3 - Predicting Sales Volume/Original Data Sets")
bwdata <- read.csv2("existing_RelativePice_environmentalImpact_Durability.csv")
bwprofits <- bwdata %>%
              mutate(profit = Profit_margin*Volume*Prices) %>%
              arrange(desc(profit)) %>%
              select(Product_ID, Product_type, profit, Profit_margin, Volume) %>%
              filter(Product_type != "Extended Warranty")

#create list of top 10 profitable BW items
Top10Profit <- bwprofits[1:10,]
Top10Profit$Product_ID <- as.factor(Top10Profit$Product_ID)
Top10Profit <- arrange(Top10Profit, desc(profit))
#sort product Id in order of most profitable
Top10Profit$Product_ID <- reorder(Top10Profit$Product_ID, -Top10Profit$profit)

#create list of top 10 volume BW items
bwVol <- arrange(bwprofits, desc(Volume))
Top10Vol <- bwVol[1:10,]
Top10Vol$Product_ID <- as.factor(Top10Vol$Product_ID)
#sort product Id in order of most profitable
Top10Vol$Product_ID <- reorder(Top10Vol$Product_ID, -Top10Vol$Volume)

#plot top ten volume BW items
ggplot(Top10Vol) +
  geom_col(aes(x=Product_ID, y=Volume, fill=Product_type)) +
  geom_point(aes(x=Product_ID, y = profit)) +
  xlab("Blackwell Product ID") +
  ylab("Sales Volume") +
  labs(title = "Blackwell's Top 10 Volume Items") +
  scale_y_continuous(breaks = seq(0, 350000, 50000))


#plot top ten profit BW items
ggplot(Top10Profit) +
  geom_col(aes(x=Product_ID, y=profit, fill=Product_type)) +
  xlab("Blackwell Product ID") +
  ylab("Profit") +
  labs(title = "Blackwell's 10 Most Profitable Items") +
  scale_y_continuous(breaks = seq(0, 300000, 50000), labels = dollar)

#plot most profitable BW categories
ggplot(bwprofits) +
  geom_col(aes(x=Product_type, y=profit)) +
  xlab("Blackwell Product Type") +
  ylab("Profit") +
  labs(title = "Blackwell's 10 Most Profitable Product Types")

#plot of all RHS items in rules
all_rules <- read.csv("~/Ubiqum/Project 2/Task 4 - Market Basket Analysis/Rules Files/all_rules.csv")
#reorder the RHS to be decreasing in value
all_rules <- within(all_rules, 
                    RHS <- factor(RHS, levels=names(sort(table(RHS), 
                                              decreasing=TRUE))))

all_rules <-  mutate(all_rules, highlight_flag = ifelse("RHS" == "{iMac}", T, F))

all_rules %>%
  mutate(highlight_flag = ifelse(RHS == "{Acer Aspire}" | RHS == "{ASUS Monitor}", T, F)) %>%
  #add highlights to certain bars
  ggplot() +
  geom_bar(aes(RHS, fill = highlight_flag)) +
  scale_y_sqrt() +
  labs(title = "Distribution of All Items on Right-Hand-Side", 
       y = "Number of Rules",
       x = "Right-Hand-Side") +
  theme(legend.position = 'none', 
        text = element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size = 18, face = 'bold'))


#bar chart of EX product types
categories1 <- gather(categories, "Laptops":"Smart_Home_Devices", 
                      key = "Product_Type", value = "Product_Name", na.rm = TRUE)
categories1$Product_Type <- as.factor(categories1$Product_Type)
categories2 <- filter(categories1, Product_Name != "NA")

#reorder to go in decreasing value
categories2 <- within(EX_Product_Categories, 
                    Product_Type <- factor(Product_Type, levels=names(sort(table(Product_Type), 
                                                         decreasing=TRUE))))
ggplot(categories2) +
  geom_bar(aes(Product_Type, fill = Product_Type)) +
  labs(title = "Distribution of Electronidex Products", 
       y = "Number of Products",
       x = "Product Type") +
  scale_y_continuous(breaks = seq(0, 65, 5)) +
  theme(legend.position = 'none', 
        text = element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size = 18, face = 'bold'))

#barchart with similar categories to Blackwell. First need to updated product types
#to match
categories2$Product_Type <- as.character(categories2$Product_Type)

categories2$Product_Type[categories2$Product_Type == "Monitors"] <- "Display"
categories2$Product_Type[categories2$Product_Type == "Desktop"] <- "PC"
categories2$Product_Type[categories2$Product_Type == "Laptops"] <- "Laptop"
categories2$Product_Type[categories2$Product_Type == "Printers"] <- "Printer"
categories2$Product_Type[categories2$Product_Type == "Printer_Ink"] <- "Printer Supplies"
categories2$Product_Type[categories2$Product_Type == "Mice"] <- "Accessories"
categories2$Product_Type[categories2$Product_Type == "Keyboard"] <- "Accessories"
categories2$Product_Type[categories2$Product_Type == "Mouse_Keyboard_Combo"] <- "Accessories"
categories2$Product_Type[categories2$Product_Type == "Computer_Cords"] <- "Accessories"
categories2$Product_Type[categories2$Product_Type == "Computer_Tablets"] <- "Tablet"
categories2$Product_Type[categories2$Product_Type == "Computer_Stands"] <- "Accessories"
categories2$Product_Type[categories2$Product_Type == "Active_Headphones"] <- "Accessories"
categories2$Product_Type[categories2$Product_Type == "Computer_Headphones"] <- "Accessories"

ggplot(categories2) +
  geom_bar(aes(Product_Type, fill = Product_Type)) +
  labs(title = "Distribution of Electronidex Products (Using Blackwell Categories", 
       y = "Number of Products",
       x = "Product Type") +
  scale_y_continuous(breaks = seq(0, 65, 5)) +
  theme(legend.position = 'none', 
        text = element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size = 18, face = 'bold'))
