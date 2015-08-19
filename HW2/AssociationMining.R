library(arules)
grocery = read.transactions('./groceries.txt', sep = ',', format = 'basket', rm.duplicates = 1)

head(grocery)

groceryrules <- apriori(grocery,parameter=list(support=.01, confidence=.5, maxlen=4))
inspect(groceryrules)
