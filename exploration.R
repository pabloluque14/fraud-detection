library(ggplot2)


# Printing histogram for class feature
ggplot(data_factorized, aes(x=isFraud)) + geom_bar(show.legend = "Histograma")


# Printing boxplot
attach(data_train)
boxplot(TransactionAmt~ isFraud == "Yes", xlab = "Amount without outliers")



ggplot(balanced.train.data, aes(x=isFraud)) + geom_bar(show.legend = "Histograma")
