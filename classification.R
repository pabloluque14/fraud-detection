#library(e1071)
#library(caret)
#library(dplyr)
library(randomForest)
#library(mlbench)
#library(funModeling)
library(DMwR)
library(glmnet)
library(caTools)

data_train = reduced_data[,features[1:20]]
  
data_train$isFraud = reduced_data[,'isFraud']

# delete duplicated rows
data_train %>% distinct()





replace_outliers <- function(x, removeNA = TRUE){
  qrts <- quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
  caps <- quantile(x, probs = c(.05, .95), na.rm = removeNA)
  iqr <- qrts[2]-qrts[1]
  h <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1]
  x[x>qrts[2]+h] <- caps[2]
  x
}



# Remove outliers
outliers <- boxplot(data_train$TransactionAmt ~ data_train$isFraud, plot=FALSE)$out
data_train <- data_train[-which(data_train$TransactionAmt %in% outliers),]


boxplot(data_train$TransactionAmt ~ data_train$isFraud)



d1 <- replace_outliers(data_train[which(data_train$isFraud == 'Yes'), 'TransactionAmt'])

#d2 <- replace_outliers(data_train[which(data_train$isFraud == 'No'), 'TransactionAmt'])


data_train[which(data_train$isFraud == 'Yes'), 'TransactionAmt'] <- d1

#data_train[which(data_train$isFraud == 'No'), 'TransactionAmt'] <- d2


boxplot(data_train$TransactionAmt ~ data_train$isFraud)






# Separarte validation and trainning set
train <- sample(nrow(data_train), 0.7*nrow(data_train), replace = FALSE)
TrainSet <- data_train[train,]
ValidSet <- data_train[-train,]

summary(TrainSet)

# Balance de minority class using SMOTE
balanced.train.data <- SMOTE(isFraud ~., TrainSet, perc.over = 600, perc.under = 150)

summary(balanced.train.data)


# Classification using Random Forest
rf_model <- randomForest(x = balanced.train.data[,1:20], 
                          y = balanced.train.data$isFraud, 
                          importance = TRUE,
                          ntree=200,
                          do.trace = TRUE)

rf.predValid <- predict(rf_model, ValidSet)

rf.error <- mean(rf.predValid == ValidSet$isFraud)                    

rf.table <-table(rf.predValid, ValidSet$isFraud)


# Print results
rf.prec <- precision(data = rf.table, reference = ValidSet$isFraud, relevant = "Yes")
rf.rec <- recall(data = rf.table, reference = ValidSet$isFraud, relevant = "Yes")
rf.fmeans <- F_meas(data = rf.table, reference = ValidSet$isFraud, relevant = "Yes")

print(rf.table)

# Classification using Logistic regresion
grid_lb <- expand.grid(.nIter = 6)

control_lb <- trainControl(method = "repeatedcv", 
                                 number = 11,
                                 repeats = 6) 

model_lb <- train(isFraud ~ ., 
                        data = balanced.train.data, 
                        method = "LogitBoost", 
                        trControl = control_lb, 
                        tuneGrid = grid_lb)

lb.predValid <- predict(model_lb, ValidSet, type = "raw")


lb.error<- mean(lb.predValid == ValidSet$isFraud)                    

lb.table <-  table(lb.predValid, ValidSet$isFraud) 


# Print results
lb.prec <- precision(data = lb.table, reference = ValidSet$isFraud, relevant = "Yes")
lb.rec <- recall(data = lb.table, reference = ValidSet$isFraud, relevant = "Yes")
lb.fmeans <- F_meas(data = lb.table, reference = ValidSet$isFraud, relevant = "Yes")

print(lb.table)

# Kaggle prediction

if (!file.exists("ieee-fraud-detection/test_innerjoin.csv")){

  test_transaction <- read_csv("ieee-fraud-detection/test_transaction.csv") 
  test_identity <- read_csv("ieee-fraud-detection/test_identity.csv")
  test_dataset <- merge(test_transaction, test_identity, by = "TransactionID", all.x = TRUE)
  
  write_csv(test_dataset, "ieee-fraud-detection/test_innerjoin.csv")

} else {
  
  test_dataset <- read_csv("ieee-fraud-detection/test_innerjoin.csv")
  
}


names(test_dataset)[names(test_dataset) == "id-35"] <- "id_35"
names(test_dataset)[names(test_dataset) == "id-12"] <- "id_12"
names(test_dataset)[names(test_dataset) == "id-16"] <- "id_16"
names(test_dataset)[names(test_dataset) == "id-15"] <- "id_15"


rows <- test_dataset$TransactionID

test_dataset <- test_dataset[,features[1:20]]

test_dataset$TransactionID <- rows

test_dataset <- test_dataset %>% mutate_at(vars_select(names(test_dataset), 
                                                       matches("id_(1[2-9]|2[0-9]|3[0-8])")), factor)
test_dataset <- test_dataset %>% mutate_at(c("DeviceType"),factor)
test_dataset <- test_dataset %>% mutate(id_12 = as.factor(ifelse(id_12 == "Unknown", "NotFound", "Found")))
test_dataset <-test_dataset %>% mutate_if(is.numeric,na.aggregate) %>% mutate_if(is.factor,
                                                                                 fct_explicit_na, na_level = "Unknown")



pred <- predict(model_smote, test_dataset)


test_dataset$isFraud <- pred

submission <- read.csv('ieee-fraud-detection/sample_submission.csv')
submission$isFraud <- pred 

submission <- submission %>% mutate(isFraud = as.factor(ifelse(isFraud == "Yes", "1", "0")))

write_csv(submission, "ieee-fraud-detection/sample_submission.csv")



# Separate both dataframes by class 
#trainYes <- data_train[ which(data_train$isFraud =='Yes'), ]

#trainNO <- data_train[ which(data_train$isFraud =='No'), ]
#trainNO <- trainNO[sample(nrow(trainNO), 12000), ]

# Merege the both dataframes by rows
#data_trainFinal <- rbind(trainNO, trainYes)

#my_data_status=df_status(data_trainFinal)  

# Shuffle the final dataframe
#set.seed(4)
#rows <- sample(nrow(data_trainFinal))
#data_trainFinal <- data_trainFinal[rows, ]

