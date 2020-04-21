library(tidyverse)
library(caret)
library(dplyr)
library(tidyselect)
#library(funModeling)
library(zoo)
library(randomForest)

library(corrplot)

# read train data
data = read.csv(file = "data/datasets_join/train_innerjoin.csv")

# factorize dataset features 
data_factorized <- data %>% mutate(isFraud = as.factor(ifelse(isFraud == 1, "Yes", "No")))
data_factorized <- data_factorized %>% mutate_at(c("ProductCD", "P_emaildomain", "R_emaildomain", "DeviceType", "DeviceInfo"), factor)
data_factorized <- data_factorized %>% mutate_at(c('addr1','addr2'), factor)
data_factorized <- data_factorized %>% mutate_at(vars_select(names(data_factorized), starts_with('M',)), factor)
data_factorized <- data_factorized %>% mutate_at(vars_select(names(data_factorized), starts_with('card',)), factor)
data_factorized <- data_factorized %>% mutate_at(vars_select(names(data_factorized), 
                                                             matches("id_(1[2-9]|2[0-9]|3[0-8])")), factor)
data_factorized <- data_factorized %>% select(-TransactionID)

# delete features with NAs 
my_data_status=df_status(data_factorized)  
vars_to_remove=subset(my_data_status, my_data_status$p_na > 40)  
data_factorized = data_factorized[,!(names(data_factorized) %in% vars_to_remove[,"variable"])]

# delete features with high dispersity
my_data_status=df_status(data_factorized) 
vars_to_remove=subset(my_data_status, my_data_status$unique > (0.7 * nrow(data_factorized)))
data_factorized = data_factorized[,!(names(data_factorized) %in% vars_to_remove[,"variable"])]

# delete features with low dispersity -> important to correlation matrix
my_data_status = df_status(data_factorized)
vars_to_remove=subset(my_data_status, 
                      my_data_status$unique < (0.01 * nrow(data_factorized)) & 
                         (my_data_status$type == "numeric" | my_data_status$type == "integer"))
data_factorized = data_factorized[,!(names(data_factorized) %in% vars_to_remove[,"variable"])]

# delete factor features with high dispersity
my_data_status=df_status(data_factorized) 
vars_to_remove=subset(my_data_status, (my_data_status$unique > 20 & my_data_status$type == "factor") )
data_factorized = data_factorized[,!(names(data_factorized) %in% vars_to_remove[,"variable"])]


# delete features with Zeros 
my_data_status=df_status(data_factorized)  
vars_to_remove=subset(my_data_status, my_data_status$p_zeros > 90)  
data_factorized = data_factorized[,!(names(data_factorized) %in% vars_to_remove[,"variable"])]


# Replace NA's values 
data_replaced <-data_factorized %>% mutate_if(is.numeric,
                                              na.aggregate) %>% mutate_if(is.factor,
                                                                                     fct_explicit_na, na_level = "Unknown")


# delete high correlated features
c_matrix <- data_replaced %>% select_if(is.numeric) %>% cor(.)
hc = findCorrelation(c_matrix, cutoff=0.8)
#corrplot(c_matrix, method="circle")
hc = sort(hc)
reduced_data = data_replaced[,-c(hc)]





# Random Forest: select important features using random forest 

f_data.rf <- randomForest(x = reduced_data[,2:31], 
             y = reduced_data$isFraud, 
             importance = TRUE,
             do.trace=TRUE,
             ntree=100)

features.imp <- importance(f_data.rf)
features = features.imp[order(features.imp[,'MeanDecreaseAccuracy'], decreasing = T), ]
features = rownames(features)





