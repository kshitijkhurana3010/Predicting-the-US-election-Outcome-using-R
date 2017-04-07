setwd("C:/Users/Kshitij/Desktop/R BIA")
# Importing the libraries 
library("randomForest")
library("ggplot2")
library("caret")

# Importing the dataset 'election_campaign_data.csv'
Data_csv <- read.csv("election_campaign_data.csv", header=T, sep=",",strip.white = T,
                     na.strings = c("NA","","NaN","?",""))
head(Data_csv)
# Dropping the variables
Mod_Data_csv = subset(Data_csv, select = -c(cand_id,last_name,first_name,twitterbirth,
                                            facebookdate, facebookjan, youtubebirth))

# Converting the variables into factor variables 
Mod_Data_csv$twitter <- as.factor(Mod_Data_csv$twitter)
Mod_Data_csv$facebook <- as.factor(Mod_Data_csv$facebook)
Mod_Data_csv$youtube <- as.factor(Mod_Data_csv$youtube)
Mod_Data_csv$cand_ici <- as.factor(Mod_Data_csv$cand_ici)
Mod_Data_csv$gen_election <- as.factor(Mod_Data_csv$gen_election)

# Removing all obs with missing values
cleaned_Data_csv <- Mod_Data_csv[complete.cases(Mod_Data_csv),]

# Calculating the number of rowsin cleaned dataset
n <- nrow(cleaned_Data_csv)

# Segregating the 70% data into training and 30% into testing
trainIndex = sample(1:n, size = round(0.7*n),replace=FALSE)
train_data = cleaned_Data_csv[trainIndex,] 
test_data = cleaned_Data_csv[-trainIndex,] 
summary(train_data)
# Model 1: 
#Random Forest with ntree = 10
rf <-randomForest(gen_election~., data=train_data, ntree=10, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

# Random Forest with ntree = 20
rf <-randomForest(gen_election~., data=train_data, ntree=20, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

# Random Forest with ntree = 30
rf <-randomForest(gen_election~., data=train_data, ntree=30, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

# Tried with different ntree values and the lowest error OOB error is with ntrees =100
rf <-randomForest(gen_election~., data=train_data, ntree=100, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

# Calculating the mtry value 
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=100,  
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, 
               na.action=na.exclude)

# Random forest model with Mtry = 5 and Ntree = 100
rf <-randomForest(gen_election~., data=train_data, ntree=100, mtry=5,na.action=na.exclude, importance=T,
                  proximity=T) 

print(rf)

predicted_values <- predict(rf, test_data,type= "prob")
threshold <- 0.5
pred<-factor(ifelse(predicted_values[,2] > threshold, 'W','L'))
levels(test_data$gen_election)

# Confusion Matrix for the Random Forest Model with Mtry=5 and Ntree=100
confusionMatrix(pred, test_data$gen_election, 
                positive = 'W')
# ROC curve for Random Forest Model
library(ROCR)
library(ggplot2)
predicted_values <- predict(rf, test_data,type= "prob")[,2] 
pred <- prediction(predicted_values, test_data$gen_election)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

# Variable Importance Plot for Random Forest
varImpPlot(rf)

# Model 2 Artificial Neural Network
# Importing the required Library
library(nnet)

# Building the Model with 5 Hidden nodes
ann <- nnet(gen_election~., data=train_data, size=5, na.action=na.exclude,maxit=1000)
summary(ann)

predicted_values_NN <- predict(ann, test_data,type= "raw")
threshold <- 0.5
pred_NN<-factor(ifelse(predicted_values_NN > threshold, 'W','L'))


# Creating confusion Matrix for the ANN Model
confusionMatrix(pred_NN, test_data$gen_election, positive = 'W')

# ROC curve for the ANN model
library(ROCR)
library(ggplot2)
predicted_values_NN <- predict(ann, test_data,type= "raw")
pred_NN <- prediction(predicted_values_NN, test_data$gen_election)

perf <- performance(pred_NN, measure = "tpr", x.measure = "fpr")
auc <- performance(pred_NN, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


# Tried different values for hidden node, the maximum that could be given is 24 as shown
ann <- nnet(gen_election~., data=train_data, size=24, na.action=na.exclude,maxit=1000)
summary(ann)


predicted_values_NN <- predict(ann, test_data,type= "raw")
threshold <- 0.5
pred_NN<-factor(ifelse(predicted_values_NN > threshold, 'W','L'))


# Creating confusion Matrix for the ANN Model, with intern nodes = 24
confusionMatrix(pred_NN, test_data$gen_election, positive = 'W')

# ROC curve for the ANN model, with intern nodes = 24
library(ROCR)
library(ggplot2)
predicted_values_NN <- predict(ann, test_data,type= "raw")
pred_NN <- prediction(predicted_values_NN, test_data$gen_election)

perf <- performance(pred_NN, measure = "tpr", x.measure = "fpr")
auc <- performance(pred_NN, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))



# GBM Model

# Building GBM Model
gbm_caret <- train(as.factor(gen_election) ~ ., 
                   data = train_data, method = "gbm", trControl = trainControl
                   (method = "repeatedcv", number = 4, repeats = 4),verbose = FALSE)
summary(gbm_caret)

predicted_values_gbm <- predict(gbm_caret, test_data,type= "prob")[,2] 
threshold <- 0.5
pred_gbm<-factor(ifelse(predicted_values_gbm > threshold, 'W','L'))

#Confusion Matrix for GBM model
confusionMatrix(pred_gbm, test_data$gen_election, 
                positive = 'W')

# ROC for GBM model
library(ROCR)
library(ggplot2)
predicted_values_gbm <- predict(gbm_caret, test_data,type= "prob")[,2]
pred_gbm <- prediction(predicted_values_gbm, test_data$gen_election)

perf <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")
auc <- performance(pred_gbm, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
#ROC plot for GBM
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#Checking for social media through ftable
ftable(xtabs(~youtube+facebook+twitter+gen_election, data=Data_csv))
colnames(Data_csv) 
colnames(train_data)
