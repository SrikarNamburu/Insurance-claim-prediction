rm(list=ls(all=TRUE))

setwd("")
#reading the.csv files
train_policy_demographics = read.csv("Train_Policy_Demographics-1542969243754.CSV")
train_claim_details = read.csv("Train_ClaimDetails-1542969243754.csv")
train_claim_size = read.csv("Train-1542969243754.csv") 
sub_preds = read.csv("predictions-1542969243754.csv")
test_claim_size = read.csv("Test-1542969243754.csv")
test_policy_demographics = read.csv("Test_Policy_Demographics-1542969243754.csv")
test_claim_details = read.csv("Test_ClaimDetails-1542969243754.csv")


#checking if the claim id is same in all
sum(train_policy_demographics$ClaimID == train_claim_details$ClaimID) 
sum(train_policy_demographics$ClaimID == train_claim_size$ClaimID) 

sum(test_policy_demographics$ClaimID == test_claim_details$ClaimID) 
sum(test_policy_demographics$ClaimID == test_claim_size$ClaimID) 


#doing outer join for df's
train4 = merge(x = train_policy_demographics , y = train_claim_details, by = "ClaimID", all = TRUE)
train4 = merge(x = train4 , y = train_claim_size, by = "ClaimID", all = TRUE)
write.csv(train4, "train4.csv")
claim = merge(x = train_policy_demographics , y = train_claim_details, by = "ClaimID", all = TRUE)
claim = merge(x = claim , y = train_claim_size, by = "ClaimID", all = TRUE)


test1 = merge(x = test_policy_demographics , y = test_claim_details, by = "ClaimID", all = TRUE)

str(train)
str(test)
write.csv(train, "train.csv")
write.csv(test, "test.csv")

#Checking the missing values in both train and test
Percentage_missing_train=data.frame("MissingPercentage"=(colSums(is.na(train))/nrow(train))*100)
Percentage_missing_test=data.frame("MissingPercentage"=(colSums(is.na(test))/nrow(test))*100)

#Dropping the claimId column and col with more missing values from both train and test
train=train[,setdiff(names(train),c("PolicyID","ClaimID","SystemicPoisoning_other","Falls","Match_Multiclaimant_multiInterestedparties_claim"))]
test=test[,setdiff(names(test),c("PolicyID","ClaimID","SystemicPoisoning_other","Falls","Match_Multiclaimant_multiInterestedparties_claim"))]

#changing to factors
to_be_factors = c("PolicyType","PolicyForm","BusinessClass","Anyothercontributors","AnyMultipleInterestedparties")
train[,to_be_factors]=data.frame(sapply(train[,to_be_factors],as.character))
train[,to_be_factors]=data.frame(sapply(train[,to_be_factors],as.factor))
test[,to_be_factors]=data.frame(sapply(test[,to_be_factors],as.character))
test[,to_be_factors]=data.frame(sapply(test[,to_be_factors],as.factor))

train$Age_Injured = train$Age_Injured/100
test$Age_Injured = test$Age_Injured/100
train$Work_related_injury_status = as.character(train$Work_related_injury_status)
train$Work_related_injury_status <- ifelse(nchar(train$Work_related_injury_status)==0, NA, train$Work_related_injury_status)
train$Work_related_injury_status = as.factor(train$Work_related_injury_status)
sum(is.na(train$Work_related_injury_status))
sum(is.na(train$Work_related_injury_status))/14417

test$Work_related_injury_status = as.character(test$Work_related_injury_status)
test$Work_related_injury_status <- ifelse(nchar(test$Work_related_injury_status)==0, NA, test$Work_related_injury_status)
test$Work_related_injury_status = as.factor(test$Work_related_injury_status)
sum(is.na(test$Work_related_injury_status))
sum(is.na(test$Work_related_injury_status))/4806


p = names(train[,c(13:43, 49:52, 59)])
for (i in p){
train[,i]= as.character(train[,i])
train[,i] <- ifelse(nchar(train[,i])== 1, "1", "0")
train[,i] = as.factor(train[,i])
}

for (i in p){
  test[,i]= as.character(test[,i])
  test[,i] <- ifelse(nchar(test[,i])== 1, "1", "0")
  test[,i] = as.factor(test[,i])
}
library(DMwR)
train = knnImputation(train, k = 3)
test = knnImputation(test, k=3)
train$Injury_Date = as.character(train$Injury_Date)
test$Injury_Date = as.character(test$Injury_Date)
a <- as.Date(train$Injury_Date,format="%d-%m-%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(train$Injury_Date,format="%m/%d/%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
train$Injury_Date <- a # Put it back in your dataframe
write.csv(train, "train_updated.csv")
train$gap <- as.Date(train$Date_reported) - as.Date(train$Injury_Date)

c <- as.Date(test$Injury_Date,format="%d-%m-%Y") # Produces NA when format is not "%m/%d/%Y"
d <- as.Date(test$Injury_Date,format="%m/%d/%Y") # Produces NA when format is not "%d.%m.%Y"
c[is.na(c)] <- d[!is.na(d)] # Combine both while keeping their ranks
test$Injury_Date <- c # Put it back in your dataframe
test$gap <- as.Date(test$Date_reported) - as.Date(test$Injury_Date)
write.csv(test, "test_updated.csv")

train$gap = as.integer(train$gap)
test$gap = as.integer(test$gap)

#Dropping date columns
train=train[,setdiff(names(train),c("Injury_Date","Date_reported"))]
test=test[,setdiff(names(test),c("Injury_Date","Date_reported"))]
write.csv(train, "train3.csv")
write.csv(test,"test2.csv")
str(train)
insurance = train
# Do Train-Val Split
library(caret)
set.seed(1234)
rows=createDataPartition(insurance$ClaimSize,p = 0.7,list = FALSE)
train1=insurance[rows,]
val1=insurance[-rows,]

# # PreProcess the data to standadize the numeric attributes
num = c("Perperson_Policylimit","Age_Injured","PerOccurrence_PolicyLimit","CombinedSingleLimit","PolicyLimitPerInjury","PrimaFacie_percentagefault_injured","PrimaFacie_percentagefault_insured","PrimaFacie_percentagefault_otherinsured","PrimaFacie_percentagefault_uninsured","gap")
preProc<-preProcess(train1[,num],method = c("center", "scale"))
train[,num]<-as.data.frame(scale(train[,num])) 
train1<-predict(preProc,train1)
val1<-predict(preProc,val1)
test1 = predict(preProc, test)

#checking for class imbalances
prop.table(table(train1$ClaimSize))
prop.table(table(val1$ClaimSize))

#model 1 Logistic

library(nnet)
logm1=multinom(ClaimSize~.,train1)
summary(logm1)

train_pred_log=predict(logm1,train1,type="class")
confusionMatrix(train_pred_log,train1$ClaimSize)

val_pred_log=predict(logm1,val1,type="class")
confusionMatrix(val_pred_log, val1$ClaimSize)

test_pred_log=predict(logm1,test,type="class")
sub_preds$ClaimSize = test_pred_log
write.csv(sub_preds, "logistic.csv", row.names = FALSE)

metrics = data.frame(model = "logistic" ,train_accuracy = 68.69, val_accuracy = 68.99, test_accuracy = '' )

#rpart
library(rpart)
model_dt <- rpart(ClaimSize ~ . , train1)

preds_train_dt <- predict(model_dt,train1, type = "class")
confusionMatrix(preds_train_dt, train1$ClaimSize)

preds_val_dt <- predict(model_dt, val1, type="class")
confusionMatrix(preds_val_dt, val1$ClaimSize)

test_pred_dt=predict(logm1,test,type="class")
sub_preds$ClaimSize = test_pred_dt
write.csv(sub_preds, "dt.csv", row.names = FALSE)

metrics_dt = data.frame(model = "decison trees" ,train_accuracy = 69.74, val_accuracy = 70.1, test_accuracy = '' )
metrics = rbind(metrics,metrics_dt)

#c5.0
library(DMwR)
library(C50)
DT_C50 <- C5.0(ClaimSize~.,data=train1, Rules = TRUE )
summary(DT_C50)

##predict on train and validation
pred_Train_c5.0 = predict(DT_C50,newdata=train1, type="class")
pred_val_c5.0 = predict(DT_C50, newdata=val1, type="class")

#Error Metrics on train and test
confusionMatrix(train1$ClaimSize,pred_Train_c5.0)
confusionMatrix(val1$ClaimSize,pred_val_c5.0)

test_pred_c5.0=predict(DT_C50,test,type="class")
sub_preds$ClaimSize = test_pred_c5.0
write.csv(sub_preds, "c5.0.csv", row.names = FALSE)

metrics_dt = data.frame(model = "decison trees c5.0" ,train_accuracy = 77.7, val_accuracy = 73.47, test_accuracy = '' )
metrics = rbind(metrics,metrics_dt)


## Random Forest

library(randomForest)

model_rf <- randomForest(ClaimSize ~ . , train1,ntree = 100,mtry = 25)

importance(model_rf)
varImpPlot(model_rf)

# Predict on the train data
preds_train_rf <- predict(model_rf)
confusionMatrix(preds_train_rf, train1$ClaimSize)


# Store predictions from the model
preds_val_rf <- predict(model_rf, val1)
confusionMatrix(preds_val_rf, val1$ClaimSize)

test_pred_rf=predict(model_rf,test,type="class")
sub_preds$ClaimSize = test_pred_rf
write.csv(sub_preds, "rf.csv", row.names = FALSE)

metrics_dt = data.frame(model = "rf" ,train_accuracy = 70.43, val_accuracy = 72.59, test_accuracy = '' )
metrics = rbind(metrics,metrics_dt)


####Building  randomforest using caret

control <- trainControl(method="cv", number=5)
set.seed(1235869)
tunegrid <- expand.grid(mtry= 15)
rf_gridsearch <- train(ClaimSize ~ ., data=train1, method = "rf",
                       trControl=control,
                       tuneGrid = tunegrid)


# Predict on the train data
preds_train_rf1 <- predict(rf_gridsearch)
confusionMatrix(preds_train_rf1, train1$ClaimSize)


# Store predictions from the model
preds_rf1 <- predict(rf_gridsearch, val1)
confusionMatrix(preds_rf1, val1$ClaimSize)

test_pred_rf_grid=predict(rf_gridsearch,test)
sub_preds$ClaimSize = test_pred_rf_grid
write.csv(sub_preds, "rf_grid.csv", row.names = FALSE)

metrics_dt = data.frame(model = "rf_grid" ,train_accuracy = 93.37, val_accuracy = 73.66, test_accuracy = '' )
metrics = rbind(metrics,metrics_dt)

#svm
dumvar=dummyVars(ClaimSize~.,data=train1)
dummies=dummyVars(~.,data=test)
x.train=predict(dumvar, newdata = train1)
y.train=train1$ClaimSize
x.val = predict(dumvar, newdata = val1)
y.val = val1$ClaimSize
x.test = predict(dummies, newdata = test)

library(e1071)

#Build SVM model with RBF kernel 
model_RBF = svm(x.train,y.train, method = "C-classification", kernel = "radial", cost = 10,
                gamma = 0.1)
summary(model_RBF)

# Predict on train and test using the model
pred_train  =  predict(model_RBF, x.train) # x is all the input variables
pred_val=predict(model_RBF,x.val)

# Build Confusion matrix
confusionMatrix(pred_train,y.train)
confusionMatrix(pred_val,y.val)

pred_test_svm_rbf =predict(model_RBF,x.test)
sub_preds$ClaimSize = pred_test_svm_rbf
write.csv(sub_preds, "svm_rbf.csv", row.names = FALSE)

metrics_svm_rbf = data.frame(model = "svm_rbf" ,train_accuracy = 89.52, val_accuracy = 66.24, test_accuracy = '' )
metrics = rbind(metrics,metrics_svm_rbf)


#Grid Search/Hyper-parameter tuning
prop.table(table(train1$ClaimSize))
tuneResult <- tune(svm, train.x = x.train, train.y = y.train, ranges = list(gamma = c(0.01,1), cost = c(4,15),class.weights= c("lessthan100K" = 1.941, "200KandAbove" = 4.484,"100K-lessthan200K" = 3.381 ),tunecontrol=tune.control(cross=5)))
print(tuneResult) 
summary(tuneResult)

#Predict model and calculate errors
tunedModel <- tuneResult$best.model;tunedModel

# Predict on train and test using the model
pred_train  =  predict(tunedModel, x.train) # x is all the input variables
pred_val=predict(tunedModel,x.val)
# Build Confusion matrix
confusionMatrix(pred_train,y.train)
confusionMatrix(pred_val,y.val)


#knn.1
library(caret)
model_knn <- knn3(ClaimSize ~ . , train1, k = 5)

preds_train_k <- predict(model_knn, train1, type = "class")
confusionMatrix(preds_train_k, train1$ClaimSize)

preds_k_val <- predict(model_knn, val1, type= "class")
confusionMatrix(preds_k_val, val1$ClaimSize)

preds_k_test <- predict(model_knn, test, type = "class")
sub_preds$ClaimSize = preds_k_test
write.csv(sub_preds, "k.1.csv", row.names = FALSE)

metrics_k.1 = data.frame(model = "k.1" ,train_accuracy = 81.72, val_accuracy = 73.53, test_accuracy = NA )
metrics = rbind(metrics,metrics_k.1)

#knn.2

library(class)

#Deciding k value for k-NN
#Experiment with various odd values of k; k={1,3,5,7,..}
#Knn with cross validation and Grid search from library caret

trctrl <- trainControl(method = "cv", number = 2)
set.seed(3333)
grid <- expand.grid(k=c(9,13,15))
knn_fit <- train(x.train,y.train, method = "knn",
                 trControl=trctrl,
                 tuneGrid=grid)
plot(knn_fit)

val_pred_k = predict(knn_fit,newdata = val1)
a = confusionMatrix(val_pred_k,val_target)
a

metrics_knn_2.0 = data.frame(model = "knn_2.0" ,train_accuracy = NA, val_accuracy = 78.28, test_accuracy = NA )
metrics = rbind(metrics,metrics_knn_2.0)

#bagged d trees
library(rpart)
library(ipred)
set.seed(1234)
model_tree_bag <- bagging(ClaimSize ~ . , data=train, nbagg = 5,control = rpart.control(cp = 0.01, xval = 6))

preds_train_tree_bag <- predict(model_tree_bag,train1)
confusionMatrix(preds_train_tree_bag, train1$ClaimSize)

preds_tree_bag <- predict(model_tree_bag, val1)
confusionMatrix(preds_tree_bag, val1$ClaimSize)

pred_test_tree_bag =predict(model_tree_bag,test)
sub_preds$ClaimSize = pred_test_tree_bag
write.csv(sub_preds, "tree_bag.csv", row.names = FALSE)


#stacked ensemble

train_preds_df <- data.frame(rf = preds_train_rf, svm = pred_train,
                             tree = pred_Train_c5.0, logistic = train_pred_log,
                             ClaimSize = train1$ClaimSize)
val_preds_df =  data.frame(rf = preds_val_rf,  svm = pred_val,
                           tree = pred_val_c5.0, logistic = val_pred_log,
                           ClaimSize = val1$ClaimSize)
test_preds_df =  data.frame(rf = test_pred_rf,  svm = pred_test_svm_rbf,
                            tree = test_pred_c5.0, logistic = test_pred_log)

stacked_model <- multinom(ClaimSize ~ . , data = train_preds_df)

preds_train_stack <- predict(stacked_model,train_preds_df)
confusionMatrix(preds_train_stack, train_preds_df$ClaimSize)

preds_stack_val <- predict(stacked_model,val_preds_df)
confusionMatrix(preds_stack_val, val_preds_df$ClaimSize)

# ###
# claim$ClaimID = NULL
# claim$PolicyID = NULL
# str(claim)
# med = read.csv("med.csv")
# claim$Match_Multiclaimant_multiInterestedparties_claim = NULL
# write.csv(claim, "claim.csv")
# #changing to factors
# to_be_factors = c("PolicyType","PolicyForm","BusinessClass","Anyothercontributors","AnyMultipleInterestedparties")
# claim[,to_be_factors]=data.frame(sapply(claim[,to_be_factors],as.character))
# claim[,to_be_factors]=data.frame(sapply(claim[,to_be_factors],as.factor))
# claim$Age_Injured = claim$Age_Injured/100
# med$SystemicPoisoning_other =NULL
# med$MultipleInjuries = NULL
# med$OtherInjuries  = NULL
# str(med)
# q = names(med)
# med[,q]=sapply(med[,q],as.character)
# med$Amputation = as.character(med$Amputation)
# for (i in q){
#   for(j in c(1:14417)){
#     
#     med$col[j] <- ifelse(nchar(med[,i][j])== 1, med[][], "")
#     
#   }
# }
# med[14417,2]
# for (i in c(1:15)){
#   for(j in c(1:14417)){
#     
#     med$col[j] <- ifelse(nchar(med[j,i])== 1, med[j][i], "")
#     
#   }
# }
# 
# 14221-14417
# sum(med$col == "")
# 14417*15 - sum(med == "")




