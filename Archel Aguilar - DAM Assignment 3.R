#-----------------------------------------
#Archel Aguilar (980817867)
# DAM - Assignment 3
#
# Credit Card Default Model
# 
# Background: 
#   -  Predict which customers will default on their credit card repayments next month.
#
# Data:
#   - Default Payments of Credit Card Clients in Taiwan from 2005
#   - This dataset contains information on default payments, demographic factors, credit data, history of payment, and bill statements 
#         of credit card clients in Taiwan from April 2005 to September 2005. 
# 
# Variables
#	-ID: 						            ID of each client
#	-LIMIT_BAL: 		            Amount of given credit in NT dollars (includes individual and family/supplementary credit
#	-SEX: 					            Gender (1=male, 2=female)
#	-EDUCATION: 		            (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
#	-MARRIAGE: 			            Marital status (1=married, 2=single, 3=others)
#	-AGE: 					            Age in years
#
#	-PAY_PC1, PAY_PC2, PAY_PC3: 
#                             First three Principal Components of repayment status from April to September, 2005
#
#	-AMT_PC1, AMT_PC2, AMT_PC3, AMT_PC4, AMT_PC5, AMT_PC6, AMT_PC7: 
#								              First seven Principal Components of the bill statement amount and the amount of previous 
#								                payments from April to September 2005
#	-default: 					        Default payment next month (1=yes, 0=no)

#
#-----------------------------------------

#clear variables
rm(list=ls())

install.packages("ISLR")
library(ISLR)

setwd("C:/Users/arche/Documents/UTS/R-References/R-references-Git/DAMAssignmentC")
#setwd("C:/Personal/UTS/R-References/R-references-Git/DAMAssignment2B")

getwd()

otrain = read.csv("AT3_credit_train_STUDENT.csv")
str(otrain)

otrain$EDUCATION = as.factor(otrain$EDUCATION)
otrain$MARRIAGE = as.factor(otrain$MARRIAGE)
otrain$SEX = as.factor(otrain$SEX)

#---------------------------------------------
# Analyse data
#---------------------------------------------

summary(otrain)

str(otrain)

contrasts(otrain$SEX)
contrasts(otrain$EDUCATION)
contrasts(otrain$MARRIAGE)

nrow(otrain)     #23,101

options(scipen=20)
#boxplot(otrain$LIMIT_BAL)

install.packages("ggplot2")
library(ggplot2)
library(scales)
library(reshape2)


ggplot(data.frame(otrain$LIMIT_BAL), aes(x="Balance Limit", y=otrain$LIMIT_BAL)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("$")
ggplot(data.frame(otrain$AGE), aes(x="Age", y=otrain$AGE)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AGE), aes(x=otrain$AGE)) + geom_bar() + xlab("Age")
#some outliers in age - over 120 years of age
ggplot(data.frame(otrain$SEX), aes(x=otrain$SEX)) + geom_bar() + xlab("Sex")
#one record of cat, dog, dolphin - need to change to NA

otrain$SEX = as.character(otrain$SEX)
otrain$SEX[otrain$SEX=="cat"] = "Other"
otrain$SEX[otrain$SEX=="dog"] = "Other"
otrain$SEX[otrain$SEX=="dolphin"] = "Other"
#write.csv(otrain, "test.csv")
otrain$SEX = as.factor(otrain$SEX)
#otrain = droplevels(otrain)

ggplot(data.frame(otrain$EDUCATION), aes(x=otrain$EDUCATION)) + geom_bar() + xlab("Education")
ggplot(data.frame(otrain$MARRIAGE), aes(x=otrain$MARRIAGE)) + geom_bar() + xlab("Marriage")
ggplot(data.frame(otrain$PAY_PC1), aes(x="Pay PC1", y=otrain$PAY_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$PAY_PC2), aes(x="Pay PC2", y=otrain$PAY_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$PAY_PC3), aes(x="Pay PC3", y=otrain$PAY_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AMT_PC1), aes(x="Amt PC1", y=otrain$AMT_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AMT_PC2), aes(x="Amt PC2", y=otrain$AMT_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AMT_PC3), aes(x="Amt PC3", y=otrain$AMT_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AMT_PC4), aes(x="Amt PC4", y=otrain$AMT_PC4)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AMT_PC5), aes(x="Amt PC5", y=otrain$AMT_PC5)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AMT_PC6), aes(x="Amt PC6", y=otrain$AMT_PC6)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AMT_PC7), aes(x="Amt PC7", y=otrain$AMT_PC7)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$default), aes(x=otrain$default)) + geom_bar() + xlab("Default")

#number of defaults 
nrow(otrain[otrain$default=="Y",]) - #5583
nrow(otrain[otrain$default=="Y",]) / nrow(otrain) #0.24 proportion of defaults


# otest = read.csv("AT3_credit_test_STUDENT.csv")
# str(otest)
# 
# otest$EDUCATION = as.factor(otest$EDUCATION)
# otest$MARRIAGE = as.factor(otest$MARRIAGE)
# otest$SEX = as.factor(otest$SEX)
# 
# nrow(otest)     #6899
# ggplot(data.frame(otest$LIMIT_BAL), aes(x="Balance Limit", y=otest$LIMIT_BAL)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("$")
# ggplot(data.frame(otest$AGE), aes(x="Age", y=otest$AGE)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AGE), aes(x=otest$AGE)) + geom_bar() + xlab("Age")
# #no ages above 100
# ggplot(data.frame(otest$SEX), aes(x=otest$SEX)) + geom_bar() + xlab("Sex")
# #no dogs, dolphins
# ggplot(data.frame(otest$EDUCATION), aes(x=otest$EDUCATION)) + geom_bar() + xlab("Education")
# #5 and 6 are both unknown need to combine.
# ggplot(data.frame(otest$MARRIAGE), aes(x=otest$MARRIAGE)) + geom_bar() + xlab("Marriage")
# ggplot(data.frame(otest$PAY_PC1), aes(x="Pay PC1", y=otest$PAY_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$PAY_PC2), aes(x="Pay PC2", y=otest$PAY_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$PAY_PC3), aes(x="Pay PC3", y=otest$PAY_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AMT_PC1), aes(x="Amt PC1", y=otest$AMT_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AMT_PC2), aes(x="Amt PC2", y=otest$AMT_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AMT_PC3), aes(x="Amt PC3", y=otest$AMT_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AMT_PC4), aes(x="Amt PC4", y=otest$AMT_PC4)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AMT_PC5), aes(x="Amt PC5", y=otest$AMT_PC5)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AMT_PC6), aes(x="Amt PC6", y=otest$AMT_PC6)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot(data.frame(otest$AMT_PC7), aes(x="Amt PC7", y=otest$AMT_PC7)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# #no defaults



#---------------------------------------------
# Create train and test data
#---------------------------------------------

install.packages('caret', dependencies = TRUE)
library(caret)

set.seed(42)

#splits sample group maintaining the ratio of the target
train = createDataPartition(y = otrain$default, p = 0.7, list = F)


# partition purchase data into two sets 
training = otrain[train, ]
resetTraining = training
testing = otrain[-train, ]
resetTesting = testing

str(training)
str(testing)

nrow(otrain)     #23,101
nrow(training)  #16,172
nrow(testing)   #6,929

nrow(otrain[otrain$default=="Y",]) #5583 - number of defaults
nrow(otrain[otrain$default=="N",]) #17518 - number of non defaults
#proportion of defaults in data = 0.2416778

nrow(training[training$default=="Y",]) #3909 - number of defaults in training
nrow(training[training$default=="N",]) #12263 - number of non defaults in training
#proportion of defaults in data = 0.2417141
#equal number of defaults


#---------------------------
# Create models
#---------------------------

#Precision:Positive Pred Value
#Recall:Sensitivtiy

#include all except for identifier (ID)
glmodel = "default ~. -ID" #all variables (AIC: 15805, F1: 0.660561, sensitivity/recall: 0.54640, precision/pos pred value: 0.83502)

def.glm = glm(formula = glmodel,
             data = training,
             family = "binomial")
summary(def.glm)
#AIC: 15803


###########################
# Create probabilities and predictions
###########################

# add the probabilities to the testing data
testing$probability = predict(def.glm, newdata = testing, type = "response")

# assume that the optimum probability threshold is 0.5
# Create the class prediction - our target is Y
testing$prediction = "N"
testing[testing$probability >= 0.5, "prediction"] = "Y"


###########################
# Evaluation using confusion matrix
###########################

#set Target=1 as the focus for confusion matrix
cm = confusionMatrix(data = as.factor(testing$prediction), testing$default, positive="Y")
#get F1 score
cm$byClass["F1"] #0.3420578 

#summary
cm
#                   Reference
# Prediction          N    Y
# N                 5092 1295
# Y                  163  379
# 
# Accuracy : 0.7896               
# 95% CI : (0.7798, 0.7991)     
# No Information Rate : 0.7584               
# P-Value [Acc > NIR] : 0.0000000004118      
# 
# Kappa : 0.2539               
# Mcnemar's Test P-Value : < 0.00000000000000022
# 
# Sensitivity : 0.22640              
# Specificity : 0.96898              
# Pos Pred Value : 0.69926              
# Neg Pred Value : 0.79724              
# Prevalence : 0.24159              
# Detection Rate : 0.05470              
# Detection Prevalence : 0.07822              
# Balanced Accuracy : 0.59769              
# 
# 'Positive' Class : Y         

#-------------------------------------------
# Lasso & Ridge check
#--------------------------------------------

install.packages("glmnet")
library(glmnet)

###########################
# Lasso Regression (F1 - 0.3316629, sensitivity/recall: 0.21744, precision/pos pred value: 0.69866)
###########################

#reset variables
training = resetTraining
#remove ID and Target from model

excludeID = which(colnames(training)=="ID")
excludeTarget = which(colnames(training)=="default")

x = model.matrix(~ ., training[, c(-excludeID, -excludeTarget)])
y = training$default

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-excludeID, -excludeTarget)])
a = testing$default

set.seed(42)

# alpha = 1 specifies lasso regression
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)
#lambda is the strength of the penalty on the coefficients
#as lambda gets larger, ths bias is unchanged but the variance drops


# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min #error measure. can choose either
cv.fit_lasso$lambda.1se #error measure. can choose either

cv.lasso_coef = coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min)
cv.lasso_coef
#co-efficients with the largest +ve result provide the biggest change to the baseline factor


prediction_lasso = predict(cv.fit_lasso$glmnet.fit, newx =z, type = "class", s = cv.fit_lasso$lambda.min)

lasso_confusion = confusionMatrix(data = as.factor(prediction_lasso), a, positive="Y")
lasso_confusion
#               Reference
# Prediction    N    Y
#           N 5098 1310
#           Y  157  364
# 
# Accuracy : 0.7883               
# 95% CI : (0.7785, 0.7978)     
# No Information Rate : 0.7584               
# P-Value [Acc > NIR] : 0.000000002055       
# 
# Kappa : 0.2451               
# Mcnemar's Test P-Value : < 0.00000000000000022
# 
# Sensitivity : 0.21744              
# Specificity : 0.97012              
# Pos Pred Value : 0.69866              
# Neg Pred Value : 0.79557              
# Prevalence : 0.24159              
# Detection Rate : 0.05253              
# Detection Prevalence : 0.07519              
# Balanced Accuracy : 0.59378              
# 
# 'Positive' Class : Y         

lasso_confusion$byClass["F1"] #0.3316629

###########################
# Ridge Regression (F1: 0.3143782, sensitivity/recall: 0.20311, precision/pos pred value: 0.69530)
###########################

#reset variables
training = resetTraining
#remove ID and Target from model

excludeID = which(colnames(training)=="ID")
excludeTarget = which(colnames(training)=="default")

x = model.matrix(~ ., training[, c(-excludeID, -excludeTarget)])
y = training$default

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-excludeID, -excludeTarget)])
a = testing$default

set.seed(42)

# alpha = 0 specifies ridge regression
cv.fit_ridge = cv.glmnet(x, y, family = 'binomial', alpha = 0)

# Results
plot(cv.fit_ridge)
cv.fit_ridge$lambda.min
cv.fit_ridge$lambda.1se

#shows the coefficents
coef(cv.fit_ridge, s = cv.fit_ridge$lambda.min)

prediction_ridge = predict(cv.fit_ridge$glmnet.fit, newx=z, type="class", s = cv.fit_ridge$lambda.min)


#set Target=1 as the focus for confusion matrix
ridge_confusion = confusionMatrix(data = as.factor(prediction_ridge), testing$default, positive="Y")
ridge_confusion
#               Reference
# Prediction    N    Y
#           N 5106 1334
#           Y  149  340
# 
# Accuracy : 0.786                
# 95% CI : (0.7761, 0.7956)     
# No Information Rate : 0.7584               
# P-Value [Acc > NIR] : 0.00000003024        
# 
# Kappa : 0.2303               
# Mcnemar's Test P-Value : < 0.00000000000000022
# 
# Sensitivity : 0.20311              
# Specificity : 0.97165              
# Pos Pred Value : 0.69530              
# Neg Pred Value : 0.79286              
# Prevalence : 0.24159              
# Detection Rate : 0.04907              
# Detection Prevalence : 0.07057              
# Balanced Accuracy : 0.58738              
# 
# 'Positive' Class : Y 

ridge_confusion$byClass["F1"] #0.3143782 

#-------------------------------------------
# Tree classification
#--------------------------------------------

install.packages("rpart", dependencies = TRUE)
install.packages("rpart.plot", dependencies = TRUE)
install.packages("mlbench")

library(rpart)
library(rpart.plot)
library(mlbench)

set.seed(42)

#reset variables
training = resetTraining
#remove ID and Target from model

excludeID = which(colnames(training)=="ID")
excludeTarget = which(colnames(training)=="default")

x = model.matrix(~ ., training[, c(-excludeID, -excludeTarget)])
y = training$default

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-excludeID, -excludeTarget)])
a = testing$default

###########################
# Decision Tree (F1: 0.5262017, sensitivity/recall: 0.4349, precision/pos pred value: 0.6661)
###########################

#build model
rpart_model = rpart(training$default ~.-ID,data = training, method="class") #use method ="anova" for regression problems

#plot tree
prp(rpart_model)


#prediction
rpart_predict = predict(rpart_model,testing,type="class")

rpart_confusion = confusionMatrix(data = as.factor(rpart_predict), testing$default, positive="Y")
rpart_confusion

rpart_confusion$byClass["F1"] #0.5262017

#               Reference
# Prediction    N    Y
#           N 4890  946
#           Y  365  728
# 
# Accuracy : 0.8108               
# 95% CI : (0.8014, 0.82)       
# No Information Rate : 0.7584               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.4144               
# Mcnemar's Test P-Value : < 0.00000000000000022
# 
# Sensitivity : 0.4349               
# Specificity : 0.9305               
# Pos Pred Value : 0.6661               
# Neg Pred Value : 0.8379               
# Prevalence : 0.2416               
# Detection Rate : 0.1051               
# Detection Prevalence : 0.1577               
# Balanced Accuracy : 0.6827               
# 
# 'Positive' Class : Y         


###########################
# Random Forests (F1: 0.5614414, sensitivity/recall: 0.4654, precision/pos pred value: 0.7075)
###########################

install.packages("randomForest")
library(randomForest)

#reset variables
training = resetTraining
#remove ID and Target from model

excludeID = which(colnames(training)=="ID")
excludeTarget = which(colnames(training)=="default")

x = model.matrix(~ ., training[, c(-excludeID, -excludeTarget)])
y = training$default

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-excludeID, -excludeTarget)])
a = testing$default

set.seed(42)


#run cross validation - takes a long time
tc = trainControl(method="cv", number=5, classProbs=TRUE)
#takes 10mins to run
rf.fit = train(default ~. -ID, data=training, method="rf", trControl=tc)
rf.fit
# Random Forest 
# 
# 16172 samples
# 16 predictor
# 2 classes: 'N', 'Y' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 12937, 12939, 12938, 12937, 12937 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2     0.7945826  0.2843341
# 12    0.8182044  0.4438461
# 23    0.8177719  0.4446908
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 12.

tuneRF(training[,c(-excludeID, -excludeTarget)], training$default, ntreeTry =1000, stepFactor = 2, improve = 1, trace=T, plot=T)




#Build random forest model
#-mytry = number of random variables selcted at each tree split (it's good to have variety for each tree to learn)
#  default 
#    -if regression=floor(number of varaibles/3)
#    -if categorical=floor(sqrt(no of independent variables))
# lower mtry means 1) less correlation between trees (good thing), 2) decreases strength of each tree. cannot predict accurately because of the limited variables (bad thing)

rf_model = randomForest(training$default ~. -ID, data = training, mtry=12, importance=TRUE, xtest=testing[,c(-excludeID, -excludeTarget)], keep.forest=TRUE, ntree=1000)
rf_model
# No. of variables tried at each split: 3 
# OOB estimate of  error rate: 18.48%
# Confusion matrix:
#   N    Y class.error
# N 11538  725  0.05912093
# Y  2263 1646  0.57892044

#--------------tried with mtry=12
# No. of variables tried at each split: 12
# 
# OOB estimate of  error rate: 18.19% <--misclassification rate
# Confusion matrix:
#   N    Y class.error
# N 11386  877  0.07151594
# Y  2064 1845  0.52801228



#model summary
summary(rf_model)

#variables contained in model 
names(rf_model)

#probability
test_prob_rf = predict(rf_model, testing, type="prob")

#predictions for test set
test_predictions_rf = predict(rf_model, testing, type="class")
rf_confusion = confusionMatrix(data = as.factor(test_predictions_rf), testing$default, positive="Y")

#predictions for test set
#test_predictions_rf = data.frame(testing, rf_model$test$predicted)
#rf_confusion = confusionMatrix(data = as.factor(test_predictions_rf$rf_model.test.predicted), testing$Target, positive="1")

rf_confusion$byClass["F1"] #0.5614414
rf_confusion
#               Reference
# Prediction    N    Y
#           N 4933  895
#           Y  322  779
# 
# Accuracy : 0.8244               
# 95% CI : (0.8152, 0.8333)     
# No Information Rate : 0.7584               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.4574               
# Mcnemar's Test P-Value : < 0.00000000000000022
# 
# Sensitivity : 0.4654               
# Specificity : 0.9387               
# Pos Pred Value : 0.7075               
# Neg Pred Value : 0.8464               
# Prevalence : 0.2416               
# Detection Rate : 0.1124               
# Detection Prevalence : 0.1589               
# Balanced Accuracy : 0.7020               
# 
# 'Positive' Class : Y     

#quantitative measure of variable importance
importance(rf_model)
#sorted plot of importance
varImpPlot(rf_model)

#mean decrease accuracy = how much of the model accuracy decreases if we drop that variable
#high value of mean decrease accuracy or gini scores higher importance of the variable in the model

#top predictors
# 1. PAY_PC1
# 2. AGE
# 3. LIMIT_BAL
# 4. AMT_PC2
# 5. AMT_PC1

#----------------------------------------------
#   Plot ROC curve
#----------------------------------------------

install.packages("pROC")
library(pROC)

test_auc = auc(testing$default, test_prob_rf[,2])
test_auc #Area under the curve: 0.803
plot(roc(testing$default, test_prob_rf[,2]))


#----------------------------------------------
#   Boosting
#----------------------------------------------

install.packages("gbm", dependencies =TRUE)
library(gbm)

#reset variables
training = resetTraining
#remove ID and Target from model

excludeID = which(colnames(training)=="ID")
excludeTarget = which(colnames(training)=="default")

x = model.matrix(~ ., training[, c(-excludeID, -excludeTarget)])
y = training$default

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-excludeID, -excludeTarget)])
a = testing$default

set.seed(42)

#cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)


# defining some parameters
gbm_depth = 5 #maximum nodes per tree
gbm_n.min = 5 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.001 #learning rate
cores_num = 4 #number of cores
gbm_cv.folds=5 #number of cross-validation folds to perform
num_trees = 4000

# fit initial model
gbm_fit = gbm(training$default ~. -ID, data = training,
              distribution='bernoulli', 
              n.trees=num_trees, #the number of GBM interaction
              interaction.depth= gbm_depth,
              n.minobsinnode = gbm_n.min, 
              shrinkage=gbm_shrinkage, 
              cv.folds=gbm_cv.folds, 
              verbose = T, #print the preliminary output
              n.cores = cores_num
)

summary(gbm_fit)


best.iter = gbm.perf(gbm_fit, method = "cv")
###How many trees should we use?

testing$probability = predict(gbm_fit, testing, n.trees = best.iter, type = "response")
#probability of MM


#----------------------------------------------
#   Predict on validation file
#----------------------------------------------

# cpurvalid = read.csv("repurchase_validation.csv")
# str(cpurvalid)
# 
# cpurvalid$Target                     = as.factor(0)
# cpurvalid$ID                         = as.integer(0)
# cpurvalid$age_of_vehicle_years       = as.factor(cpurvalid$age_of_vehicle_years)
# cpurvalid$sched_serv_warr            = as.factor(cpurvalid$sched_serv_warr )         
# cpurvalid$non_sched_serv_warr        = as.factor(cpurvalid$non_sched_serv_warr)
# cpurvalid$sched_serv_paid            = as.factor(cpurvalid$sched_serv_paid)
# cpurvalid$non_sched_serv_paid        = as.factor(cpurvalid$non_sched_serv_paid)
# cpurvalid$total_paid_services        = as.factor(cpurvalid$total_paid_services)
# cpurvalid$total_services             = as.factor(cpurvalid$total_services)
# cpurvalid$mth_since_last_serv        = as.factor(cpurvalid$mth_since_last_serv)
# cpurvalid$annualised_mileage         = as.factor(cpurvalid$annualised_mileage)
# cpurvalid$num_dealers_visited        = as.factor(cpurvalid$num_dealers_visited)
# cpurvalid$num_serv_dealer_purchased  = as.factor(cpurvalid$num_serv_dealer_purchased)
# 
# summary(cpurvalid)
# 
# #------------------
# # check data
# #------------------
# #frequency charts
# ggplot(data.frame(cpurvalid$age_band), aes(x=cpurvalid$age_band)) + geom_bar() + xlab("Age Band")
# ggplot(data.frame(cpurvalid$gender), aes(x=cpurvalid$gender)) + geom_bar() + xlab("Gender")
# ggplot(data.frame(cpurvalid$car_model), aes(x=cpurvalid$car_model)) + geom_bar() + xlab("Car Model")
# ggplot(data.frame(cpurvalid$car_segment), aes(x=cpurvalid$car_segment)) + geom_bar() + xlab("Car Type")
# ggplot(data.frame(cpurvalid$age_of_vehicle_years), aes(x=cpurvalid$age_of_vehicle_years)) + geom_bar() + xlab("Vehicle Age (Deciles)")
# ggplot(data.frame(cpurvalid$sched_serv_warr), aes(x=cpurvalid$sched_serv_warr)) + geom_bar() + xlab("# Scheduled services (Deciles)")
# ggplot(data.frame(cpurvalid$non_sched_serv_warr), aes(x=cpurvalid$non_sched_serv_warr)) + geom_bar() + xlab("# Non-Scheduled services (Deciles)")
# ggplot(data.frame(cpurvalid$sched_serv_paid), aes(x=cpurvalid$sched_serv_paid)) + geom_bar() + xlab("Amount paid for scheduled services (Deciles)")
# ggplot(data.frame(cpurvalid$non_sched_serv_paid), aes(x=cpurvalid$non_sched_serv_paid)) + geom_bar() + xlab("Amount paid for non scheduled services (Deciles)")
# ggplot(data.frame(cpurvalid$total_paid_services), aes(x=cpurvalid$total_paid_services)) + geom_bar() + xlab("Amount paid for all services (Deciles)")
# ggplot(data.frame(cpurvalid$total_services), aes(x=cpurvalid$total_services)) + geom_bar() + xlab("Total # services (Deciles)")
# ggplot(data.frame(cpurvalid$mth_since_last_serv), aes(x=cpurvalid$mth_since_last_serv)) + geom_bar() + xlab("Months since last service (Deciles)")
# ggplot(data.frame(cpurvalid$annualised_mileage), aes(x=cpurvalid$annualised_mileage)) + geom_bar() + xlab("Months since last service (Deciles)")
# ggplot(data.frame(cpurvalid$num_dealers_visited), aes(x=cpurvalid$num_dealers_visited)) + geom_bar() + xlab("Number of dealers visited for servicing (Deciles)")
# ggplot(data.frame(cpurvalid$num_serv_dealer_purchased), aes(x=cpurvalid$num_serv_dealer_purchased)) + geom_bar() + xlab("Number of services at purchased dealer (Deciles)")
# 
# 
# 
# str(cpurvalid)
# cpurvalid = cpurvalid[c(17,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
# str(cpurvalid)
# 
# firstrow = training[1,]
# 
# cpurvalid = rbind(training[1,], cpurvalid)
# cpurvalid = cpurvalid[-1,]
# 
# 
# #probability
# cpurvalid.prob_rf = predict(rf_model, cpurvalid, type="prob")
# cpurvalid.prob_rf = as.data.frame(cpurvalid.prob_rf)
# cpurvalid$prob_0 = as.numeric(cpurvalid.prob_rf[,1])
# cpurvalid$prob_1 = as.numeric(cpurvalid.prob_rf[,2])
# 
# #predictions for test set
# cpurvalid.predictions_rf = predict(rf_model, cpurvalid, type="class")
# 
# cpurvalid$Target = cpurvalid.predictions_rf
# cpurvalid.predTarget = nrow(cpurvalid[cpurvalid$Target=="1",]) #1235
# #proportion of target predicted
# cpurvalid.predTarget / nrow(cpurvalid) #0.0247 -similar to training proportion
# 
# write.csv(cpurvalid, "predicted_repurchase_validation.csv")


#---------------------- END ---------------------------



