#-----------------------------------------
# DAM-I.T.
#
# Archel Aguilar (980817867)
# Jack
# Jay
#
# DAM - Assignment 3
#
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


ggplot(data.frame(otrain$LIMIT_BAL), aes(x="Credit Limit", y=otrain$LIMIT_BAL)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("$")
hist(otrain$LIMIT_BAL, xlab="Credit Limit", main="Histogram of Credit Limit")
ggplot(data.frame(otrain$LIMIT_BAL), aes(x=otrain$LIMIT_BAL)) + geom_bar() + xlab("Credit Limit")


ggplot(data.frame(otrain$AGE), aes(x="Age", y=otrain$AGE)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AGE), aes(x=otrain$AGE)) + geom_bar() + xlab("Age")
length(which(otrain$AGE>100))

mean(otrain$AGE)


#convert age to deciles
#quantile(otrain$AGE, prob = seq(0, 1, length = 11), type = 5)
#otrain$AGEDEC = cut_number(otrain$AGE, n=11, closed="left")
#ggplot(data.frame(otrain$AGEDEC), aes(x=otrain$AGEDEC)) + geom_bar() + xlab("Age Decile")
#remove age
#otrain = within(otrain, rm("AGE"))


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

#group education (1= university or higher, 2=highschool, 3=Other)
otrain$EDU_ADJ[otrain$EDUCATION==1 | otrain$EDUCATION==2] = 1
otrain$EDU_ADJ[otrain$EDUCATION==3] = 2
otrain$EDU_ADJ[otrain$EDUCATION==0 | otrain$EDUCATION==4 | otrain$EDUCATION==5 | otrain$EDUCATION==6] = 3
otrain$EDU_ADJ = as.factor(otrain$EDU_ADJ)


ggplot(data.frame(otrain$MARRIAGE), aes(x=otrain$MARRIAGE)) + geom_bar() + xlab("Marriage")
ggplot(data.frame(otrain$PAY_PC1), aes(x="Pay PC1", y=otrain$PAY_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$PAY_PC2), aes(x="Pay PC2", y=otrain$PAY_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$PAY_PC3), aes(x="Pay PC3", y=otrain$PAY_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$AMT_PC1), aes(x="Amt PC1", y=otrain$AMT_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$AMT_PC2), aes(x="Amt PC2", y=otrain$AMT_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$AMT_PC3), aes(x="Amt PC3", y=otrain$AMT_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$AMT_PC4), aes(x="Amt PC4", y=otrain$AMT_PC4)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$AMT_PC5), aes(x="Amt PC5", y=otrain$AMT_PC5)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$AMT_PC6), aes(x="Amt PC6", y=otrain$AMT_PC6)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")
ggplot(data.frame(otrain$AMT_PC7), aes(x="Amt PC7", y=otrain$AMT_PC7)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("")


hist(otrain$PAY_PC1, xlab="PAY_PC1", main="Histogram of PAY_PC1")
hist(otrain$PAY_PC2, xlab="PAY_PC2", main="Histogram of PAY_PC2")
hist(otrain$PAY_PC3, xlab="PAY_PC3", main="Histogram of PAY_PC3")

hist(otrain$AMT_PC1, xlab="AMT_PC1", main="Histogram of AMT_PC1")
hist(otrain$AMT_PC2, xlab="AMT_PC2", main="Histogram of AMT_PC2")
hist(otrain$AMT_PC3, xlab="AMT_PC3", main="Histogram of AMT_PC3")
hist(otrain$AMT_PC4, xlab="AMT_PC4", main="Histogram of AMT_PC4")
hist(otrain$AMT_PC5, xlab="AMT_PC5", main="Histogram of AMT_PC5")
hist(otrain$AMT_PC6, xlab="AMT_PC6", main="Histogram of AMT_PC6")
hist(otrain$AMT_PC7, xlab="AMT_PC7", main="Histogram of AMT_PC7")


ggplot(data.frame(otrain$default), aes(x=otrain$default)) + geom_bar() + xlab("Default")


ggplot(data.frame(otrain$PAY_PC1, otrain$default), aes(x=otrain$default, y=otrain$PAY_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#negative coeffients have impact on defaults - defaulters tend to have more -ve (pay dulies)
ggplot(data.frame(otrain$AMT_PC1, otrain$default), aes(x=otrain$default, y=otrain$AMT_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$AGE, otrain$default), aes(x=otrain$default, y=otrain$AGE)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(otrain$LIMIT_BAL, otrain$default), aes(x=otrain$default, y=otrain$LIMIT_BAL)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(as.integer(otrain$EDUCATION), otrain$default), aes(x=otrain$default, y=as.integer(otrain$EDUCATION))) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#most of the defaults have lower education

boxplot(as.integer(otrain$EDUCATION) ~ otrain$default, data=otrain)
        
plot(otrain$PAY_PC1, otrain$LIMIT_BAL, col=otrain$default)

contrasts(otrain$default)


#number of defaults 
nrow(otrain[otrain$default=="Y",]) - #5583
nrow(otrain[otrain$default=="Y",]) / nrow(otrain) #0.24 proportion of defaults

#pairs(otrain)

install.packages("sjstats")
library(sjstats)

prop.table(table(otrain$SEX))
prop.table(table(otrain$EDUCATION))
prop.table(table(otrain$MARRIAGE))


#---------------------------------------------
# Create train and test data
#---------------------------------------------

install.packages('caret', dependencies = TRUE)
library(caret)

set.seed(12345) #42

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
#glmodel = "default ~. -ID" #all variables (AIC: 15805, F1: 0.660561, sensitivity/recall: 0.54640, precision/pos pred value: 0.83502)
#glmodel = "default ~ PAY_PC1 + AGE:LIMIT_BAL +AMT_PC1 + AMT_PC2 + PAY_PC2 + PAY_PC3 - ID"
glmodel = "default ~ PAY_PC1 + AGE:LIMIT_BAL + AGE:EDUCATION + AMT_PC1 + AMT_PC2 + PAY_PC2 + PAY_PC3 - ID" 


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

install.packages("pROC")
library(pROC)

test_auc = auc(testing$default, testing$probability)
test_auc #Area under the curve: 0.803
plot(roc(testing$default, testing$probability))



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
prediction_lasso.prob = predict(cv.fit_lasso$glmnet.fit, newx =z, type = "response", s = cv.fit_lasso$lambda.min)

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

#------------get ROC score

install.packages("pROC")
library(pROC)

test_auc = auc(testing$default, as.matrix(prediction_lasso.prob)[,1])
test_auc #Area under the curve: 0.7374
plot(roc(testing$default,  as.matrix(prediction_lasso.prob)[,1]))

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
prp(rpart_model, digits = -3)


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


#xmodel = "default ~. -ID" (Kaggle: 0.69462)
#xmodel = "default ~ PAY_PC1 + AGE:LIMIT_BAL + EDUCATION + AMT_PC1 + AMT_PC2 + AMT_PC6 + AMT_PC5 + AMT_PC7 + AMT_PC4 + AMT_PC3 + PAY_PC2 + PAY_PC3 + MARRIAGE + SEX - ID"
#xmodel = "default ~ PAY_PC1 + AGE:LIMIT_BAL + AMT_PC1 + AMT_PC2 + PAY_PC2 + PAY_PC3 - ID"
#xmodel = "default ~ PAY_PC1 + AGE:LIMIT_BAL + AGE:EDU_ADJ + AMT_PC1 + AMT_PC2 + PAY_PC2 + PAY_PC3 - ID"
#xmodel = "default ~ PAY_PC1 + AGE:LIMIT_BAL + AGE:EDUCATION + AMT_PC1 + AMT_PC2 + PAY_PC2 + PAY_PC3 - ID" #(Kaggle: 0.68825)
xmodel = "default ~ PAY_PC1 + AGE:LIMIT_BAL + AGE:EDUCATION + AMT_PC1 + AMT_PC2  + AMT_PC3 + AMT_PC4 + AMT_PC5 + AMT_PC6 + PAY_PC2 + PAY_PC3 - ID"

#run cross validation - takes a long time
tc = trainControl(method="cv", number=5, classProbs=TRUE)
#takes 10mins to run
rf.fit = train(form=as.formula(xmodel), data=training, method="rf", trControl=tc)
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

tuneRF(training[,c(-excludeID, -excludeTarget)], training$default, ntreeTry =2000, stepFactor = 2, improve = 1, trace=T, plot=T)

#Build random forest model
#-mytry = number of random variables selcted at each tree split (it's good to have variety for each tree to learn)
#  default 
#    -if regression=floor(number of varaibles/3)
#    -if categorical=floor(sqrt(no of independent variables))
# lower mtry means 1) less correlation between trees (good thing), 2) decreases strength of each tree. cannot predict accurately because of the limited variables (bad thing)

testing$SEX       = factor(testing$SEX, levels=levels(training$SEX))
testing$EDUCATION = factor(testing$EDUCATION, levels=levels(training$EDUCATION))
testing$MARRIAGE  = factor(testing$MARRIAGE, levels=levels(training$MARRIAGE))
testing$default   = factor(testing$default, levels=levels(training$default))

#rf_model = randomForest(as.formula(xmodel), data = training, mtry=12, importance=TRUE, xtest=testing[,c(-excludeID, -excludeTarget)], keep.forest=TRUE, ntree=1000)
#rf_model
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
#     N    Y class.error
# N 11386  877  0.07151594
# Y  2064 1845  0.52801228

rf_model = randomForest(as.formula(xmodel), data = training, mtry=9, importance=TRUE, keep.forest=TRUE, ntree=1000)
rf_model


#model summary
summary(rf_model)

#variables contained in model 
names(rf_model)

#probability
test_prob_rf = predict(rf_model, testing, type="prob")

#predictions for test set
test_predictions_rf = predict(rf_model, testing, type="class")
rf_confusion = confusionMatrix(data = as.factor(test_predictions_rf), testing$default, positive="Y")


rf_confusion$byClass["F1"] #0.548433
rf_confusion
# Reference
# Prediction    N    Y
# N 4891  904
# Y  364  770
# 
# Accuracy : 0.817          
# 95% CI : (0.8077, 0.826)
# No Information Rate : 0.7584         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.439          
# Mcnemar's Test P-Value : < 2.2e-16      
# 
# Sensitivity : 0.4600         
# Specificity : 0.9307         
# Pos Pred Value : 0.6790         
# Neg Pred Value : 0.8440         
# Prevalence : 0.2416         
# Detection Rate : 0.1111         
# Detection Prevalence : 0.1637         
# Balanced Accuracy : 0.6954         
# 
# 'Positive' Class : Y    


#quantitative measure of variable importance
importance(rf_model)
#sorted plot of importance
varImpPlot(rf_model)

plot(rf_model)


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
test_auc #Area under the curve: 0.7881
plot(roc(testing$default, test_prob_rf[,2]))

#------------------
# write predictions to file
#------------------

ovalid = read.csv("AT3_credit_test_STUDENT.csv")

# #------------------
# # check data
# #------------------

str(ovalid)

ovalid$EDUCATION = as.factor(ovalid$EDUCATION)
ovalid$MARRIAGE = as.factor(ovalid$MARRIAGE)
ovalid$SEX = as.factor(ovalid$SEX)

nrow(ovalid)     #6899

ovalid$SEX = as.character(ovalid$SEX)
ovalid$SEX[ovalid$SEX=="cat"] = "Other"
ovalid$SEX[ovalid$SEX=="dog"] = "Other"
ovalid$SEX[ovalid$SEX=="dolphin"] = "Other"
ovalid$SEX = as.factor(ovalid$SEX)


validation_out = NULL 
validation_out$ID = ovalid$ID
validation_out$predict = predict(rf_model, ovalid, type="class")

validation_out = as.data.frame(validation_out)
validation_out$default = 0
validation_out$default[validation_out$predict=="Y"] = 1
validation_out$default = as.factor(validation_out$default)

#rownames(validation_out) = c()

output = validation_out[,c("ID","default")]

write.csv(output, "AT3_DAM_IT_credit_sample_UPLOAD_RF.csv", row.names = FALSE)



#----------------------------------------------
#   Boosting (F1: 0.5853835, sensitivity/recall: 0.4833, precision/pos pred value: 0.7422)
#----------------------------------------------

install.packages("gbm", dependencies=TRUE)
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

training$default_binary = 0
training[training$default == "Y", "default_binary"] = 1
#training$default_binary = as.factor(training$default_binary)

testing$default_binary = 0
testing[testing$default == "Y", "default_binary"] = 1
#testing$default_binary = as.factor(testing$default_binary)


set.seed(42)

#bmodel = "default_binary~. -ID"
bmodel = "default_binary ~ PAY_PC1 + AGE:LIMIT_BAL + AGE:EDUCATION + AMT_PC1 + AMT_PC2 + PAY_PC2 + PAY_PC3 - ID - default"


#tuning
fitControl = trainControl(## 10-fold CV
  method = "repeatedcv", #repeatedcv
  number = 10,
  ## repeated ten times
  repeats = 10)

gbm_tune = train(as.formula(bmodel), data=training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbm_tune

# defining some parameters
# gbm_depth = 3 #maximum nodes per tree
# gbm_n.min = 10 #minimum number of observations in the trees terminal, important effect on overfitting
# gbm_shrinkage=0.1 #learning rate
# cores_num = 2 #number of cores
# gbm_cv.folds=5 #number of cross-validation folds to perform
# num_trees = 150 #20000 is best

gbm_depth = 3 #maximum nodes per tree
gbm_n.min = 10 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.1 #learning rate
cores_num = 1 #number of cores
gbm_cv.folds=0 #number of cross-validation folds to perform (can be zero if you have tuned it already)
num_trees = 150 #150 is best

# fit initial model
gbm_fit = gbm(as.formula(bmodel), data = training,
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



###How many trees should we use?
#best.iter = gbm.perf(gbm_fit, method = "cv")
#test_prob_rf = predict(rf_model, testing, type="prob")

testing$probability = predict(gbm_fit, testing, n.trees = num_trees, type = "response")
#probability of Y

# # Modify the probability threshold to see if you can get a better accuracy
testing$prediction = "N"
testing[testing$probability >= 0.5, "prediction"] = "Y"
testing$default_binary = as.factor(testing$default_binary)
testing$prediction = as.factor(testing$prediction)


#predictions for test set
boost_confusion = confusionMatrix(testing$prediction, testing$default, positive="Y")

boost_confusion$byClass["F1"] #0.5251608
boost_confusion

# Prediction    N    Y
# N 4960  967
# Y  295  707
# 
# Accuracy : 0.8179          
# 95% CI : (0.8086, 0.8269)
# No Information Rate : 0.7584          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4242          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.4223          
#             Specificity : 0.9439          
#          Pos Pred Value : 0.7056          
#          Neg Pred Value : 0.8368          
#              Prevalence : 0.2416          
#          Detection Rate : 0.1020          
#    Detection Prevalence : 0.1446          
#       Balanced Accuracy : 0.6831          
#                                           
#        'Positive' Class : Y    

#plot roc curve
test_auc = auc(testing$default, testing$probability)
test_auc #Area under the curve: 0.8034
plot(roc(testing$default, testing$probability))

#show partial dependencies
plot(gbm_fit, i="PAY_PC1")
plot(gbm_fit, i="AGE:LIMIT_BAL")
plot(gbm_fit, i="AGE:EDUCATION")
plot(gbm_fit, i="AMT_PC1")
#plot(gbm_fit, i="EDUCATION")

#----------------------------------------------
#   Predict on validation file
#----------------------------------------------

ovalid = read.csv("AT3_credit_test_STUDENT.csv")

# #------------------
# # check data
# #------------------

str(ovalid)

ovalid$EDUCATION = as.factor(ovalid$EDUCATION)
ovalid$MARRIAGE = as.factor(ovalid$MARRIAGE)
ovalid$SEX = as.factor(ovalid$SEX)

nrow(ovalid)     #6899
ggplot(data.frame(ovalid$LIMIT_BAL), aes(x="Credit Limit", y=ovalid$LIMIT_BAL)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("$")
hist(ovalid$LIMIT_BAL, xlab="Credit Limit", main="Histogram of Credit Limit")
ggplot(data.frame(ovalid$AGE), aes(x="Age", y=ovalid$AGE)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AGE), aes(x=ovalid$AGE)) + geom_bar() + xlab("Age")
#some outliers in age - over 120 years of age

ggplot(data.frame(ovalid$SEX), aes(x=ovalid$SEX)) + geom_bar() + xlab("Sex")
#one record of cat, dog, dolphin - need to change to NA

ovalid$SEX = as.character(ovalid$SEX)
ovalid$SEX[ovalid$SEX=="cat"] = "Other"
ovalid$SEX[ovalid$SEX=="dog"] = "Other"
ovalid$SEX[ovalid$SEX=="dolphin"] = "Other"
#write.csv(ovalid, "test.csv")
ovalid$SEX = as.factor(ovalid$SEX)
#ovalid = droplevels(ovalid)

ggplot(data.frame(ovalid$EDUCATION), aes(x=ovalid$EDUCATION)) + geom_bar() + xlab("Education")
ggplot(data.frame(ovalid$MARRIAGE), aes(x=ovalid$MARRIAGE)) + geom_bar() + xlab("Marriage")
ggplot(data.frame(ovalid$PAY_PC1), aes(x="Pay PC1", y=ovalid$PAY_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$PAY_PC2), aes(x="Pay PC2", y=ovalid$PAY_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$PAY_PC3), aes(x="Pay PC3", y=ovalid$PAY_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AMT_PC1), aes(x="Amt PC1", y=ovalid$AMT_PC1)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AMT_PC2), aes(x="Amt PC2", y=ovalid$AMT_PC2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AMT_PC3), aes(x="Amt PC3", y=ovalid$AMT_PC3)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AMT_PC4), aes(x="Amt PC4", y=ovalid$AMT_PC4)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AMT_PC5), aes(x="Amt PC5", y=ovalid$AMT_PC5)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AMT_PC6), aes(x="Amt PC6", y=ovalid$AMT_PC6)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$AMT_PC7), aes(x="Amt PC7", y=ovalid$AMT_PC7)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data.frame(ovalid$default), aes(x=ovalid$default)) + geom_bar() + xlab("Default")

hist(ovalid$PAY_PC1, xlab="PAY_PC1", main="Histogram of PAY_PC1")
hist(ovalid$PAY_PC2, xlab="PAY_PC2", main="Histogram of PAY_PC2")
hist(ovalid$PAY_PC3, xlab="PAY_PC3", main="Histogram of PAY_PC3")

hist(ovalid$AMT_PC1, xlab="AMT_PC1", main="Histogram of AMT_PC1")
hist(ovalid$AMT_PC2, xlab="AMT_PC2", main="Histogram of AMT_PC2")
hist(ovalid$AMT_PC3, xlab="AMT_PC3", main="Histogram of AMT_PC3")
hist(ovalid$AMT_PC4, xlab="AMT_PC4", main="Histogram of AMT_PC4")
hist(ovalid$AMT_PC5, xlab="AMT_PC5", main="Histogram of AMT_PC5")
hist(ovalid$AMT_PC6, xlab="AMT_PC6", main="Histogram of AMT_PC6")
hist(ovalid$AMT_PC7, xlab="AMT_PC7", main="Histogram of AMT_PC7")


prop.table(table(ovalid$SEX))
prop.table(table(ovalid$EDUCATION))
prop.table(table(ovalid$MARRIAGE))


# #------------------
# # write predictions to file
# #------------------


validation_out = NULL 
validation_out$ID = ovalid$ID
validation_out$prob = predict(gbm_fit, ovalid, n.trees = num_trees, type = "response")
validation_out$default = 0

validation_out = as.data.frame(validation_out)

validation_out[validation_out$prob >= 0.5, "default"] = 1
validation_out$default = as.factor(validation_out$default)

rownames(validation_out) = c()

output = validation_out[,c("ID","default")]

write.csv(output, "AT3_DAM_IT_Boost_0602.csv", row.names = FALSE)

#---------------------- END ---------------------------



