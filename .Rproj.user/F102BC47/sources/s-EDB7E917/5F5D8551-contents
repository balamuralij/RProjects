library(tidyverse)
library(miscset)

df_tel <- read.csv("C:/Users/bjanarth/Documents/DataScience/Science Projects/TelcoChurn.csv")
str(df_tel)


#Conver Char to Factor
df_tel <- df_tel %>% mutate_if(is.character, as.factor)
str(df_tel)

#Convert int to Factor
df_tel$SeniorCitizen <- as.factor(df_tel$SeniorCitizen)

#Check for missing Values
df_tel %>% map(~ sum(is.na(.)))


#impute Missing Values for Total Charges with Median
df_tel <- df_tel %>% mutate(TotalCharges = replace(TotalCharges,is.na(TotalCharges),median(TotalCharges,na.rm = T)))

#Verify if Missing values are there 
sum(is.na(df_tel$TotalCharges))
colSums(is.na(df_tel))

##Prepare for Analysis
#remove customer id as it contains unique records
df_tel <- df_tel %>% select(-customerID)


#Check Class Bias
table(df_tel$Churn)
### Class Bias - True


############Test Data with Bias - Balanced

input_yes <- df_tel[which(df_tel$Churn == "Yes"),]
input_no  <- df_tel[which(df_tel$Churn == "No"),]
set.seed(100)

input_yes_training <- sample(1:nrow(input_yes),0.7*nrow(input_yes))
input_no_training <- sample(1:nrow(input_no),0.7*nrow(input_yes))

training_yes <- input_yes[input_yes_training,]
training_no  <- input_no[input_no_training,]
trainingdata_ub <- rbind(training_yes, training_no)

#Test Data
test_yes <- input_yes[-input_yes_training,]
test_no  <- input_no[-input_no_training,]
testdata_ub <- rbind(test_yes, test_no)

fit_tel <- glm(Churn~.,data=trainingdata_ub,family = binomial)
fit_tel
predicted <- predict(fit_tel,testdata_ub,type = "response")
predicted
summary(fit_tel)

df_tel$Churn
contrasts(df_tel$Churn)

predicted_positive = rep("No",length(predicted))
predicted_positive[predicted > 0.5] = "Yes"
predicted_positive = as.factor(predicted_positive)


library(caret)

telftr = as.factor(testdata_ub$Churn)
confusionMatrix(predicted_positive,telftr,positive="Yes")

################################################


#####################Test Data Biased  - AS - IS 
set.seed(5)
inTrain <- createDataPartition(y = df_tel$Churn, p=0.75, list=FALSE)

train_b <- df_tel[inTrain,]
test_b <- df_tel[-inTrain,]

#Fit the MOdel
fit_model <- glm(Churn~., data=train_b, family=binomial)
attributes(fit_model)$effects
#Prediction
predicted_model <- predict(fit_model, test_b, type="response")

predicted_model_adj = rep("No", length(predicted_model))
predicted_model_adj[predicted_model > 0.5] = "Yes"

predicted_model_adj = as.factor(predicted_model_adj)
test_fct = test_b$Churn
confusionMatrix(predicted_model_adj,test_fct,positive="Yes")


#feature importance
sort(varImp(fit_model),decreasing = TRUE)
######################################

###################################################
#Fit Model after Feature Selection
# fitting the model
fit_feature <- glm(Churn~SeniorCitizen + tenure + MultipleLines + InternetService + StreamingTV + Contract + PaperlessBilling + PaymentMethod + TotalCharges
           , data=train_b,
           family=binomial)
#prediction
predicted_post_feature <- predict(fit_feature, test_b, type="response")

predicted_post_feature_adj = rep("No", length(predicted_post_feature))
predicted_post_feature_adj[predicted_post_feature > 0.5] = "Yes"

predicted_post_feature_adj = as.factor(predicted_post_feature_adj)
test_fct = test_b$Churn
confusionMatrix(predicted_post_feature_adj,test_fct,positive="Yes")
#######################################################
library(ROCR)
pr <- prediction(predicted_post_feature, test_b$Churn)

# plot ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#AUC Value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###VAlidate Fitness
library(pscl)
pR2(fit_model)

#anova
anova(fit_model,fit_feature,test = "Chisq")

library(lmtest)
lrtest(fit_model,fit_feature)

###########
#Try Using Lasso
library(glmnet)
x <- model.matrix(Churn~.,train_b)
y <- ifelse(train_b$Churn=="Yes",1,0)

cv.out <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse" )
plot(cv.out)

lambda_min <- cv.out$lambda.min
lambda_1se <- cv.out$lambda.1se
coef(cv.out,s=lambda_1se)

x_test <- model.matrix(Churn~.,test_b)
#predict 
lasso_prod <- predict(cv.out,newx = x_test,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("No",nrow(test_b))
lasso_predict[lasso_prod>.5] <- "Yes"

table(pred=lasso_predict,true=test_b$Churn)
#accuracy
mean(lasso_predict==test_b$Churn)


###############################################################333
##Random Forest
library(randomForest)
rf_model = randomForest(Churn~., data = train_b, importance = T)
rf_model

rfpredicted <- predict(rf_model, test_b)
confusionMatrix(rfpredicted, test_b$Churn, positive = "Yes")
