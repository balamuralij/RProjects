library(tidyverse)
library(miscset)

df_tel <- read.csv("C:/Users/bjanarth/Documents/DataScience/Science Projects/TelcoChurn.csv")
#structure
str(df_tel)
#dimensions
dim_desc(df)
#names
names(df)
#datatypes
glimpse(df_tel)

df_tel <- df_tel %>% mutate_if(is.character, as.factor)
df_tel$SeniorCitizen <- as.factor(df_tel$SeniorCitizen)
glimpse(df_tel)

str(df)

df_tel %>% map(~ sum(is.na(.)))

#impute

df_tel <- df_tel %>% mutate(TotalCharges = replace(TotalCharges,is.na(TotalCharges),median(TotalCharges,na.rm = T)))

#check if missing values are there
sum(is.na(df_tel$TotalCharges))


library(psych)


describe(df_tel)

colSums(is.na(df_tel))

df_tel_tbl <- df_tel %>% select_if(is.factor) %>% summarise_all(n_distinct)

#Data Analysis
#Gender
ggplot(df_tel) + geom_bar(aes(x=gender, fill=Churn),position = "dodge")

df_tel %>% group_by(gender,Churn) %>% summarise(n=n())

#Senior Citizen - Churn More
ggplot(df_tel) + geom_bar(aes(x=SeniorCitizen, fill=Churn),position = "dodge")

df_tel %>% group_by(SeniorCitizen) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))


df_tel %>% group_by(SeniorCitizen,Churn) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))


#partner - No Partners Churn More
ggplot(df_tel) + geom_bar(aes(x=Partner, fill=Churn),position = "dodge")

df_tel %>% group_by(Partner) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))

df_tel %>% group_by(Partner,Churn) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))

#dependents - No Dependents churn More
ggplot(df_tel) + geom_bar(aes(x=Dependents, fill=Churn),position = "dodge")


df_tel %>% group_by(Dependents) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))
df_tel %>% group_by(Dependents,Churn) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))

#box Plots
ggplot(df_tel,aes(x=SeniorCitizen,y=TotalCharges)) + geom_boxplot()

ggplot(df_tel,aes(x=Partner,y=TotalCharges)) + geom_boxplot()

ggplot(df_tel,aes(x=Dependents,y=TotalCharges)) + geom_boxplot()


#Compare against Total Charges

#Senior Citizen

df_tel %>% select(SeniorCitizen,Churn,TotalCharges,tenure) %>% filter(SeniorCitizen == 1,Churn == "Yes")  %>%
  summarize(n=n(),total = sum(TotalCharges),avg_tenure = sum(tenure)/n)


#No Partners
df_tel %>% select(Partner,Churn,TotalCharges,tenure) %>% filter(Partner == "No",Churn == "Yes")  %>%
  summarize(n=n(),total = sum(TotalCharges),avg_tenure = sum(tenure)/n)

#No Dependents   ( High Total Charges)
df_tel %>% select(Dependents,Churn,TotalCharges,tenure) %>% filter(Dependents == "No",Churn == "Yes")  %>%
  summarize(n=n(),total = sum(TotalCharges),avg_tenure = sum(tenure)/n)


#get No Dependents
dependents_tel <- df_tel %>% filter(Dependents == "No")

ggplotGrid(ncol=2,
           lapply(c("PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup",
                    "DeviceProtection"),
                  function(col){
                    ggplot(dependents_tel,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge") 
                  }))


ggplotGrid(ncol=2,
           lapply(c("TechSupport","StreamingTV","StreamingMovies","Contract",
                    "PaperlessBilling","PaymentMethod"),
                  function(col){
                    ggplot(dependents_tel,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
                  }))

ggplot(dependents_tel)+geom_bar(aes(x=PaymentMethod,fill=Churn),position = "dodge")




library(caret)


#remove customer id
df_tel <- df_tel %>% select(-customerID)

#train and test split 75 25

set.seed(5)
inTrain_tel <- createDataPartition(y = df_tel$Churn, p=0.75,list = FALSE)
train_tel <- df_tel[inTrain_tel,]
test_tel  <- df_tel[-inTrain_tel,]

library(e1071)
#class bias
table(df_tel$Churn)
#fit logistic regression
fit_tel <- glm(Churn~.,data=train_tel,family = binomial)

summary(fit_tel)
av_tel <- anova(fit_tel)
#prediction
churn_tel.probs <- predict(fit_tel,test_tel,type = "response")
churn_tel.probs

library(InformationValue)
library(smbinning)
library(VIF)
vif(fit_tel)
head(churn_tel.probs)
df_tel$Churn
contrasts(df_tel$Churn)
contrasts
summary(fit)

glm.pred_tel = rep("No",length(churn_tel.probs))
telftr = as.factor(test_tel$Churn)
glm.pred_tel[churn_tel.probs > 0.5] = "Yes"
glm.pred_tel = as.factor(glm.pred_tel)

confusionMatrix(glm.pred_tel,telftr, positive = "Yes")


glm.pred_telN = rep("No",length(churn_tel.probs))
telftr = as.factor(test_tel$Churn)
glm.pred_telN[churn_tel.probs > 0.5] = "Yes"
plotROC(test_tel$Churn,glm.pred_telN)

