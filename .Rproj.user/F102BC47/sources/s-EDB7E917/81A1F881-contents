library(tidyverse)
library(miscset)

df <- read.csv("C:/Users/bjanarth/Documents/DataScience/Science Projects/TelcoChurn.csv")
#structure
str(df)
#dimensions
dim_desc(df)
#names
names(df)
#datatypes
glimpse(df)

df <- df %>% mutate_if(is.character, as.factor)
df$SeniorCitizen <- as.factor(df$SeniorCitizen)
glimpse(df)

str(df)

df %>% map(~ sum(is.na(.)))

#impute

df <- df %>% mutate(TotalCharges = replace(TotalCharges,is.na(TotalCharges),median(TotalCharges,na.rm = T)))

#check if missing values are there
sum(is.na(df$TotalCharges))


library(psych)

describe(df)

colSums(is.na(df))

df_tbl <- df %>% select_if(is.factor) %>% summarise_all(n_distinct)

#Data Analysis
#Gender
ggplot(df) + geom_bar(aes(x=gender, fill=Churn),position = "dodge")

df %>% group_by(gender,Churn) %>% summarise(n=n())

#Senior Citizen - Churn More
ggplot(df) + geom_bar(aes(x=SeniorCitizen, fill=Churn),position = "dodge")

df %>% group_by(SeniorCitizen) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))


df %>% group_by(SeniorCitizen,Churn) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))


#partner - No Partners Churn More
ggplot(df) + geom_bar(aes(x=Partner, fill=Churn),position = "dodge")

df %>% group_by(Partner) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))

df %>% group_by(Partner,Churn) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))

#dependents - No Dependents churn More
ggplot(df) + geom_bar(aes(x=Dependents, fill=Churn),position = "dodge")


df %>% group_by(Dependents) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))
df %>% group_by(Dependents,Churn) %>% summarise(n=n()) %>% mutate(freq = n/sum(n))

#box Plots
ggplot(df,aes(x=SeniorCitizen,y=TotalCharges)) + geom_boxplot()

ggplot(df,aes(x=Partner,y=TotalCharges)) + geom_boxplot()

ggplot(df,aes(x=Dependents,y=TotalCharges)) + geom_boxplot()


#Compare against Total Charges

#Senior Citizen

df %>% select(SeniorCitizen,Churn,TotalCharges,tenure) %>% filter(SeniorCitizen == 1,Churn == "Yes")  %>%
  summarize(n=n(),total = sum(TotalCharges),avg_tenure = sum(tenure)/n)


#No Partners
df %>% select(Partner,Churn,TotalCharges,tenure) %>% filter(Partner == "No",Churn == "Yes")  %>%
  summarize(n=n(),total = sum(TotalCharges),avg_tenure = sum(tenure)/n)

#No Dependents   ( High Total Charges)
df %>% select(Dependents,Churn,TotalCharges,tenure) %>% filter(Dependents == "No",Churn == "Yes")  %>%
  summarize(n=n(),total = sum(TotalCharges),avg_tenure = sum(tenure)/n)


#get No Dependents
dependents <- df %>% filter(Dependents == "No")

ggplotGrid(ncol=2,
           lapply(c("PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup",
                    "DeviceProtection"),
                  function(col){
                    ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge") 
                                                                  }))


ggplotGrid(ncol=2,
           lapply(c("TechSupport","StreamingTV","StreamingMovies","Contract",
                    "PaperlessBilling","PaymentMethod"),
                  function(col){
                    ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
                  }))

ggplot(dependents)+geom_bar(aes(x=PaymentMethod,fill=Churn),position = "dodge")




library(caret)


#remove customer id
df <- df %>% select(-customerID)

#train and test split 75 25

set.seed(5)
inTrain <- createDataPartition(y = df$Churn, p=0.75,list = FALSE)
train <- df[inTrain,]
test  <- df[-inTrain,]
#fit logistic regression
fit <- glm(Churn~.,data=train,family = binomial)

#prediction
churn.probs <- predict(fit,test,type = "response")
head(churn.probs)
df_tel$Churn
contrasts(df$Churn)
summary(fit)

glm.pred = rep("No",length(churn.probs))
glm.pred
glm.pred[churn.probs > 0.5] = "Yes"
test$Churn
confusionMatrix(glm.pred,test$Churn, positive = "Yes")

glm.pred1 = rep("No", length(churn.probs))
glm.pred1[churn.probs > 0.5] = "Yes"
glm.chur = as.character(test$Churn)
confusionMatrix(glm.pred1, glm.chur, positive = "Yes")
