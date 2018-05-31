mydata2 <- read.csv("C:/Users/bjanarth/Documents/DataScience/SMART/PolymerData.csv", header=TRUE,na.strings = 0)
read.csv("C:/Users/bjanarth/Documents/DataScience/SMART/PolymerData.csv",header=TRUE,na.strings = 0)
mydata1$Consumption.Qty[mydata$Consumption.Qty=='NA'] <- 6414
View(mydata2)
scale(mydata2,center=TRUE,scale=TRUE)
myscl = mydata2$Consumption.Qty
scale(myscl,center=TRUE,scale=TRUE)



mydata$Manufactured..Qty[mydata$Manufactured..Qty=="#NA"] <- 0
View(mydata)
summary(mydata2)

mydata2$material = as.factor(mydata2$material)
mydata2$WW = as.factor(mydata2$WW)

#Dropping dependent variable for calculating Multicollinearity
mdata2_a = subset(mydata2,select = -c(Consumption.Qty))
summary(mdata2_a)

#Identifying numeric variables
numericData <- mdata2_a[sapply(mdata2_a, is.numeric)]
View(numericData)

#Calculating Correlation
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)

#Build Linear Regression Model
fit = lm(Consumption.Qty ~ ., data=mydata2)
summary(fit)

anova(fit)

#Extracting R-squared Value
summary(fit)$r.squared
#Extracting Adjusted R-squared value
summary(fit)$adj.r.squared

par(mfrow=c(2,2))
plot(fit)

AIC(fit)
BIC(fit)


#Stepwise Selection based on AIC
library(MASS)
step <- stepAIC(fit, direction="both")
summary(step)

#Stepwise Selection with BIC
n = dim(mydata2)[1]
stepBIC = stepAIC(fit,k=log(n))
summary(stepBIC)


#Standardised coefficients
library(QuantPsyc)
lm.beta(stepBIC)


#See Predicted Value
pred = predict(stepBIC,mydata2)
View(pred)

#See Actual vs. Predicted Value
finaldata = cbind(mydata2,pred)
print(head(subset(finaldata, select = c(Consumption.Qty,pred))))
print(subset(finaldata, select = c(WW,Consumption.Qty,pred)))




#Calculating RMSE
rmse = sqrt(mean((mydata2$Consumption.Qty - pred)^2))
print(rmse)
