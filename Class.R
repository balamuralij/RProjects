c=5
c<-9
getwd()
?stringr
?getwd()
x
print(x)

set.seed(1)
df4 <- data.frame(Y = rnorm(15), Z = ceiling(rnorm(15)))

set.seed(1)
ifelse(sign(rnorm(15))==-1,0,1)

mydata = c(sample(LETTERS[1:5],16,replace = TRUE))
mydata1 = data.frame(sample(LETTERS[1:5],16,replace = TRUE))
df4[1:5,]

summary(df4)
summary(df4[1])
names(df4)
nrow(df4)
ncol(df4)
str(df4)
head(df4)
head(mydata)
head(mydata1)
library(dplyr)
sample_frac(mydata1, 0.1)
data()
library(dplyr)
install.packages(dplyr)
installed.packages(dplyr)
install.packages("dplyr")
install.packages("dplyr",dependencies )
library("dplyr")


mydata = C:\Users\bjanarth\Documents\DataScience\sampledata.csv
mydata = read.csv(file = "C:\\Users\\bjanarth\\Documents\\DataScience\\sampledata.csv")
d <- mydata
smple1 <- sample_n(mydata,3)
smplef1 <- sample_frac(mydata,0.1)
x1 = distinct(mydata)
view(x1)
x2 = distinct(mydata, Index, .keep_all= TRUE)
myselect = select(mydata, -Index, -State)
mydata7 = filter(mydata, Index == "A")
summarise(mydata, Y2015_mean = mean(Y2015), Y2015_med=median(Y2015))
library("data.table")
summarise(mydata)
numdata = mydata[sapply(mydata,is.numeric)]
summarise_all(numdata, funs(n(),mean,median))
?data.table
plot(mydata)
