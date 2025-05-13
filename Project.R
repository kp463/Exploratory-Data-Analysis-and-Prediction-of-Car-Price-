rm(list=ls())
setwd("D:/ISI/Sem-1/Deepayan sir")
C=read.csv(file="CarPricedata.csv")
attach(C)
E=C[,-c(1,2,26)]
y=log(price)
Z=cbind(E,price,y)
D=cbind(E,y)
attach(D)
View(D)
attach(Z)
View(Z)

#EDA

# Missing Values Check
any(is.na(C))

# Checking Outliers
par(mfrow=c(4,4))
boxplot(C[,10],main="wheelbase",col="skyblue")
boxplot(C[,11],main="carlength",col="skyblue")
boxplot(C[,12],main="carwidth",col="skyblue")
boxplot(C[,13],main="carheight",col="skyblue")
boxplot(C[,14],main="curbweight",col="skyblue")
boxplot(C[,17],main="enginesize",col="skyblue")
boxplot(C[,19],main="boreratio",col="skyblue")
boxplot(C[,20],main="stroke",col="skyblue")
boxplot(C[,21],main="compressionratio",col="skyblue")
boxplot(C[,22],main="horsepower",col="skyblue")
boxplot(C[,23],main="peakrpm",col="skyblue")
boxplot(C[,24],main="citympg",col="skyblue")
boxplot(C[,25],main="highwaympg",col="skyblue")
boxplot(C[,26],main="car price",col="skyblue")
par(mfrow=c(1,1))

# Checking collinearity
library(dplyr)
library(reshape)
library(ggplot2)
corr = data.matrix(cor(D[sapply(D,is.numeric)])) 
mel = melt(corr)
ggplot(mel, aes(X1,X2))+geom_tile(aes(fill=value)) +
  geom_text(aes(label = round(value, 1)))+
  scale_fill_gradient2(low='yellow',mid = 'white' ,high='green')

# Observing the relation of continuous variables with car price using scatterplot
par(mfrow=c(3,2))
plot(wheelbase,price,xlab="wheelbase",ylab="car price",pch=19,col="blue")
plot(carlength,price,xlab="carlength",ylab="car price",pch=19,col="blue")
plot(carwidth,price,xlab="carwidth",ylab="car price",pch=19,col="blue")
plot(carheight,price,xlab="carheight",ylab="car price",pch=19,col="blue")
plot(curbweight,price,xlab="curbweight",ylab="car price",pch=19,col="blue")
plot(enginesize,price,xlab="enginesize",ylab="car price",pch=19,col="blue")
par(mfrow=c(1,1))
par(mfrow=c(3,3))
plot(boreratio,price,xlab="boreratio",ylab="car price",pch=19,col="blue")
plot(stroke,price,xlab="stroke",ylab="car price",pch=19,col="blue")
plot(compressionratio,price,xlab="compressionratio",ylab="car price",pch=19,col="blue")
plot(horsepower,price,xlab="horsepower",ylab="car price",pch=19,col="blue")
plot(peakrpm,price,xlab="peakrpm",ylab="car price",pch=19,col="blue")
plot(citympg,price,xlab="citympg",ylab="car price",pch=19,col="blue")
plot(highwaympg,price,xlab="highwaympg",ylab="car price",pch=19,col="blue")
par(mfrow=c(1,1))

# Observing the relation of categorical variables with car price using boxplot
par(mfrow=c(3,3))
boxplot(price~factor(fuelsystem),data=C,col="yellow")
boxplot(price~factor(enginelocation),data=C,col="yellow")
boxplot(price~factor(drivewheel),data=C,col="yellow")
boxplot(price~factor(carbody),data=C,col="yellow")
boxplot(price~factor(fueltype),data=C,col="yellow")
boxplot(price~factor(doornumber),data=C,col="yellow")
boxplot(price~factor(aspiration),data=C,col="yellow")
boxplot(price~factor(CarName),data=C,col="yellow")
boxplot(price~factor(cylindernumber),data=C,col="yellow")
par(mfrow=c(1,1))

# Observing the skewness of variables
N=D[sapply(D,is.numeric)]
View(N)
ncol(N)
col.names=colnames(N)
col.names
par(mfrow=c(3,5))
for(i in 1:14)
{
  hist(N[,i],main=paste("Histogram of",col.names[i]),freq = F,xlab=col.names[i],
       col="green")
}

par(mfrow=c(1,1))

# Reporting values
library(moments)
R=matrix(0,nrow=14,ncol=3)
R[,1]=as.vector(apply(N,2,median))
R[,2]=as.vector(apply(N,2,IQR))
R[,3]=as.vector(apply(N,2,skewness))
colnames(R)=c("Median","IQR","Skewness")
rownames(R)=c("wheelbase","carlength","carwidth","carheight","curbweight","enginesize",
              "boreratio","stroke","compressionratio","horsepower","peakrpm","citympg",
              "highwaympg","price")
R
# Data Preparation
library(fastDummies)
data=dummy_cols(D,select_columns = c("CarName","fueltype","aspiration","doornumber",
                                     "carbody","drivewheel","enginelocation","enginetype",
                                     "cylindernumber","fuelsystem"),
                remove_first_dummy = T,
                  remove_selected_columns = T)
View(data)

# Breaking the dataset into train and test set
library(caTools)
set.seed(seed=2207)
sample=sample.split(data[,1],SplitRatio = 0.8)
train=subset(data,sample==T)
test=subset(data,sample==F)


hist(subset(C$price,sample==T),freq=F,col="green",main="Histogram of carprice of train set"
     ,xlab="car price")
hist(log(subset(C$price,sample==T)),freq=F,col="green",main="Histogram of carprice of train set"
     ,xlab="car price")
# Model fiiting

#Model-1
train1=train
View(train1)
s1=summary(lm(y~.,train1)) 
s1
library(readxl)
coeff.pval=data.frame(s1$coefficients)
t1=cbind(rownames(coeff.pval),coeff.pval)
writexl::write_xlsx(t1,'D:/ISI/Deepayan sir/t1.xlsx')
nrow(s1$coefficients)
library(car)
residualPlot(lm(train1$y~.,train1[-14]),pch=19)
shapiro.test(resid(lm(y~.,train1)))
ncvTest(lm(y~.,train1))
qqPlot(resid(lm(y~.,train1)),ylab="residual quantiles",main="Q-Q plot of Residuals")

#Model-2
train3=subset(train,select=-c(wheelbase,carlength,carwidth,horsepower,citympg,highwaympg,
                              enginesize,doornumber_two))
attach(train3)
s3=summary(lm(y~.,train3)) 
s3
library(readxl)
coeff.pval=data.frame(s3$coefficients)
t3=cbind(rownames(coeff.pval),coeff.pval)
writexl::write_xlsx(t3,'D:/ISI/Deepayan sir/t3.xlsx')
nrow(s3$coefficients)
library(car)
residualPlot(lm(y~.,train3),pch=19,main="")
shapiro.test(resid(lm(y~.,train3)))
ncvTest(lm(y~.,train3))
qqPlot(resid(lm(y~.,train3)),ylab="residual quantiles",pch=19)

#Model-3
train4=subset(train3,select=c(y,curbweight,CarName_bmw,`CarName_dodge `,`CarName_maxda `,
                             `CarName_mitsubishi `,`CarName_plymouth `,`CarName_toyota `,
                             carbody_wagon,enginelocation_rear,fuelsystem_mpfi,
                             `CarName_renault `,aspiration_turbo))
attach(train4)
s4=lm(y~.,train4)
summary(s4) 
residualPlot(lm(y~.,train4),
             main="Residual Plot vs Fitted Values of Response Variable",pch=19)
library(readxl)
coeff.pval=data.frame(summary(s4)$coefficients)
t4=cbind(rownames(coeff.pval),coeff.pval)
writexl::write_xlsx(t4,'D:/ISI/Deepayan sir/t4.xlsx')
nrow(s4$coefficients)
library(car)
residualPlot(lm(y~.,train4),pch=19)
shapiro.test(resid(lm(y~.,train4)))
ncvTest(lm(y~.,train4))
qqPlot(resid(lm(y~.,train4)),ylab="residual quantiles",pch=19,main="Q-Q Plot of Residuals")

#Prediction-------------
P=matrix(0,nrow=nrow(test),ncol=4)
P[,2]=predict(s4,newdata=test)
P[,1]=test$y
P[,3]=subset(C$price,sample==F)
P[,4]=floor(exp(P[,2]))
colnames(P)=c("Actual log(price)","Fitted log(price)","Actual Price","Fitted Price")
P
library(readxl)
writexl::write_xlsx(data.frame(P),'D:/ISI/Deepayan sir/P.xlsx')
SSE1=sum((P[,1]-P[,2])^2)
SSE1

#PRESS--------------
h1=hatvalues(s4)
e1=resid(s4)
PRESS1=sum((e1/(1-h1))^2)
PRESS1

#Lasso----------------
library(lattice)
library(glmnet)
str(train)
y1= train$y
X= model.matrix( ~ . - y - 1,train)
fm.lasso=cv.glmnet(X, y1, alpha = 1)
s.cv <- c(lambda.min = fm.lasso$lambda.min, lambda.1se = fm.lasso$lambda.1se)
s.cv
round(coef(fm.lasso, s = s.cv), 3)
#Choosing lambda
cv.lasso <- cv.glmnet(X, y1, alpha = 1, nfolds = 50)
plot(cv.lasso)

f1=glmnet(X, y1, alpha = 1)
plot(f1, xvar = "dev", label = TRUE)
#Fitting Model from Lasso---------
rownames(coef(fm.lasso, s = 'lambda.1se'))[coef(fm.lasso, s = 'lambda.1se')[,1]!= 0]
s5=lm(y~carwidth+curbweight+enginesize+horsepower+citympg+`CarName_audi `+CarName_bmw+
        `CarName_buick `+`CarName_mazda `+`CarName_saab `+`CarName_toyota `+
        carbody_hatchback+carbody_wagon+drivewheel_rwd+enginelocation_rear+
        cylindernumber_four+
      fuelsystem_2bbl+fuelsystem_mpfi,data=train)
summary(s5)
library(readxl)
coeff.pval=data.frame(summary(s5)$coefficients)
t5=cbind(rownames(coeff.pval),coeff.pval)
writexl::write_xlsx(t5,'D:/ISI/Deepayan sir/t5.xlsx')
h2=hatvalues(s5)
e2=resid(s5)
PRESS2=sum((e2/(1-h2))^2)
PRESS2

#Prediction---------------
Q=matrix(0,nrow=nrow(test),ncol=4)
Q[,1]=test$y
Q[,2]=predict(s5,newdata = test)
Q[,3]=subset(C$price,sample==F)
Q[,4]=floor(exp(Q[,2]))
colnames(Q)=c("Actual log(price)","Fitted log(price)","Actual Price","Fitted Price")
Q
writexl::write_xlsx(data.frame(Q),'D:/ISI/Deepayan sir/Q.xlsx')
SSE2=sum((Q[,1]-Q[,2])^2)
SSE2
residualPlot(s5,pch=19)
qqPlot(resid(s5),ylab="residual quantile",pch=19)
shapiro.test(resid(s5))
ncvTest(s5)



