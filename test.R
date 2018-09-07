library(Metrics)
library(lattice) 
library(corpcor)
library(ggplot2)
library(car)
library(mice)
library(missForest)
library(psych)
library(psy)
library(stats)
library(rpart)
library(VIM)
library(caret)

data1 <- read.csv("C:\\Users\\sudee\\Desktop\\R\\6. Hackathon\\1522419498_DSA_Hackathon_Dataset.csv")
summary(data1)
traindata<- data1[1:12999,]
testdata<- data1[13000:nrow(data1),]
summary(traindata)
test_price<- data1$price[13000:nrow(data1)]

validation <-read.csv("C:\\Users\\sudee\\Desktop\\R\\6. Hackathon\\1522419497_DSA_Hackathon_Validation_Dataset.csv")
val_pri<- validation$price

is.na(traindata)
traindata$bedrooms[traindata$bedrooms>6]<- NA
traindata$bathrooms[traindata$bathrooms>4]<- NA


boxplot(traindata$price,horizontal = T)
boxplot(traindata$bedrooms,horizontal = T)
boxplot(traindata$bathrooms,horizontal = T)
boxplot(traindata$sqft_living,horizontal = T)
boxplot(traindata$sqft_lot,horizontal = T)
boxplot(traindata$floors,horizontal = T)
boxplot(traindata$waterfront,horizontal = T)
boxplot(traindata$view,horizontal = T)
boxplot(traindata$condition,horizontal = T)
boxplot(traindata$grade,horizontal = T)
boxplot(traindata$sqft_above,horizontal = T)
boxplot(traindata$sqft_basement,horizontal = T)
boxplot(traindata$yr_built,horizontal = T)
boxplot(traindata$yr_renovated,horizontal = T)
boxplot(traindata$zipcode,horizontal = T)


mice_plot <- aggr(traindata, col=c('grey','red'),numbers=TRUE, sortVars=TRUE,labels=names(traindata), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))



impute_train<- mice(traindata,method = 'cart', m=3, maxit = 10)
train1<-complete(impute_train,1)
train2<-complete(impute_train,2)
train3<-complete(impute_train,3)

cor2pcor(cor(train1))
cor(train1)

model<- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+factor(waterfront)+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+zipcode, data = traindata)
model1<- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+factor(waterfront)+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+zipcode, data = train1)
model2<- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+zipcode, data = train2)
model3<- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+factor(waterfront)+condition+grade+sqft_above+sqft_basement, data = train3)
model4<- lm(price~bedrooms+bathrooms+sqft_living+floors+factor(waterfront)+view+grade+sqft_above+sqft_basement, data = train1)

par(mfrow=c(2,2))

plot(model)
plot(model1)
plot(model2)
plot(model3)
plot(model4)

par(mfrow=c(1,2))
hist(train1$price,freq = T,plot = T,labels = T,col = 'blue')
dat=data.frame(log(train1$price),train1)
hist(dat$log.train1.price.,freq = T,plot = T,labels = T,col = 'blue')

par(mfrow=c(2,2))
model5<- lm(log.train1.price.~bedrooms+bathrooms+sqft_living+factor(waterfront)+floors+grade+sqft_above+sqft_basement, data = dat)
plot(model5)

confint(model5)
residualPlots(model5) 


influence.measures(model5)
influenceIndexPlot(model5, id.n=3)


newdata<-dat[c(-12778,-4025),]
model6<- lm(log.train1.price.~bedrooms+bathrooms+sqft_living+floors+factor(waterfront)+view+grade+sqft_above+sqft_basement, data = newdata)
influenceIndexPlot(model6)
plot(model6)
residualPlot(model6)

confint(model6)

prediction <- predict(model6,testdata,type = 'response')
prediction
pre<-exp(prediction)
summary(prediction)
pre
summary(pre)

#validation
prediction2<- predict(model6,validation,type = 'response')
prediction2
val1<-exp(prediction2)
rmse(val1,val_pri)
mape(val1,val_pri)
mape(pre,testdata)

