dataTest <- read.table("data.csv", header=TRUE, sep=",")

dataload <- read.table("day.csv", header=TRUE, sep=",")
dataloadhour <- read.table("hour.csv", header=TRUE, sep=",")



train=dataloadhour[1:15000,]
test=dataloadhour[15001:17379,]

dataTest$registered=0
dataTest$casual=0
dataTest$cnt=0

test$registered=0
test$casual=0
test$cnt=0
data=rbind(train,test,dataTest)

str(data)


table(is.na(data))

par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(data$season)
hist(data$weathersit)
hist(data$hum)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

prop.table(table(data$weathersit))

data$season=as.factor(data$season)
data$weathersit=as.factor(data$weathersit)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

boxplot(train$cnt~train$hr,xlab="hour", ylab="count of users")

boxplot(log(train$cnt)~train$hr,xlab="hour",ylab="log(count)")


boxplot(train$cnt~train$weekday,xlab="weekday",ylab="count of users")


sub=data.frame(train$registered,train$casual,train$cnt,train$temp,train$hum,train$atemp,train$windspeed)
cor(sub)

data$hr=as.factor(data$hr)

data$hr=as.integer(data$hr) # convert hour to integer


library(rpart)
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(RColorBrewer)
#library(randomForest)
#library(compare)
d=rpart(registered~hr,data=train)
fancyRpartPlot(d)


d=rpart(casual~hr,data=train)
fancyRpartPlot(d)

d=rpart(casual~temp,data=train)
fancyRpartPlot(d)

d=rpart(registered~temp,data=train)
fancyRpartPlot(d)

data$dp_reg=""
data$dp_reg=0
data$dp_reg[data$hr<8]=1
data$dp_reg[data$hr>=22]=2
data$dp_reg[data$hr>9 & data$hr<18]=3
data$dp_reg[data$hr==8]=4
data$dp_reg[data$hr==9]=5
data$dp_reg[data$hr==20 | data$hr==21]=6
data$dp_reg[data$hr==19 | data$hr==18]=7


data$dp_cas=""
data$dp_cas=0
data$dp_cas[data$hr<10 & data$hr< 9 ]=1
data$dp_cas[data$hr<10 & data$hr> 9 ]=2
data$dp_cas[data$hr>=10 & data$hr< 20 ]=3
data$dp_cas[data$hr>=10 & data$hr>= 20 ]=4


data$temp_cas=""
data$temp_cas=0
data$temp_cas[data$temp<0.63 & data$temp< 0.41 ]=1
data$temp_cas[data$temp<0.63 & data$temp>= 0.41 ]=2
data$temp_cas[data$temp>=0.63 & data$temp< 0.71 ]=3
data$temp_cas[data$temp>=0.63 & data$temp>= 0.71 ]=4

data$temp_reg=""
data$temp_reg=0
data$temp_reg[data$temp<0.69 & data$temp< 0.39 ]=1
data$temp_reg[data$temp<0.69 & data$temp>= 0.39 ]=2
data$temp_reg[data$temp>=0.69]=3

data$day_type=""
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

data$year_part=""
data$year_part[data$yr==0]=1
data$year_part[data$yr==0 & data$mnth>3]=2
data$year_part[data$yr==0 & data$mnth>6]=3
data$year_part[data$yr==0 & data$mnth>9]=4
data$year_part[data$yr==1]=5
data$year_part[data$yr==1 & data$mnth>3]=6
data$year_part[data$yr==1 & data$mnth>6]=7
data$year_part[data$yr==1 & data$mnth>9]=8
data$year_part[data$yr==2017]=1
data$year_part[data$yr==2017 & data$mnth>3]=2
data$year_part[data$yr==2017 & data$mnth>6]=3
data$year_part[data$yr==2017 & data$mnth>9]=4
table(data$year_part)

data$weekend=0
data$weekend[data$weekday==0 | data$weekday==6 ]=1


data$logreg=log(data$registered + 1)
data$logcas=log(data$casual + 1)

data$hr=as.factor(data$hr)

data$weathersit=as.factor(data$weathersit)


data$season=as.factor(data$season)

data$holiday=as.factor(data$holiday)


data$day_type=as.factor(data$day_type)
data$workingday=as.factor(data$workingday)

data$mnth=as.factor(data$mnth)

data$weekday=as.factor(data$weekday)

data$weekend=as.factor(data$weekend)

train=data[1:15000,]
test=data[15001:17379,]
dataTest=data[17380:17414,]


library(randomForest)
#predicting the log of registered users.
set.seed(415)

fit1 <- randomForest(logreg ~ hr +workingday+weekday+holiday+ day_type +temp_reg+hum+windspeed+season+weathersit+dp_reg+weekend+yr+year_part, data=train,importance=TRUE, ntree=250)
pred1=predict(fit1,test)
test$logreg=pred1

fit1 <- randomForest(logreg ~ hr+workingday+weekday+holiday+temp_reg+hum+windspeed+season+weathersit+weekend, data=train)

levels(test$hr) <- levels(train$hr)
levels(test$workingday) <- levels(train$workingday)
levels(test$weekday) <- levels(train$weekday)
levels(test$holiday) <- levels(train$holiday)
levels(test$temp_reg) <- levels(train$temp_reg)
levels(test$hum) <- levels(train$hum)
#levels(test$atemp) <- levels(train$atemp)
levels(test$season) <- levels(train$season)
levels(test$weathersit) <- levels(train$weathersit)
levels(test$dp_reg) <- levels(train$dp_reg)
levels(test$weekend) <- levels(train$weekend)
levels(test$yr) <- levels(train$yr)
levels(test$year_part) <- levels(train$year_part)
pred1=predict(fit1,test)
test$logreg=pred1


levels(dataTest$hr) <- levels(train$hr)
levels(dataTest$workingday) <- levels(train$workingday)
levels(dataTest$weekday) <- levels(train$weekday)
levels(dataTest$holiday) <- levels(train$holiday)
levels(dataTest$temp_reg) <- levels(train$temp_reg)
levels(dataTest$hum) <- levels(train$hum)
#levels(dataTest$atemp) <- levels(train$atemp)
levels(dataTest$season) <- levels(train$season)
levels(dataTest$weathersit) <- levels(train$weathersit)
levels(dataTest$dp_reg) <- levels(train$dp_reg)
levels(dataTest$weekend) <- levels(train$weekend)
#levels(dataTest$yr) <- levels(train$yr)
levels(dataTest$year_part) <- levels(train$year_part)
pred1Test=predict(fit1,dataTest)
dataTest$logreg=pred1Test

set.seed(415)
fit2 <- randomForest(logcas ~ hr+weekday+hum+atemp+temp_cas+windspeed+season+weathersit+holiday+workingday+dp_cas+weekend+year_part, data=train,importance=TRUE, ntree=5000)
pred2=predict(fit2,test)
test$logcas=pred2

pred2Test=predict(fit2,dataTest)
dataTest$logcas=pred2Test

test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$cnt=test$casual+test$registered

dataTest$registered=exp(dataTest$logreg)-1
dataTest$casual=exp(dataTest$logcas)-1
dataTest$cnt=dataTest$casual+dataTest$registered

compare=dataloadhour[15001:17379,] 
s<-data.frame(instant=test$instant,dteday=test$dteday, test_registered=test$registered, original_registed=compare$registered, test_casual=test$casual, original_casual=compare$casual, test_count=test$cnt, original_count=compare$cnt)
write.csv(s,file="submit.csv",row.names=FALSE)

result<-data.frame(dteday=dataTest$dteday, hr=dataTest$hr,casual=dataTest$casual,registered=dataTest$registered,cnt=dataTest$cnt)
write.csv(result,file="result.csv",row.names=FALSE)

erreur_registered=abs(compare$registered-test$registered)
erreur_casual=abs(compare$casual-test$casual)
erreur_cnt=abs(compare$cnt-test$cnt)

1 - sum((compare$registered-test$registered)^2)/sum((compare$registered-mean(compare$registered))^2)


1 - sum((compare$casual-test$casual)^2)/sum((compare$casual-mean(compare$casual))^2)


1 - sum((compare$cnt-test$cnt)^2)/sum((compare$cnt-mean(compare$cnt))^2)
fit1$importance[order(fit1$importance[, 1], decreasing = TRUE), ]
print(fit1)

psi1 <- mean((compare$registered - test$registered)^2)
sigma21 <- 1/2379 * var((compare$registered - test$registered)^2) 
# 95% CI:
c(psi1 - 1.96 * sqrt(sigma21), psi1, psi1 + 1.96 * sqrt(sigma21))


mean(erreur_cnt)
mean(erreur_registered)
mean(erreur_casual)

result <- read.table("submit.csv", header=TRUE, sep=",")

saveRDS(fit1, "fit1.rds")
saveRDS(fit2, "fit2.rds")

##################################################
dataTest <- read.table("data.csv", header=TRUE, sep=",")

dataload <- read.table("day.csv", header=TRUE, sep=",")
dataloadhour <- read.table("hour.csv", header=TRUE, sep=",")

train=dataloadhour[1:15000,]
test=dataloadhour[15001:17379,]

dataTest$registered=0
dataTest$casual=0
dataTest$cnt=0
data=rbind(train,test,dataTest)

str(data)

table(is.na(data))

par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(data$season)
hist(data$weathersit)
hist(data$hum)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

prop.table(table(data$weathersit))

data$season=as.factor(data$season)
data$weathersit=as.factor(data$weathersit)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

boxplot(train$cnt~train$hr,xlab="hour", ylab="count of users")

boxplot(log(train$cnt)~train$hr,xlab="hour",ylab="log(count)")


boxplot(train$cnt~train$weekday,xlab="weekday",ylab="count of users")


sub=data.frame(train$registered,train$casual,train$cnt,train$temp,train$hum,train$atemp,train$windspeed)
cor(sub)

train$hr=as.factor(train$hr)
test$hr=as.factor(test$hr)

train$hr=as.integer(train$hr) # convert hour to integer
test$hr=as.integer(test$hr) # modifying in both train and test data set

library(rpart)
#library(rattle) #these libraries will be used to get a good visual plot for the decision tree model.
#library(rpart.plot)
#library(RColorBrewer)
library(randomForest)
#library(compare)
#d=rpart(registered~hr,data=train)
#fancyRpartPlot(d)

#d=rpart(casual~hr,data=train)
#fancyRpartPlot(d)

#d=rpart(casual~temp,data=train)
#fancyRpartPlot(d)


#d=rpart(registered~temp,data=train)
#fancyRpartPlot(d)

data$dp_reg=""
data$dp_reg=0
data$dp_reg[data$hr<8]=1
data$dp_reg[data$hr>=22]=2
data$dp_reg[data$hr>9 & data$hr<18]=3
data$dp_reg[data$hr==8]=4
data$dp_reg[data$hr==9]=5
data$dp_reg[data$hr==20 | data$hr==21]=6
data$dp_reg[data$hr==19 | data$hr==18]=7


data$dp_cas=""
data$dp_cas=0
data$dp_cas[data$hr<10 & data$hr< 9 ]=1
data$dp_cas[data$hr<10 & data$hr> 9 ]=2
data$dp_cas[data$hr>=10 & data$hr< 20 ]=3
data$dp_cas[data$hr>=10 & data$hr>= 20 ]=4


data$temp_cas=""
data$temp_cas=0
data$temp_cas[data$temp<0.63 & data$temp< 0.41 ]=1
data$temp_cas[data$temp<0.63 & data$temp>= 0.41 ]=2
data$temp_cas[data$temp>=0.63 & data$temp< 0.71 ]=3
data$temp_cas[data$temp>=0.63 & data$temp>= 0.71 ]=4

data$temp_reg=""
data$temp_reg=0
data$temp_reg[data$temp<0.69 & data$temp< 0.39 ]=1
data$temp_reg[data$temp<0.69 & data$temp>= 0.39 ]=2
data$temp_reg[data$temp>=0.69]=3

#data$day_type=""
#data$day_type[data$holiday==0 & data$workingday==0]="weekend"
#data$day_type[data$holiday==1]="holiday"
#data$day_type[data$holiday==0 & data$workingday==1]="working day"

data$year_part=1
data$year_part[data$mnth>3]=2
data$year_part[data$mnth>6]=3
data$year_part[data$mnth>9]=4
table(data$year_part)

data$weekend=0
data$weekend[data$weekday==0 | data$weekday==6 ]=1


data$logreg=log(data$registered + 1)
data$logcas=log(data$casual + 1)


data$hr=as.factor(data$hr)
data$weathersit=as.factor(data$weathersit)
data$season=as.factor(data$season)
data$holiday=as.factor(data$holiday)
#data$day_type=as.factor(data$day_type)
data$workingday=as.factor(data$workingday)
data$mnth=as.factor(data$mnth)
data$weekday=as.factor(data$weekday)
data$weekend=as.factor(data$weekend)


train=data[1:15000,]
test=data[15001:17379,]
dataTest=data[17380:17499,]


#predicting the log of registered users.
set.seed(415)
fit1 <- randomForest(logreg ~ hr+workingday+weekday+holiday+temp_reg+hum+atemp+windspeed+season+weathersit+dp_reg+weekend+year_part, data=train,importance=TRUE, ntree=250)

levels(test$hr) <- levels(train$hr)
levels(test$workingday) <- levels(train$workingday)
levels(test$weekday) <- levels(train$weekday)
levels(test$holiday) <- levels(train$holiday)
levels(test$temp_reg) <- levels(train$temp_reg)
levels(test$hum) <- levels(train$hum)
levels(test$atemp) <- levels(train$atemp)
levels(test$season) <- levels(train$season)
levels(test$weathersit) <- levels(train$weathersit)
levels(test$dp_reg) <- levels(train$dp_reg)
levels(test$weekend) <- levels(train$weekend)
levels(test$yr) <- levels(train$yr)
levels(test$year_part) <- levels(train$year_part)


pred1=predict(fit1,test)
test$logreg=pred1

pred1New=predict(fit1,dataTest)
dataTest$logreg=pred1New

set.seed(415)
fit2 <- randomForest(logcas ~ hr+weekday+hum+atemp+temp_cas+windspeed+season+weathersit+holiday+workingday+dp_cas+weekend+year_part, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2


pred2New=predict(fit2,dataTest)
dataTest$logreg=pred2New

test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$cnt=test$casual+test$registered


dataTest$registered=exp(dataTest$logreg)-1
dataTest$casual=exp(dataTest$logcas)-1
dataTest$cnt=dataTest$casual+dataTest$registered

compare=dataloadhour[15001:17379,] 
s<-data.frame(instant=test$instant,dteday=test$dteday, test_registered=test$registered, original_registed=compare$registered, test_casual=test$casual, original_casual=compare$casual, test_count=test$cnt, original_count=compare$cnt)
write.csv(s,file="submit.csv",row.names=FALSE)



result<-data.frame(dteday=dataTest$dteday, hr=dataTest$hr,casual=dataTest$casual,registered=dataTest$registered,cnt=dataTest$cnt)
write.csv(result,file="result.csv",row.names=FALSE)

erreur_registered=abs(compare$registered-test$registered)
erreur_casual=abs(compare$casual-test$casual)
erreur_cnt=abs(compare$cnt-test$cnt)

mean(erreur_cnt)
mean(erreur_registered)
mean(erreur_casual)

result <- read.table("submit.csv", header=TRUE, sep=",")

1 - sum((compare$registered-test$registered)^2)/sum((compare$registered-mean(compare$registered))^2)


1 - sum((compare$casual-test$casual)^2)/sum((compare$casual-mean(compare$casual))^2)


1 - sum((compare$cnt-test$cnt)^2)/sum((compare$cnt-mean(compare$cnt))^2)
fit1$importance[order(fit1$importance[, 1], decreasing = TRUE), ]
print(fit1)
print(fit2)


saveRDS(fit1, "fit1.rds")
saveRDS(fit2, "fit2.rds")

