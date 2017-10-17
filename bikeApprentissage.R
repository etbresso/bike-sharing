<<<<<<< HEAD
dataload <- read.table("day.csv", header=TRUE, sep=",")
dataloadhour <- read.table("hour.csv", header=TRUE, sep=",")


train=dataloadhour[1:15000,]
test=dataloadhour[15001:17379,]

test$registered=0
test$casual=0
test$cnt=0
data=rbind(train,test)

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
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(randomForest)

d=rpart(registered~hr,data=train)
fancyRpartPlot(d)

d=rpart(casual~hr,data=train)
fancyRpartPlot(d)

d=rpart(casual~temp,data=train)
fancyRpartPlot(d)


d=rpart(registered~temp,data=train)
fancyRpartPlot(d)



data=rbind(train,test)

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
data$dp_cas[data$hr< 9 ]=1
data$dp_cas[data$hr>9 & data$hr<10 ]=2
data$dp_cas[data$hr>=10 & data$hr< 20 ]=3
data$dp_cas[data$hr>= 20 ]=4


data$temp_cas=""
data$temp_cas=0
data$temp_cas[data$temp< 0.41 ]=1
data$temp_cas[data$temp<0.63 & data$temp>= 0.41 ]=2
data$temp_cas[data$temp>=0.63 & data$temp< 0.71 ]=3
data$temp_cas[data$temp>= 0.71 ]=4

data$temp_reg=""
data$temp_reg=0
data$temp_reg[data$temp< 0.39 ]=1
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

train=data[1:15000,]
test=data[15001:17379,]

train$hr=as.factor(train$hr)
test$hr=as.factor(test$hr)

train$weathersit=as.factor(train$weathersit)
test$weathersit=as.factor(test$weathersit)


train$season=as.factor(train$season)
test$season=as.factor(test$season)

train$holiday=as.factor(train$holiday)
test$holiday=as.factor(test$holiday)


train$workingday=as.factor(train$workingday)
test$workingday=as.factor(test$workingday)

train$mnth=as.factor(train$mnth)
test$mnth=as.factor(test$mnth)

train$weekday=as.factor(train$weekday)
test$weekday=as.factor(test$weekday)

train$weekend=as.factor(train$weekend)
test$weekend=as.factor(test$weekend)


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

set.seed(415)
fit2 <- randomForest(logcas ~ hr+weekday+hum+atemp+temp_cas+windspeed+season+weathersit+holiday+workingday+dp_cas+weekend+year_part, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2

test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$cnt=test$casual+test$registered

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
=======
dataload <- read.table("day.csv", header=TRUE, sep=",")
dataloadhour <- read.table("hour.csv", header=TRUE, sep=",")


train=dataloadhour[1:15000,]
test=dataloadhour[15001:17379,]

test$registered=0
test$casual=0
test$cnt=0
data=rbind(train,test)

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
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(randomForest)

d=rpart(registered~hr,data=train)
fancyRpartPlot(d)

d=rpart(casual~hr,data=train)
fancyRpartPlot(d)

d=rpart(casual~temp,data=train)
fancyRpartPlot(d)


d=rpart(registered~temp,data=train)
fancyRpartPlot(d)



data=rbind(train,test)

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
data$dp_cas[data$hr< 9 ]=1
data$dp_cas[data$hr>9 & data$hr<10 ]=2
data$dp_cas[data$hr>=10 & data$hr< 20 ]=3
data$dp_cas[data$hr>= 20 ]=4


data$temp_cas=""
data$temp_cas=0
data$temp_cas[data$temp< 0.41 ]=1
data$temp_cas[data$temp<0.63 & data$temp>= 0.41 ]=2
data$temp_cas[data$temp>=0.63 & data$temp< 0.71 ]=3
data$temp_cas[data$temp>= 0.71 ]=4

data$temp_reg=""
data$temp_reg=0
data$temp_reg[data$temp< 0.39 ]=1
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

train=data[1:15000,]
test=data[15001:17379,]

train$hr=as.factor(train$hr)
test$hr=as.factor(test$hr)

train$weathersit=as.factor(train$weathersit)
test$weathersit=as.factor(test$weathersit)


train$season=as.factor(train$season)
test$season=as.factor(test$season)

train$holiday=as.factor(train$holiday)
test$holiday=as.factor(test$holiday)


train$workingday=as.factor(train$workingday)
test$workingday=as.factor(test$workingday)

train$mnth=as.factor(train$mnth)
test$mnth=as.factor(test$mnth)

train$weekday=as.factor(train$weekday)
test$weekday=as.factor(test$weekday)

train$weekend=as.factor(train$weekend)
test$weekend=as.factor(test$weekend)


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

set.seed(415)
fit2 <- randomForest(logcas ~ hr+weekday+hum+atemp+temp_cas+windspeed+season+weathersit+holiday+workingday+dp_cas+weekend+year_part, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2

test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$cnt=test$casual+test$registered

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
>>>>>>> 54db6c9509422090b985078529ecbc110ea76ff0
print(fit2)