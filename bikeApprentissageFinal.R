library(randomForest)

dataload <- read.table("day.csv", header=TRUE, sep=",")
dataloadhour <- read.table("hour.csv", header=TRUE, sep=",")

train=dataloadhour[1:17379,]

data=rbind(train)

data$season=as.factor(data$season)
data$weathersit=as.factor(data$weathersit)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

train$hr=as.factor(train$hr)

train$hr=as.integer(train$hr) # convert hour to integer

data=rbind(train)

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

#Cr?ation de groupe de valeur d?fini ? partir de r?gle g?n?r?e par des arbres de d?cision
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
data$dp_cas[data$hr<= 9 ]=1
data$dp_cas[data$hr>=20]=2
data$dp_cas[data$hr>9 & data$hr< 12 ]=3
data$dp_cas[data$hr>=12 & data$hr< 20 ]=4


data$temp_cas=""
data$temp_cas=0
data$temp_cas[data$temp<0.35]=1
data$temp_cas[data$temp>=0.35 & data$temp<0.47 ]=2
data$temp_cas[data$temp>=0.47 & data$temp< 0.71 ]=3
data$temp_cas[data$temp>=0.71]=4

data$temp_reg=""
data$temp_reg=0
data$temp_reg[data$temp<0.29]=1
data$temp_reg[data$temp>=0.29 & data$temp<0.47]=2
data$temp_reg[data$temp>=0.47 & data$temp<0.71]=3
data$temp_reg[data$temp>=0.71]=4

data$year_part=1
data$year_part[data$mnth>3]=2
data$year_part[data$mnth>6]=3
data$year_part[data$mnth>9]=4
table(data$year_part)

data$weekend=0
data$weekend[data$weekday==0 | data$weekday==6 ]=1


data$logreg=log(data$registered + 1)
data$logcas=log(data$casual + 1)

#Les nouveaux attributs sont ajout?s aux donn?es
train=data[1:17379,]

train$hr=as.factor(train$hr)

train$weathersit=as.factor(train$weathersit)


train$season=as.factor(train$season)

train$holiday=as.factor(train$holiday)


train$workingday=as.factor(train$workingday)

train$mnth=as.factor(train$mnth)

train$weekday=as.factor(train$weekday)

train$weekend=as.factor(train$weekend)


#Apprentissage
set.seed(415)
fit1 <- randomForest(logreg ~ hr+workingday+weekday+holiday+temp_reg+hum+atemp+windspeed+season+weathersit+dp_reg+weekend+year_part, data=train,importance=TRUE, ntree=250)

set.seed(415)
fit2 <- randomForest(logcas ~ hr+weekday+hum+atemp+temp_cas+windspeed+season+weathersit+holiday+workingday+dp_cas+weekend+year_part, data=train,importance=TRUE, ntree=250)

saveRDS(fit1, "fit1.rds")
saveRDS(fit2, "fit2.rds")
