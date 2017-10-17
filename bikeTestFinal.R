library(randomForest)

dataTest <- read.table("data.csv", header=TRUE, sep=",")

dataload <- read.table("day.csv", header=TRUE, sep=",")
dataloadhour <- read.table("hour.csv", header=TRUE, sep=",")

train=dataloadhour[1:17379,]

dataTest$registered=0
dataTest$casual=0
dataTest$cnt=0

data=rbind(train,dataTest)

data$season=as.factor(data$season)
data$weathersit=as.factor(data$weathersit)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$hr=as.factor(data$hr)
data$hr=as.integer(data$hr) # convert hour to integer

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
data$temp_reg[data$temp<0.29]=1
data$temp_reg[data$temp>=0.29 & data$temp<0.47]=2
data$temp_reg[data$temp>=0.47 & data$temp<0.71]=3
data$temp_reg[data$temp>=0.71]=4

data$year_part=1
data$year_part[data$mnth>3]=2
data$year_part[data$mnth>6]=3
data$year_part[data$mnth>9]=4

data$weekend=0
data$weekend[data$weekday==0 | data$weekday==6 ]=1


data$logreg=log(data$registered + 1)
data$logcas=log(data$casual + 1)

data$hr=as.factor(data$hr)

data$weathersit=as.factor(data$weathersit)


data$season=as.factor(data$season)

data$holiday=as.factor(data$holiday)


data$workingday=as.factor(data$workingday)

data$mnth=as.factor(data$mnth)

data$weekday=as.factor(data$weekday)

data$weekend=as.factor(data$weekend)

train=data[1:17379,]
dataTest=data[17380:17499,]


data$hr=as.factor(data$hr)

data$weathersit=as.factor(data$weathersit)


data$season=as.factor(data$season)

data$holiday=as.factor(data$holiday)


data$workingday=as.factor(data$workingday)

data$mnth=as.factor(data$mnth)

data$weekday=as.factor(data$weekday)

data$weekend=as.factor(data$weekend)


levels(dataTest$hr) <- levels(train$hr)
levels(dataTest$workingday) <- levels(train$workingday)
levels(dataTest$weekday) <- levels(train$weekday)
levels(dataTest$holiday) <- levels(train$holiday)
levels(dataTest$temp_reg) <- levels(train$temp_reg)
levels(dataTest$hum) <- levels(train$hum)
levels(dataTest$atemp) <- levels(train$atemp)
levels(dataTest$season) <- levels(train$season)
levels(dataTest$weathersit) <- levels(train$weathersit)
levels(dataTest$windspeed) <- levels(train$windspeed)
levels(dataTest$dp_reg) <- levels(train$dp_reg)
levels(dataTest$weekend) <- levels(train$weekend)
levels(dataTest$yr) <- levels(train$yr)
levels(dataTest$year_part) <- levels(train$year_part)

#predicting the log of registered users.
set.seed(415)
fit1 = readRDS(file = "fit1.rds")

pred1=predict(fit1,dataTest)
dataTest$logreg=pred1

set.seed(415)
fit2 = readRDS(file = "fit2.rds")
pred2=predict(fit2,dataTest)
dataTest$logcas=pred2

dataTest$registered=exp(dataTest$logreg)-1
dataTest$casual=exp(dataTest$logcas)-1
dataTest$cnt=dataTest$casual+dataTest$registered

s<-data.frame(instant=dataTest$instant,dteday=dataTest$dteday, test_registered=dataTest$registered, test_casual=dataTest$casual, test_count=dataTest$cnt)
write.csv(s,file="submit.csv",row.names=FALSE)

result<-data.frame(dteday=dataTest$dteday, hr=dataTest$hr,casual=dataTest$casual,registered=dataTest$registered,cnt=dataTest$cnt)
write.csv(result,file="result.csv",row.names=FALSE)

#result <- read.table("submit.csv", header=TRUE, sep=",")

