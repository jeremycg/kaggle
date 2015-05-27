#first read in training data:

library(data.table)
setwd("C:/Users/jeremy/Desktop/kaggle/fbbid/")
bids<-fread("bids.csv")
train<-fread("train.csv")

mergeddata<-merge(bids,train,by="bidder_id")
#ok now we want some features!

#number of bids - total
totalbids<-mergeddata%>%group_by(bidder_id)%>%summarise(totalbids=n())

#number of bids - mean per auction
meanbids<-mergeddata%>%group_by(bidder_id,auction)%>%summarise(numberbids=n())
meanbids<-meanbids%>%group_by(bidder_id)%>%summarise(meanbids=mean(numberbids))


#auctions bid on
numauctions<-(mergeddata%>%group_by(bidder_id,auction)%>%summarise(numberbids=n()))[,.N,by=bidder_id] 
setnames(numauctions,c("bidder_id","N"),c("bidder_id","numauctions"))

#number of devices
numdevices<-(mergeddata%>%group_by(bidder_id,device)%>%summarise(n()))[,.N,by=bidder_id]
setnames(numdevices,c("bidder_id","N"),c("bidder_id","numdevices"))

#merchandise (number of types)
holding<-mergeddata%>%group_by(bidder_id,merchandise)%>%summarise(n())
numtypes<-holding%>%group_by(bidder_id)%>%summarise(numtypes=n())

#merchandise (most common)
holding<-mergeddata%>%group_by(bidder_id,merchandise)%>%summarise(number=n())
commonmerch<-holding%>%group_by(bidder_id)%>%summarise(commonmerch=merchandise[[which.max(.[["number"]])]])

#country (number)
holding<-mergeddata%>%group_by(bidder_id,country)%>%summarise(n())
numcountry<-holding%>%group_by(bidder_id)%>%summarise(numcountry=n())

#country (most common)
holding<-mergeddata%>%group_by(bidder_id,country)%>%summarise(number=n())
commoncountry<-holding%>%group_by(bidder_id)%>%summarise(country=country[[which.max(.[["number"]])]])

#number of bidders at address
#either I'm dumb, or there are no shared addresses
#numberaddress<-mergeddata%>%group_by(bidder_id)%>%summarise(number=length(unique(.[["address"]])))

#number of paymentaccounts per bidder
#holding<-mergeddata%>%group_by(bidder_id,payment_account)%>%summarise(n())
#numaccounts<-holding%>%group_by(bidder_id)%>%summarise(n())
#only ever one

#ok so we can deal with timing etc later, lets try combining em all and fitting a model.
fulldata<-join_all(list(train,totalbids,meanbids,numauctions,numdevices,
                   numtypes,commonmerch,numcountry,commoncountry
                   ), by = 'bidder_id', type = 'full')

fulldata<-as.data.frame(fulldata)
fulldata<-na.omit(fulldata)
predictors<-fulldata[5:12]
predictors$commonmerch<-as.factor(predictors$commonmerch)
predictors$country<-as.factor(predictors$country)
outcomes<-fulldata[,4]


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10)

#gradient boosting
gbmFit1 <- train(x=predictors,y=outcomes,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)
print(gbmFit1)

#hok, so lets predict
test<-fread("test.csv")
mergeddata<-merge(bids,test,by="bidder_id")
#getall the above features - just copy and paste
totalbids<-mergeddata%>%group_by(bidder_id)%>%summarise(totalbids=n())
meanbids<-mergeddata%>%group_by(bidder_id,auction)%>%summarise(numberbids=n())
meanbids<-meanbids%>%group_by(bidder_id)%>%summarise(meanbids=mean(numberbids))
numauctions<-(mergeddata%>%group_by(bidder_id,auction)%>%summarise(numberbids=n()))[,.N,by=bidder_id] 
setnames(numauctions,c("bidder_id","N"),c("bidder_id","numauctions"))
numdevices<-(mergeddata%>%group_by(bidder_id,device)%>%summarise(n()))[,.N,by=bidder_id]
setnames(numdevices,c("bidder_id","N"),c("bidder_id","numdevices"))
holding<-mergeddata%>%group_by(bidder_id,merchandise)%>%summarise(n())
numtypes<-holding%>%group_by(bidder_id)%>%summarise(numtypes=n())
holding<-mergeddata%>%group_by(bidder_id,merchandise)%>%summarise(number=n())
commonmerch<-holding%>%group_by(bidder_id)%>%summarise(commonmerch=.[[which.max(.[["number"]])]])
holding<-mergeddata%>%group_by(bidder_id,country)%>%summarise(n())
numcountry<-holding%>%group_by(bidder_id)%>%summarise(numcountry=n())
holding<-mergeddata%>%group_by(bidder_id,country)%>%summarise(number=n())
commoncountry<-holding%>%group_by(bidder_id)%>%summarise(country=country[[which.max(.[["number"]])]])
fulldata2<-join_all(list(test,totalbids,meanbids,numauctions,numdevices,
                        numtypes,commonmerch,numcountry,commoncountry
                        ), by = 'bidder_id', type = 'full')

#ok a couple hacks to make it work
#remove countries that didnt appear in training
fulldata2<-as.data.frame(fulldata2)
fulldata2<-na.omit(fulldata2)
fulldata2$country[!(fulldata2$country %in% fulldata$country)]<-"cn"
predictors2<-fulldata2[,4:11]
predictors2$commonmerch<-as.factor(predictors2$commonmerch)
predictors2$country<-as.factor(predictors2$country)

#predict!
x=cbind(as.data.frame(test),predict(gbmFit1,predictors2))
x$`predict(gbmFit1, predictors2)`[x$`predict(gbmFit1, predictors2)`<0]<-0
write.csv(x,"output.csv")
#gave me score of 0.87021
