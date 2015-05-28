#first read in training data:

library(data.table)
library(plyr)
library(dplyr)
library(caret)
setwd("C:/Users/jeremy/Desktop/kaggle/kaggle/fbbid/")
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

#let's see about time since last bid
differences<-function(df){
  df<-df[order(df$time), ]
  df$time<-c(0,diff(df$time))
  df
}

timediffbids<-mergeddata%>%group_by(auction)%>%do(.,differences(.))

#now we need to do a couple things
#get mean time to bid per user
meantimes<-timediffbids%>%group_by(bidder_id)%>%summarise(averagetimetobid=mean(time))

#get sd
sdtimes<-timediffbids%>%group_by(bidder_id)%>%summarise(sdtimetobid=sd(time))
sdtimes$sdtimetobid[is.na(sdtimes$sdtimetobid)]<-0

#get % of first bidding
percentfirst<-timediffbids%>%group_by(bidder_id)%>%summarise(percentfirst=sum(.[["time"]]==0)/n())



#bid against self
mergeddata2<-mergeddata
mergeddata2$self<-0
bidself<-function(df){
  df$self<-0
  if(nrow(df)==1){
    return(df)}
  df<-df[order(df$time), ]
  df$prevbidder<-c("none",df$bidder_id[-nrow(df)])
  df$self[df$bidder_id==df$prevbidder]<-1
  df$prevbidder<-NULL
  df
}

bidagainstself<-mergeddata2%>%group_by(auction)%>%do(.,bidself(.))
bidagainstself<-bidagainstself%>%group_by(bidder_id)%>%summarise(percentself=sum(.[["self"]]==1)/n())

#number final bids
finalbids<-timediffbids%>%group_by(auction)%>%mutate(last = c(rep(0,n()-1), 1))
numfinal<-finalbids%>%group_by(bidder_id)%>%summarise(percentfinal=sum(last))

#percent of final bids of total
percentfinal<-finalbids%>%group_by(bidder_id)%>%summarise(percentfinal=mean(last))

#auctions bid on to auctions won
percentwon<-finalbids%>%group_by(bidder_id,auction)%>%summarise(percentfinal=sum(last))%>%
    group_by(bidder_id)%>%summarise(winpercent=mean(percentfinal))

#add in
#percent mismatched bids - how often do they bid with a weird search term
holding<-mergeddata%>%group_by(auction,merchandise)%>%summarise(number=n())
merchperauction<-holding%>%group_by(auction)%>%
  summarise(cmerchandise=merchandise[[which.max(.[["number"]])]])
holding<-merge(mergeddata,merchperauction,by="auction")
percentmismatch<-holding%>%group_by(bidder_id,auction)%>%summarise(mismatch=sum(merchandise==cmerchandise)/n())%>%
    group_by(bidder_id)%>%summarise(percentmismatch=mean(mismatch))
#percent switching search term - bidding with different term = maybe not bot
#zz<-mergeddata%>%group_by(bidder_id,auction)%>%distinct(.,"merchandise")%>%
#  group_by(bidder_id,auction)%>%summarise(n())
#appears to be none
#ok so we can join all the data
fulldata<-join_all(list(train,totalbids,meanbids,numauctions,numdevices,
                        numtypes,commonmerch,numcountry,commoncountry,meantimes,
                        sdtimes,percentfirst,bidagainstself,numfinal,percentfinal,
                        percentwon,percentmismatch
), by = 'bidder_id', type = 'full')

fulldata<-as.data.frame(fulldata)
fulldata<-na.omit(fulldata)
predictors<-fulldata[,5:19]
predictors$commonmerch<-as.factor(predictors$commonmerch)
predictors$country<-as.factor(predictors$country)
outcomes<-fulldata[,4]


fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10, 
  savePred=T)

#gradient boosting
gbmFit1 <- train(x=predictors,y=outcomes,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)
print(gbmFit1)

#rffit1 <- train(fulldata[,4]~.,data=fulldata[,5:18],method="rf",trControl = fitControl,verbose=FALSE)
#print(rffit1)

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
timediffbids<-mergeddata%>%group_by(auction)%>%do(.,differences(.))
meantimes<-timediffbids%>%group_by(bidder_id)%>%summarise(averagetimetobid=mean(time))
sdtimes<-timediffbids%>%group_by(bidder_id)%>%summarise(sdtimetobid=sd(time))
sdtimes$sdtimetobid[is.na(sdtimes$sdtimetobid)]<-0
percentfirst<-timediffbids%>%group_by(bidder_id)%>%summarise(percentfirst=sum(.[["time"]]==0)/n())
mergeddata2<-mergeddata
mergeddata2$self<-0
bidagainstself<-mergeddata2%>%group_by(auction)%>%do(.,bidself(.))
bidagainstself<-bidagainstself%>%group_by(bidder_id)%>%summarise(percentself=sum(.[["self"]]==1)/n())
finalbids<-timediffbids%>%group_by(auction)%>%mutate(last = c(rep(0,n()-1), 1))
numfinal<-finalbids%>%group_by(bidder_id)%>%summarise(percentfinal=sum(last))
percentfinal<-finalbids%>%group_by(bidder_id)%>%summarise(percentfinal=mean(last))
percentwon<-finalbids%>%group_by(bidder_id,auction)%>%summarise(percentfinal=sum(last))%>%
  group_by(bidder_id)%>%summarise(winpercent=mean(percentfinal))
holding<-mergeddata%>%group_by(auction,merchandise)%>%summarise(number=n())
merchperauction<-holding%>%group_by(auction)%>%
  summarise(cmerchandise=merchandise[[which.max(.[["number"]])]])
holding<-merge(mergeddata,merchperauction,by="auction")
percentmismatch<-holding%>%group_by(bidder_id,auction)%>%summarise(mismatch=sum(merchandise==cmerchandise)/n())%>%
  group_by(bidder_id)%>%summarise(percentmismatch=mean(mismatch))
#ok so we can join all the data
fulldata2<-join_all(list(test,totalbids,meanbids,numauctions,numdevices,
                         numtypes,commonmerch,numcountry,commoncountry,meantimes,
                         sdtimes,percentfirst,bidagainstself,numfinal,percentfinal,
                         percentwon,percentmismatch),
                    by = 'bidder_id', type = 'full')

#ok a couple hacks to make it work
#remove countries that didnt appear in training
fulldata2<-as.data.frame(fulldata2)
fulldata2$country[!(fulldata2$country %in% fulldata$country)]<-"cn"
predictors2<-fulldata2[,4:18]
predictors2$commonmerch<-as.factor(predictors2$commonmerch)
predictors2$country<-as.factor(predictors2$country)

#predict!
x=cbind(as.data.frame(test),predict(gbmFit1,predictors2))
x$`predict(gbmFit1, predictors2)`[x$`predict(gbmFit1, predictors2)`<0]<-0
write.csv(x,"output.csv")
#gave me score of 0.87679
