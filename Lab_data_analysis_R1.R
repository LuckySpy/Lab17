# Getting some insight in the moodle data
eTestMetaInfo <- read.csv("eTestMetaInfo.csv")
eTestTries <- read.csv("eTestTries.csv")
userIds <- read.csv("userIds.csv")
sharepoint <- read.csv("sharePoint.csv")

library(splitstackshape)
library(plyr)



#The date and time that the test beggin
temp <- as.data.frame(eTestTries$Begonnenam)
colnames(temp) <- "time"
temp<-cSplit(temp,"time"," ")
test.sDates <- as.data.frame(cSplit(temp,"time_1","-"))
test.sTime  <-as.data.frame(cSplit(temp,"time_2",":"))  



#The date and time that the test ends

temp1 <- as.data.frame(eTestTries$Beendet)
colnames(temp1) <- "time"
temp1<-cSplit(temp1,"time"," ")
test.eDates <- as.data.frame(cSplit(temp1,"time_1","-"))
test.eTime  <-as.data.frame(cSplit(temp1,"time_2",":"))




time.ofTries<- cbind.data.frame(test.sDates,test.sTime,test.eDates,test.eTime)
#Change the columna so that we have them in the desired order (date, date details, time , time details)
time.ofTries<-time.ofTries[,c(5,2,3,4,1,6,7,8,13,10,11,12,9,14,15,16)]


#Renaming the columns
colnames(time.ofTries)[1]<-("dateStart")
colnames(time.ofTries)[2]<-("yearStart")
colnames(time.ofTries)[3]<-("monthStart")
colnames(time.ofTries)[4]<-("dayStart")
colnames(time.ofTries)[5]<-("timeStart")
colnames(time.ofTries)[6]<-("hourStart")
colnames(time.ofTries)[7]<-("minuteStart")
colnames(time.ofTries)[8]<-("secondStart")
colnames(time.ofTries)[9]<-("dateEnd")
colnames(time.ofTries)[10]<-("yearEnd")
colnames(time.ofTries)[11]<-("monthEnd")
colnames(time.ofTries)[12]<-("dayEnd")
colnames(time.ofTries)[13]<-("timeEnd")
colnames(time.ofTries)[14]<-("hourEnd")
colnames(time.ofTries)[15]<-("minuteEnd")
colnames(time.ofTries)[16]<-("secondEnd")


#Count how many of the tests started have finished and how many got left
status<-count(eTestTries,'Status')




#A nice way to represent your data in histogram

x <- eTestTries$VerbrauchteZeit
h<-hist(x,
        breaks=12,
        col="red",
        xlab="Miles Per Gallon",
        main="Histogram with normal curve and box")
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
box()


library(sqldf)


#change name for the specific column with index 7
colnames(eTestTries)[7]<-("email")

#Find the students that they are related to TMG records and their type 
Courses<- as.data.frame(sqldf("SELECT Distinct Bewertung,VerbrauchteZeit,email FROM eTestTries,userIds WHERE   email == emailMoodle GROUP BY etestnr "))




#put together the columns that can be utilize for analysis
test.tries <-as.data.frame(time.ofTries)
test.tries <-cbind.data.frame(eTestTries[,2],test.tries)
test.tries <-cbind.data.frame(eTestTries[,7],test.tries)
test.tries <-cbind.data.frame(test.tries,eTestTries[,11:length(eTestTries)])
colnames(test.tries)[1]<-("email")
colnames(test.tries)[2]<-("id")



#We are going to merge testtries with etestmetainfo
moolde.test<- merge(test.tries, eTestMetaInfo, by="id")



count(moolde.test[,21:43])


library(rattle)
rattle()

