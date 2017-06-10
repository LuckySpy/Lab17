#set working directory

setwd("~/LabR")

#load big files insted of opening them from the ui of the application
TMG_October <- read.csv("TMG_October.csv")
userIds <- read.csv("userIds.csv")
courseRooms <- read.csv("courseRooms.csv")
eTestMetaInfo <- read.csv("eTestMetaInfo.csv")
eTestTries <- read.csv("eTestTries.csv")
TMG_November <- read.csv("TMG_November.csv")



#view the table of the data.frame TMG_October
View(TMG_October)



#Command to check the type of a variable or columns
typeof()
str(TMG_November)



#Exract a column from a data.frame in a new table column in this example in uriX
uriX<-as.data.frame( TMG_November$uri)



#changes the type of the data.frame uriX from factor to character
uriX <- data.frame(lapply(uriX, as.character), stringsAsFactors=FALSE)


#The data frame remStrPart is the result of removing the string part http://www3.elearning.rwth-aachen.de/ from each entry of data frame uriX
remStrPart<-as.data.frame(gsub("http://www3.elearning.rwth-aachen.de/", " ", uriX[,]))

#Initialization of empty data frame with data rypes characters and specific column names
#initDtFrm <- data.frame(hey=character(), yes=character(),hello=character(),wtf=character(),wtf1=character(),stringsAsFactors=FALSE)



#Split the single first row of a column of the data.frame x but it needs to be trasponse
#initDtFrm<-data.frame(strsplit(uriX[,],"/",fixed=TRUE))



#Instalation of new packages with specific name , in this case splitstackshape
#install.packages("splitstackshape")
#install.packages("sqldf")



#load the library we need to use
library(splitstackshape)

#change the name of a specific comlumn in a specific data.frame
colnames(remStrPart) <- "urisX"



#Command to split the URI fields that has different length and regions that are separated by "/" so for 
#those that do not have as many regions as others we use NA fieds and put everything into a data.frame
uriFields<-cSplit(remStrPart, "urisX", "/")


#the function that follows gives a pseudo random value in this case 6 letters. Whenever its called returns the same letters
#set.seed(45); sample(LETTERS, 6)

#exmaple how to count the NA elements in columnc of a data.frame
#set.seed(45)
#df <- data.frame(matrix(sample(c(NA,1:5), 50, replace=TRUE), ncol=5))
#colSums(!is.na(df))






# To count the elements of columns that are not NA
colSums(!is.na(uriFields))

#Find the rows that the non empty elements are placed in the column urisX_36 of data.frame uriFields
which (!is.na(uriFields$urisX_36),arr.in=TRUE)



#Make the data.frames from factors to strings so we can apply SQL
uriFieldsStr <- data.frame(lapply(uriFields, as.character), stringsAsFactors=FALSE)
courseRoomsStr <- data.frame(lapply(courseRooms, as.character), stringsAsFactors=FALSE)




library(sqldf)
library(tcltk)
#Find the courses that they are related to TMG records and their type 
Courses<- as.data.frame(sqldf("SELECT Distinct c.CRTitle,c.CRType,c.LVNumber FROM courseRooms c, uriFieldsStr u WHERE   u.LVNumber == c.LVNumber "))




#Change the name of the second column so we can merge over the same column with data.frameCourseRooms
colnames(uriFieldsStr)[2]<-("LVNumber")
joinCrsUri<- merge(courseRooms, uriFieldsStr, by="LVNumber")
joinCrsUri[17:length(joinCrsUri)]<- NULL





#The result of the JoinCrsUri is the merge result of CourseRooms and URI so we want to group the courses based on the semester given
#keep in mind that this is for a single TMG month


dif.sem<- as.data.frame(sqldf("SELECT Distinct urisX_04 FROM joinCrsUri "))
dif.sem<- as.data.frame(sqldf("SELECT DISTINCT urisX_04, Count(urisX_04) FROM joinCrsUri GROUP BY urisX_04 "))



#Use filter then use for loop and a data frame that increases in size and keep also the data separated in semesters
ss12.q1<-filter(joinCrsUri,urisX_04=='ss12')




#Drop multiple consecutive columns of a dataframe by using index of columns 
uriFieldsStr[13:61] <-NULL



#separate the date and time that was written in the column log and there was a " " gap in between that we used it to separate them

dTime<-as.data.frame( TMG_November$logtime)
colnames(dTime) <- "tim"
dTime<-cSplit(dTime, "tim", " ")
# the result is two columns the first one with date the second one with time


#Combine the operation with the corresponding uri in the uriFieldsStr
data.combined <- data.frame(uriFieldsStr,TMG_November$operation)
data.combined <- data.frame(lapply(data.combined, as.character), stringsAsFactors=FALSE)


#We want to use the function count so we have to load the corresponding ibrary that contains it
library(plyr)

#How many times each operation have appeared
count(data.combined,'TMG_November.operation')

#Add the fields of date and time to the data combined data.frame
#data.combined <- data.frame(uriFieldsStr,dTime)
#sum<- as.data.frame(count(data.combined,'urisX_03'))





#separate the data and time to day month year and hour minute etc.
specific.date<-as.data.frame( dTime$tim_1)
colnames(specific.date) <- "dateField"
specific.date<-cSplit(specific.date, "dateField", "-")

specific.time<-as.data.frame( dTime$tim_2)
colnames(specific.time) <- "timeField"
specific.time<-cSplit(specific.time, "timeField", ":")

specific.dateTime<-data.frame(specific.date,specific.time)
data.combined<-data.frame(data.combined,specific.dateTime)


#Try to get the machine used during the students sessions from clientagent logs
extractMachine<- function(agent) {
  agent <- as.character(agent)
  
  if (length(grep("Macintosh", agent)) > 0) {
    return ("Macintosh")
  } else if (length(grep("iPhone", agent)) > 0) {
    return ("iPhone")
  } else if (length(grep("iPad", agent)) > 0) {
    return ("iPad")
  } else if (length(grep("Windows", agent)) > 0) {
    return ("Windows")
  }  else if (length(grep("Android ", agent)) > 0) {
      return ("Android")
  }  else if (length(grep("linux-gnu", agent)) > 0) {
    return ("Linux")
  } else {
    return ("Other")
  }
}

machine <- NULL
for (i in 1:nrow(TMG_November)) {
  machine <- c(machine, extractMachine(TMG_November[i,"clientagent"]))
}

machine<-as.data.frame(machine)

# Put the data we are intested in in the data combined data.frame
data.combined<-data.frame(data.combined,machine)


#convert a column of a data.frame into factor
data.combined$urisX_02 <- as.factor(data.combined$urisX_02)


#Remove from the dataframe of a specific column rows that are apearing more than a number of times or less
#data.combined<- as.data.frame(data.combined[as.numeric(ave(data.combined$urisX_02, data.combined$urisX_02, FUN=length)) >= 50, ])
#data.combined<- as.data.frame(data.combined[as.numeric(ave(data.combined$urisX_03, data.combined$urisX_03, FUN=length)) >= 500, ])


library(ggplot2)


ggplot(data.combined, aes(x = timeField_1, fill = TMG_November.operation)) +
  geom_bar(width = 0.5) +
  facet_wrap(~urisX_01=="ws14") + 
  ggtitle("semester") +
  xlab("time") +
  ylab("Total Count") +
  labs(fill = "TMG_November.operation")
