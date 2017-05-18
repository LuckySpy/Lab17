#set working directory

setwd("~/LabR")

#load big files insted of opening them from the ui of the application
TMG_October <- read.csv("TMG_October.csv")
userIds <- read.csv("userIds.csv")
courseRooms <- read.csv("courseRooms.csv")
eTestMetaInfo <- read.csv("eTestMetaInfo.csv")
eTestTries <- read.csv("eTestTries.csv")



#view the table of the data.frame TMG_October
View(TMG_October)

TMG_November <- read.csv("TMG_November.csv")

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
initDtFrm <- data.frame(hey=character(), yes=character(),hello=character(),wtf=character(),wtf1=character(),stringsAsFactors=FALSE)



#Split the single first row of a column of the data.frame x but it needs to be trasponse
initDtFrm<-data.frame(strsplit(uriX[,],"/",fixed=TRUE))



#Instalation of new packages with specific name , in this case splitstackshape
install.packages("splitstackshape")
install.packages("sqldf")


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

#find the rows that the non empty elements are placed in the column urisX_36 of data.frame uriFields
which (!is.na(uriFields$urisX_36),arr.in=TRUE)

#Make the data.frames from factors to strings so we can apply SQL
uriFieldsStr <- data.frame(lapply(uriFields, as.character), stringsAsFactors=FALSE)
courseRoomsStr <- data.frame(lapply(courseRooms, as.character), stringsAsFactors=FALSE)


#Find the courses that they are related to TMG records and their type 
Courses<- as.data.frame(sqldf("SELECT Distinct CRTitle,CRType FROM courseRooms,uriFieldsStr WHERE   urisX_02 == LVnumber "))


#Drop columns off a dataframe by using index of column 
uriFieldsStr[13:61] <-NULL



dTime<-as.data.frame( TMG_November$logtime)
colnames(dTime) <- "tim"
dTime<-cSplit(dTime, "tim", " ")
# the result is two columns the first one with date the second one with time


#Combine the operation with the corresponding uri in the uriFieldsStr
data.combined <- data.frame(uriFieldsStr,TMG_November$operation)
data.combined <- data.frame(lapply(data.combined, as.character), stringsAsFactors=FALSE)

#how many times each operation have appeared
count(data.combined,'TMG_November.operation')

#Lets try to visualize how many operations per week are happening
data.combined <- data.frame(uriFieldsStr,dTime)
sum<- as.data.frame(count(data.combined,'urisX_03'))


