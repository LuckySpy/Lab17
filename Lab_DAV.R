
#Set working directory
setwd("~/LabR/Lab17")



#Load big files insted of opening them from the ui of the application
TMG_November <- read.csv("TMG_October.csv")
courseRooms <- read.csv("courseRooms.csv")

#Clean the column Faculty of table courseRooms from noisy annecessary string after reaching to the basic faculties
#courseRooms$Faculty<-gsub("FakultΓƒΒ¤t fΓƒΒΌr ", "",courseRooms$Faculty)
#courseRooms$Faculty<-gsub("^.*?r ", "",courseRooms$Faculty)
#courseRooms$Faculty<-gsub(" Fak.*?t", "",courseRooms$Faculty)


#Since we know the different faculties we can also apply 

courseRooms$Faculty<-gsub("^.*Mathematik, Informatik und Naturwissenschaften.*", "Mathematik, Informatik, Naturwissenschaften",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Architektur.*", "Architektur",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Bauingenieurwesen.*", "Bauingenieurwesen",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Maschinenwesen.*", "Maschinenwesen",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Georessourcen und Materialtechnik.*", "Georessourcen und Materialtechnik",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Elektrotechnik und Informationstechnik.*", "Elektrotechnik und Informationstechnik",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Philosophische.*", "Philosophische",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Wirtschaftswissenschaften.*", "Wirtschaftswissenschaften",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Medizinische.*", "Medizin",courseRooms$Faculty)



#Replace the faculties with the correct


##Example of pattern mathcing and special sharacters that can be used
##gsub("^.*?_","_","ATGAS_1121")
##result[1] "_1121"
##This regular expression matches the beginning of the string (^), any character (.) repeated zero or more times (*), and underscore (_). The ? makes the match "lazy" so that it only matches are far as the first underscore. That match is replaced with just an underscore.



#Exract a column from a data.frame in a new table column in this example in uriX
uriX<-as.data.frame( TMG_November$uri)



#The data frame tempUri is the result of removing the string part http://www3.elearning.rwth-aachen.de/ from each entry of data frame uriX
tempUri<-as.data.frame(gsub("http://www3.elearning.rwth-aachen.de/", " ", uriX[,]))


#Load the library we need to use
library(splitstackshape)

#change the name of a specific comlumn in a specific data.frame
colnames(tempUri) <- "urisX"



#Command to split the URI fields that has different length and regions that are separated by "/" so for 
#those that do not have as many regions as others we use NA fieds and put everything into a data.frame
uriFields<-cSplit(tempUri, "urisX", "/")
uriFields[13:length(uriFields)] <-NULL


#Libraries to apply SQL
library(sqldf)
library(tcltk)


#Find the courses that they are related to TMG records and their type 
Courses<- as.data.frame(sqldf("SELECT Distinct c.CRTitle,c.CRType,c.LVNumber FROM courseRooms c, uriFields u WHERE   u.urisX_02 == c.LVNumber "))





library(splitstackshape)
#Separate the LVNumber from the semester and the code so they can be used to join other data.frames
#Courses<-cSplit(Courses, "LVNumber", "-")


#Count how many courses each faculty has and how many each institute
faculties.courses<- as.data.frame(sqldf("SELECT Faculty, COUNT(CRTitle) FROM courseRooms  GROUP BY Faculty "))
colnames(faculties.courses)[2]<-("coursesNum")


#Clean the entries that has nothing to do with faculties
faculties.courses<-faculties.courses[!faculties.courses$coursesNum <= 25 , ]
faculties.courses<-faculties.courses[!faculties.courses$Faculty == "" , ]

#Clean the entries of the Table courseRooms (the special character + cannot be recognized)

courseRooms<-cSplit(courseRooms, "Faculty", "/")





#Clean the entries that there is no CRType so that it will make the job easier







