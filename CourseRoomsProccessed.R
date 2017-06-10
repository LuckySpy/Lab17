
#Set working directory
setwd("~/LabR/Lab17")

##---------------------------------PART 1 LOAD THE NEEDED DATA TABLES AND LIBRARIES TO BE USED---------------------------

#Load big files insted of opening them from the ui of the application
TMG_November <- read.csv("TMG_October.csv")
courseRooms <- read.csv("courseRooms.csv")

#Libraries to apply SQL
library(sqldf)
library(tcltk)

#Load the library we need to use
library(splitstackshape)



##---------------------------------PART 2 cORRECT WROGN ENTRIES DUE TO DIFFERENT DECODING---------------------------------


##---------------------------------PART 2.1 cORRECT WROGN ENTRIES FACULTY COLUMN------------------------------------------


#Use regular expression for pattern matching so that bad coded enties can be replaced with the corresponding values so that they can be  grouped correctly

courseRooms$Faculty<-gsub("^.*Mathematik, Informatik und Naturwissenschaften.*", "Mathematik, Informatik, Naturwissenschaften",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Architektur.*", "Architektur",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Bauingenieurwesen.*", "Bauingenieurwesen",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Maschinenwesen.*", "Maschinenwesen",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Georessourcen und Materialtechnik.*", "Georessourcen und Materialtechnik",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Elektrotechnik und Informationstechnik.*", "Elektrotechnik und Informationstechnik",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Philosophische.*", "Philosophische",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Wirtschaftswissenschaften.*", "Wirtschaftswissenschaften",courseRooms$Faculty)
courseRooms$Faculty<-gsub("^.*Medizinische.*", "Medizin",courseRooms$Faculty)



#Count how many courses each faculty has and how many each institute
faculties.courses<- as.data.frame(sqldf("SELECT Faculty, COUNT(CRTitle) FROM courseRooms  GROUP BY Faculty "))
colnames(faculties.courses)[2]<-("coursesNum")
faculties.courses<-faculties.courses[!faculties.courses$coursesNum <= 25 , ]
faculties.courses<-faculties.courses[!faculties.courses$Faculty == "" , ]



##---------------------------------PART 2.2 cORRECT WROGN ENTRIES CRTYPE COLUMN------------------------------------------


#Clean the column coursetype using regular expressions for patterns matching
courseRooms$CRType<-gsub("^.*ndliche.*fung.*", "Mündliche Prüfung (MP)",courseRooms$CRType)
courseRooms$CRType<-gsub("Vorlesung.*bung.*Praktikum.*", "Vorlesung/Übung/Praktikum (VÜP)",courseRooms$CRType)
courseRooms$CRType<-gsub("Interpretation.*sentation*", "Interpretation und Presenation",courseRooms$CRType)
courseRooms$CRType<-gsub("^.[^Vorlesung]*bung [:(:].*", "Übung (Ü)",courseRooms$CRType)
courseRooms$CRType<-gsub("Vorlesung/.*bung [:(:].*", "Vorlesung/Übung (VÜ)",courseRooms$CRType)
courseRooms$CRType<-gsub("^.*sentation [:(:].*", "Presenation (P)",courseRooms$CRType)
courseRooms$CRType<-gsub("^.*bung/Praktikum [:(:].*", "Übung/Praktikum (ÜP)",courseRooms$CRType)
courseRooms$CRType<-gsub("Labor.*bungen [:(:].*", "Laborübungen (LÜ)",courseRooms$CRType)
courseRooms$CRType<-gsub("Programmier.*bungen [:(:].*", "Programmierübungen (PÜ)",courseRooms$CRType)
courseRooms$CRType<-gsub(".*bung [:(:].*", "Übung (Ü)",courseRooms$CRType)




#Group together the same type of courses to check what are the dominant course types
crtype<- as.data.frame(sqldf("SELECT CRType, COUNT(CRType) FROM courseRooms  GROUP BY CRType "))
fcrtype<- merge(fcrtype, faculties.courses[1], by="Faculty")

#WE SHOULD TAKE ONE BY ONE THE FACULTIES AND MAKE A TABLE FOR EACH EIthER WIth tHE CRTYPES OR WITH THE COURSES

colnames(crtype)[2]<-("CRTypeNum")
crtype<-crtype[!crtype$CRType <= 6 , ]



##---------------------------------PART 2.3 cORRECT WROGN ENTRIES INSTITUTE COLUMN------------------------------------------


#Clean the columns of institute
courseRooms$Institute<-gsub(" f.*r ", " für ",courseRooms$Institute)
courseRooms$Institute<-gsub("Lehrstuhl fur .*chstfrequenzelektronik", "Lehrstuhl für Höchstfrequenzelektronik",courseRooms$Institute)
courseRooms$Institute<-gsub("Geb.*ude", "Gebäude",courseRooms$Institute)
courseRooms$Institute<-gsub("Berufsp.*dagogik", "Berufspädagogik",courseRooms$Institute)
courseRooms$Institute<-gsub("FZ J.*lich", "FZ Jülich",courseRooms$Institute)
courseRooms$Institute<-gsub("Gesch.*ftsf.*hrung", "Geschäftsführung",courseRooms$Institute)
courseRooms$Institute<-gsub("Gesch.*fts", "Geschäfts",courseRooms$Institute)
courseRooms$Institute<-gsub("mobilit.*t", "mobilität",courseRooms$Institute)
courseRooms$Institute<-gsub("Innovationsökonomik.*konomik", "Innovationsökonomik",courseRooms$Institute)
courseRooms$Institute<-gsub("Verhaltens.*konomik", "Verhaltensökonomik",courseRooms$Institute)#watch this one again if it worls row 90
courseRooms$Institute<-gsub("Grenzfl.*chen", "Grenzflächen",courseRooms$Institute)
courseRooms$Institute<-gsub("str.*mungssimulation", "strömungssimulation",courseRooms$Institute)
courseRooms$Institute<-gsub("P.*daudiologie", "Pädaudiologie",courseRooms$Institute)
courseRooms$Institute<-gsub("gyn.*kologische", "gynäkologische",courseRooms$Institute)
courseRooms$Institute<-gsub("P.*diatrie", "Pädiatrie",courseRooms$Institute)
courseRooms$Institute<-gsub("B.*rgerliches", "Bürgerliches",courseRooms$Institute)
courseRooms$Institute<-gsub("An.*sthesiologie", "Anästhesiologie",courseRooms$Institute)
courseRooms$Institute<-gsub("Geod.*tisches", "Geodätisches",courseRooms$Institute)
courseRooms$Institute<-gsub("H.*matologie", "Hämatologie",courseRooms$Institute)
courseRooms$Institute<-gsub("Zellul.*re", "Zelluläre",courseRooms$Institute)
courseRooms$Institute<-gsub("Oberfl.*chentechnik", "Oberflächentechnik",courseRooms$Institute)
courseRooms$Institute<-gsub("Orthop.*die", "Orthopädie",courseRooms$Institute)
courseRooms$Institute<-gsub("Str.*mungslehre", "Strömungslehre",courseRooms$Institute)
courseRooms$Institute<-gsub("Mikro.*konomie", "Mikroökonomie",courseRooms$Institute)
courseRooms$Institute<-gsub("Gef.*chirurgie", "Gefässchirurgie",courseRooms$Institute)
courseRooms$Institute<-gsub("W.*rme", "Wärme",courseRooms$Institute)
courseRooms$Institute<-gsub("Stoff.*bertragung", "Stoffübertragung",courseRooms$Institute)
courseRooms$Institute<-gsub("Energie.*konomik", "Energieökonomik",courseRooms$Institute)
courseRooms$Institute<-gsub("Zahn.*rztliche", "Zahnärztliche",courseRooms$Institute)
courseRooms$Institute<-gsub("Schwei.*technik und F.*getechnik", "Schweisstechnik und Fügetechnik",courseRooms$Institute)
courseRooms$Institute<-gsub("St.*dtebau", "Städtebau",courseRooms$Institute)
courseRooms$Institute<-gsub("Stra.*enwesen", "Strassenwesen",courseRooms$Institute)
courseRooms$Institute<-gsub("Universit.*tsbibliothek", "Universitätsbibliothek",courseRooms$Institute)
courseRooms$Institute<-gsub("für .*kosystemanalyse ", "für Ökosystemanalyse ",courseRooms$Institute)



institute.name<- as.data.frame(sqldf("SELECT Institute, COUNT(Institute) FROM courseRooms  GROUP BY Institute "))


##---------------------------------PART 2.4 cORRECT WROGN ENTRIES CRTITLE COLUMN------------------------------------------



#Correct some of the values in the table of the course titles
courseRooms$CRTitle<-gsub("Lehrstuhl fur .*chstfrequenzelektronik", "Lehrstuhl für Höchstfrequenzelektronik",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Geb.*ude", "Gebäude",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Berufsp.*dagogik", "Berufspädagogik",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("FZ J.*lich", "FZ Jülich",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Gesch.*ftsf.*hrung", "Geschäftsführung",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Gesch.*fts", "Geschäfts",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("mobilit.*t", "mobilität",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Innovationsökonomik.*konomik", "Innovationsökonomik",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Verhaltens.*konomik", "Verhaltensökonomik",courseRooms$CRTitle)#watch this one again if it worls row 90
courseRooms$CRTitle<-gsub("Grenzfl.*chen", "Grenzflächen",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("str.*mungssimulation", "strömungssimulation",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("P.*daudiologie", "Pädaudiologie",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("gyn.*kologische", "gynäkologische",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("P.*diatrie", "Pädiatrie",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("B.*rgerliches", "Bürgerliches",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("An.*sthesiologie", "Anästhesiologie",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Geod.*tisches", "Geodätisches",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("H.*matologie", "Hämatologie",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Zellul.*re", "Zelluläre",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Oberfl.*chentechnik", "Oberflächentechnik",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Orthop.*die", "Orthopädie",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Str.*mungslehre", "Strömungslehre",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Mikro.*konomie", "Mikroökonomie",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Gef.*chirurgie", "Gefässchirurgie",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("W.*rme", "Wärme",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Stoff.*bertragung", "Stoffübertragung",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Energie.*konomik", "Energieökonomik",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Zahn.*rztliche", "Zahnärztliche",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Schwei.*technik und F.*getechnik", "Schweisstechnik und Fügetechnik",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("St.*dtebau", "Städtebau",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Stra.*enwesen", "Strassenwesen",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Universit.*tsbibliothek", "Universitätsbibliothek",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("für .*kosystemanalyse ", "für Ökosystemanalyse ",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("Einf.*hrung", "Einführung",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("P.*dagogik", "Pädagogik",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub(",.*bung", ", Übung",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("^.*bung", "Übung",courseRooms$CRTitle)
courseRooms$CRTitle<-gsub("f.*r", "für",courseRooms$CRTitle)



#Group together the same type of courses to check what are the dominant course types
crtitle<- as.data.frame(sqldf("SELECT CRTitle, COUNT(CRTitle) FROM courseRooms  GROUP BY CRTitle "))




##---------------------------------PART 3 CLEAN SOME VALUES AND FILL SOME MISSING VALUES------------------------------------------





##Rows that do not contain LV number as noticed they contain no usefull information so they are deleted completely
courseRooms$LVNumber[courseRooms$LVNumber==""] <- NA
courseRooms<-as.data.frame(na.omit(courseRooms))


##Rows with missing values

#Find the rows that the non empty elements are placed in the all the columns of the data.framecourseRooms
courseRooms[courseRooms==""] <- NA


#Detect entries with NA value (or empty cells as we have already written)
empCells<-as.data.frame(which (is.na(courseRooms),arr.in=TRUE))
courseRooms[empCells$row,]



##---------------------------------PART 4 EXTRACT TABLES THAT ARE GOING TO BE USED FOR VISUALIZATION------------------------------------------

#Table with the number of courses in courserooms per faculty
write.csv(faculties.courses, file = "faculties.courses.csv")


#Result table with institutes per faculty and number of courses per institute  are being visualized in a treemap like form
facalty.institute<- as.data.frame(sqldf("SELECT Faculty,Institute, COUNT(CRTitle) FROM courseRooms  GROUP BY Institute ORDER BY Faculty ASC "))
colnames(facalty.institute)[3]<-("coursesNum")

#Visualization of the data in a treemap structure

library(treemap)
library(d3treeR)
trmp<-treemap(facalty.institute, #Your data frame object
              index=c("Faculty","Institute"),  #A list of your categorical variables
              vSize = "coursesNum",  #This is your quantitative variable
              type="index", #Type sets the organization and color scheme of your treemap
              palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
              title="Number of courses in faculties", #Customize your title
              fontsize.title = 14 #Change the font size of the title
)

d3tree2( trmp,rootname = "RWTH" )
