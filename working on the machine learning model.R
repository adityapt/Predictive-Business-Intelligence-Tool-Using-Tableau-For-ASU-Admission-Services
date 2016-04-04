#-------------------------------------------------------------------------------------------------------------------------
# 3/20/2016 working on the machine learning model
#-------------------------------------------------------------------------------------------------------------------------

setwd("~/capstone")
mod = read.csv("modifiedcsv3182016.csv")

library(randomForest)

View(as.data.frame(names(mod)))

rf = randomForest(ADMITTEDFLAG ~ ., mod, method = "class")
library(rpart)
rf = rpart(ADMITTEDFLAG ~ CALCULATEDINDEX+CALCULATEDINDEXGROUP+HIGHSCHOOLGPA+HIGHSCHOOLABORGPA+HIGHSCHOOLRANKPERCENT+HIGHSCHOOLDESCR+CENSUSAPPDIVISIONADJDESCR+APPLICATIONCOMPLETE+ SATQUANTITATIVEEXAMSCORE , mod, method = "class")
library(rpart.plot)
prp(rf)
predict(rf,mod)
table(mod$ADMIT,predict(rf,mod)[,2]>0.5)
#------------------------------------------------------------------------------------------------------------------------
# Taking the important columns for the admit train dataset 
#------------------------------------------------------------------------------------------------------------------------

columns = 
c(
"APPNUMBERS",
"TERMCODE",
"ADMITTYPEADJ",
"CENSUSAPPCOLLEGECODE",
"CENSUSAPPDEPTADJCODE",
"CENSUSAPPDEGREECODE",
"CENSUSAPPACADEMICPLANCODE",
"GENDERCODE",
"MKT",
"HIGHSCHOOLCODE",
"HIGHSCHOOLGPA",
"HIGHSCHOOLABORGPA",
"HIGHSCHOOLRANKPERCENT",
"HIGHSCHOOLGRADUATIONYEAR",
"SATCOMBINEDEXAMSCORE",
"SATCOMBINEDEXAMSCOREADJ",
"SATVERBALEXAMSCORE",
"SATQUANTITATIVEEXAMSCORE",
"ACTCOMPOSITEEXAMSCORE",
"CALCULATEDINDEX",
"CALCULATEDINDEXGROUP",
"LASTTRANSFERINSTCODE",
"CUMULATIVETRANSFERGPA",
"STUDENTCAMPUSDESCRIPTION",
"ADMIT",
"PROVINCEDATA",
"ASUADSHSCHNAME",
"MONTHYEAR",
"ASUCITZNCOUNTRYLD",
"VISADESCR",
"VISAPERMIT",
"UGRDINTLGPA",
"IELTSSCORE",
"TOEFLCOMP",
"ADMEVALCOMPLETEDT",
"FIRSTAPPROG",
"FIRSTAPPLAN",
"FIRSTADPROG",
"FIRSTADPLAN",
"APPLICATIONYEAR",
"STUDENTCOMPLETEDATE"
)


length(which(mod$APPLICATIONCOMPLETE == 1 & mod$ADMEVALCOMPLETEDT == ''))
length(which(mod$APPLICATIONCOMPLETE == 1 & mod$STUDENTCOMPLETEDATE == '' & mod$APPLICATIONYEAR == 2015))

names(mod)
ml = mod[,columns]

dim(ml)
#[1] 33112    41

names(ml)

#rf = randomForest(ADMITTEDFLAG ~ ., mod, method = "class")
library(rpart)
rf = rpart(ADMIT ~., ml, method = "class", xval = 10, parms = list(split = 'gini'))
#library(rpart.plot)
library(rpart.plot)
prp(rf)
head(predict(rf))
table(ml$ADMIT,predict(rf)[,2]>0.5)
# FALSE  TRUE
#0 17351  1439
#1  1484 12838

#varImp(rf)
#Overall
#ADMEVALCOMPLETEDT         7459.4461
#ASUADSHSCHNAME            6388.5014
#ASUCITZNCOUNTRYLD          643.3234
#CALCULATEDINDEX            291.8679
#CALCULATEDINDEXGROUP       289.1826
#CENSUSAPPACADEMICPLANCODE  775.2962
#CENSUSAPPDEGREECODE        252.4891
#CENSUSAPPDEPTADJCODE       425.8837
#FIRSTADPLAN               6502.8615
#FIRSTADPROG               6493.6663
#FIRSTAPPLAN               1420.4884
#FIRSTAPPROG               1417.4472
#HIGHSCHOOLABORGPA          203.1417
#IELTSSCORE                 702.9831
#LASTTRANSFERINSTCODE       285.6852
#MONTHYEAR                 1775.0331
#STUDENTCOMPLETEDATE       2077.6865
#TOEFLCOMP                  992.1082
#VISADESCR                 1455.2790
#VISAPERMIT                1455.2790
#TERMCODE                     0.0000
#ADMITTYPEADJ                 0.0000
#CENSUSAPPCOLLEGECODE         0.0000
#GENDERCODE                   0.0000
#MKT                          0.0000
#HIGHSCHOOLCODE               0.0000
#HIGHSCHOOLGPA                0.0000
#HIGHSCHOOLRANKPERCENT        0.0000
#HIGHSCHOOLGRADUATIONYEAR     0.0000
#SATCOMBINEDEXAMSCORE         0.0000
#SATCOMBINEDEXAMSCOREADJ      0.0000
#SATVERBALEXAMSCORE           0.0000
#SATQUANTITATIVEEXAMSCORE     0.0000
#ACTCOMPOSITEEXAMSCORE        0.0000
#CUMULATIVETRANSFERGPA        0.0000
#STUDENTCAMPUSDESCRIPTION     0.0000
#PROVINCEDATA                 0.0000
#UGRDINTLGPA                  0.0000
#APPLICATIONYEAR              0.0000

library(mice)
mi = complete(mice(ml))

ml$ADMIT = as.factor(ml$ADMIT)
gl = glm(ADMIT~.,nona,family = "binomial")

#30189
# 30189/33112
#[1] 0.9117238
require(rpart)
data(iris)
rpartModel <- rpart(Species ~ ., data = iris)
prp(rpartModel)
save(rpartModel, file = "C:/Users/Aditya/Documents/capstone/RpartModel.RData")

predict(rpartModel,type = "class")
SCRIPT_STR('
  require(rpart)
           data(iris)
           load(file = "C:/Users/Aditya/Documents/capstone/RpartModel.RData")
           newiris <- data.frame(Sepal.Length = .arg1, 
           Sepal.Width = .arg2, 
           Petal.Length = .arg3, 
           Petal.Width = .arg4)
           as.factor(predict(rpartModel, newiris, type ="class"))',
           ATTR(Sepal.Length),ATTR(Sepal.Width),ATTR(Petal.Length),ATTR(Petal.Width)
           ) 

#____________________________________________________________________________________________________________
# 3/22/2016 certain queries to be run in the dataset
#____________________________________________________________________________________________________________

summary(ml$UGRDINTLGPA)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   3.080   3.500   3.396   3.800   4.000   19449 

boxplot(ml$UGRDINTLGPA~ml$ADMIT)
boxplot(ml$UGRDINTLGPA~mod$ENROLLED)

length(which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
              & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
              &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
              & is.na(ml$ACTCOMPOSITEEXAMSCORE) == TRUE) & ml$ADMIT == 1 ))
#[1] 4393

View(ml[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
              & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
              &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
              & is.na(ml$ACTCOMPOSITEEXAMSCORE) == TRUE) & ml$ADMIT == 1 ),])
#for FTF students
View(ml[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
               & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
               &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
               & is.na(ml$ACTCOMPOSITEEXAMSCORE) == TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" ),])

length(which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
               & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
               &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
               & is.na(ml$ACTCOMPOSITEEXAMSCORE) == TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" ))
#[1] 2188

View(ml[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
               & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
               &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
               & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)== TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" ),])

length(which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
               & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
               &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
               & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)== TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" ))
#[1] 196

# lets say i am removing these students from the dataset
levels(ml$VISADESCR)

unique(ml$VISADESCR[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
               & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
               &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
               & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)== TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" )])

#[1]                                Academic Student               Exchange Visitor               International not coming to US
#[5] Treaty Invest, Spouse & Child  Spouse or Child of TN1 or TN2 

#42 Levels:  Academic Student Alien in Transit Alien in Transit to UN Aliens Accompanying an O2 ... Waiver - Tourist
 
a = unique(ml$VISADESCR[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                           & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                           &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                           & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)== TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" )])

unique(ml$VISAPERMIT[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                           & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                           &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                           & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)== TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" )])
#[1]    F1 J1 JN E2 TD


library(sqldf) 
sqldf("select distinct VISADESCR,VISAPERMIT from ml where VISAPERMIT in ('F1','J1','JN','E2','TD') ")

#VISADESCR VISAPERMIT
#1               Academic Student         F1
#2  Treaty Invest, Spouse & Child         E2 Professionals Holding Advanced 
#3  Spouse or Child of TN1 or TN2         TD NAFTA professional worker: Mexico, Canada
#4 International not coming to US         JN Exchange visitor
#5               Exchange Visitor         J1

unique(ml$ASUCITZNCOUNTRYLD[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                            & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                            &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                            & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE) & is.na(ml$UGRDINTLGPA)== TRUE) & ml$ADMIT == 1 & ml$ADMITTYPEADJ == "FTF" )])



length(ml[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                                   & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                                   &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                                   & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)  & is.na(ml$UGRDINTLGPA)== TRUE) & ml$ADMIT == 0 & ml$ADMITTYPEADJ == "FTF" ),])
# 41

# what is the difference between these 40 and those 196?

admit = ml[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                 & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                 &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                 & is.na(ml$ACTCOMPOSITEEXAMSCORE) == TRUE) & ml$ADMITTYPEADJ == "FTF" ),]
dim(admit)
#[1] 7337   40
summary(admit$UGRDINTLGPA)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.04    2.90    3.41    3.29    3.80    4.00    6169
table(admit$ADMIT)
#0    1 
#7141  196 
length(which(is.na(admit$UGRDINTLGPA) & admit == 1 ))
#[1] 55

summary(ml$HIGHSCHOOLGPA)
nrow(admit)

names(ml)
library(sqldf)

sqldf("select distinct UGRDINTLGPA from ml where calculatedindexgroup = 'No Index' and admit =1 ")

#---------------------------------------------------------------------------------------------------
#3/23/2016 
#---------------------------------------------------------------------------------------------------
allnull = mod[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                 & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                 &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                 & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)  & is.na(ml$UGRDINTLGPA)== TRUE)  & ml$ADMITTYPEADJ == "FTF" ),]
# No Index

#These ppl dont have an index but they came from a english speakin school?
summary(mod$ADMITDATE)

str(allnull)
nrow(allnull)
#[1] 6169
table(allnull$APPLICATIONYEAR, allnull$ADMIT)
#Year    0    1
#2009  196    5
#2010  896   19
#2011  842   21
#2012  652    2
#2013 1354    2
#2014 1419    1
#2015  756    4

unique(allnull$ASUCITZNCOUNTRYLD[which(allnull$ADMIT == 1 )])

unique(allnull$ASUADSHSCHNAME[which(allnull$ADMIT == 1 & allnull$ASUCITZNCOUNTRYLD == 'India' )])
#null

unique(allnull$ASUADSHSCHNAME[which(allnull$ADMIT == 1 & allnull$ASUCITZNCOUNTRYLD == 'China' )])

allnulltrn = mod[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                     & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                     &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                     & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)  & is.na(ml$UGRDINTLGPA)== TRUE)  & ml$ADMITTYPEADJ == "TRN" ),]

nrow(allnulltrn)
#4558
table(allnulltrn$APPLICATIONYEAR, allnulltrn$ADMIT)

unique(allnulltrn$LASTTRANSFERINSTDESCR[which(allnulltrn$ADMIT == 1 )])

length(allnulltrn$CUMULATIVETRANSFERGPA[which(allnulltrn$ADMIT == 1 & is.na(allnulltrn$CUMULATIVETRANSFERGPA) == "TRUE" )])
#195

#------------------------------------------------------------------------------------------------------
#working on teh transfer students who havent provided the cumulative transfer gpa
#_______________________________________________________________________________________________________
View(mod)

allnulltrn = mod[which((is.na(mod$HIGHSCHOOLGPA) & is.na(mod$HIGHSCHOOLABORGPA) & is.na(mod$HIGHSCHOOLRANKPERCENT) 
                        & is.na(mod$SATCOMBINEDEXAMSCORE) & is.na(mod$SATCOMBINEDEXAMSCORE)
                        &is.na(mod$SATCOMBINEDEXAMSCOREADJ) & is.na(mod$SATVERBALEXAMSCORE) & is.na(mod$SATQUANTITATIVEEXAMSCORE) 
                        & is.na(mod$ACTCOMPOSITEEXAMSCORE) & is.na(mod$TOEFLCOMP) & is.na(mod$IELTSSCORE)  & is.na(mod$UGRDINTLGPA) & is.na(mod$CUMULATIVETRANSFERGPA)== TRUE)  & ml$ADMITTYPEADJ == "TRN" ),]

nrow(allnulltrn)
#[1] 3337
table(allnulltrn$ADMIT)
#0    1 
#3142  195 
View(allnulltrn[which(allnulltrn$ADMIT == 1),])
sort(table (factor(allnulltrn$LASTTRANSFERINSTDESCR[which(allnulltrn$ADMIT == 1)])))

#Federal University of Parana                 Federal University of Pelotas 
#1                                             1 
#Federal University of Rio Grande do Norte              Federal University of Uberlandia 
#1                                             1 
#Huazhong University of Science and Technology     Kanda University of International Studies 
#1                                             1 
#Karaganda State University E A Buketov     New York Institute Technical Old Westbury 
#1                                             1 
#Potiguar University               University of California Irvine 
#1                                             1 
#                                   Foreign Institution 
#3                                           182 
sort(table (factor(allnulltrn$LASTTRANSFERINSTSTATECODE[which(allnulltrn$ADMIT == 1)])))
#CA  NY     
#1   1 193 

# Trying to find out the number of transfer students who didnt get an admit!

sort(table (factor(allnulltrn$LASTTRANSFERINSTDESCR[which(allnulltrn$ADMIT == 0)])))
# these people were rejected! on what basis we dont know ! lets compare both sets

table(allnulltrn$ADMIT)
#0    1 
#3142  195 
sort(table (factor(allnulltrn$APPLICATIONCOMPLETE[which(allnulltrn$ADMIT == 0)])))
#1    0 
#132 3010 

View(allnulltrn[which(allnulltrn$APPLICATIONCOMPLETE == 1),])
View(sqldf("select * from allnulltrn "))

# creating a dataset with transfer students and have completed their application!
trnapplied = allnulltrn[which(allnulltrn$APPLICATIONCOMPLETE == 1),]
table(trnapplied$ADMIT)
#0   1 
#132 195
table(factor(trnapplied$LASTTRANSFERINSTDESCR))
summary(trnapplied)

table(trnapplied$GENDERDESCR,trnapplied$ADMIT)
        #0   1
#Female  29  80
#Male   103 115

table(factor(trnapplied$LASTTRANSFERINSTDESCR),trnapplied$ADMIT)
names(allnulltrn)
table(factor(trnapplied$ASUCITZNCOUNTRYLD),trnapplied$ADMIT)[which.max(table(factor(trnapplied$ASUCITZNCOUNTRYLD),trnapplied$ADMIT))]

# turns out that most of the people who dont give any details and get an admit are from china
# next is saudi arabia who gets most of the rejects!

#---------------------------------------------------------------------------------------------------
# 3/24/2016
#---------------------------------------------------------------------------------------------------
allnull = mod[which((is.na(ml$HIGHSCHOOLGPA) & is.na(ml$HIGHSCHOOLABORGPA) & is.na(ml$HIGHSCHOOLRANKPERCENT) 
                     & is.na(ml$SATCOMBINEDEXAMSCORE) & is.na(ml$SATCOMBINEDEXAMSCORE)
                     &is.na(ml$SATCOMBINEDEXAMSCOREADJ) & is.na(ml$SATVERBALEXAMSCORE) & is.na(ml$SATQUANTITATIVEEXAMSCORE) 
                     & is.na(ml$ACTCOMPOSITEEXAMSCORE) & is.na(ml$TOEFLCOMP) & is.na(ml$IELTSSCORE)  & is.na(ml$UGRDINTLGPA)== TRUE)  & ml$ADMITTYPEADJ == "FTF" ),]


nrow(allnull)
table(allnull$ADMIT)
#  0    1 
#6115   54 

View(allnull[which(allnull$ADMIT == 1),])

# deleting the rows with TRN 
mod = mod[which(mod$ADMITTYPEADJ == "FTF"),]
# deleting the rows witl allnull attributes and admit = 1
length(allnull$APPNUMBERS[which(allnull$ADMIT == 1)])
#54
library(sqldf)
mod = sqldf("select * from mod where Appnumbers not in (select appnumbers from allnull where admit = 1)")

nrow(mod)

nrow(mod[which((is.na(mod$HIGHSCHOOLGPA) & is.na(mod$HIGHSCHOOLABORGPA) & is.na(mod$HIGHSCHOOLRANKPERCENT) 
                     & is.na(mod$SATCOMBINEDEXAMSCORE) & is.na(mod$SATCOMBINEDEXAMSCORE)
                     &is.na(mod$SATCOMBINEDEXAMSCOREADJ) & is.na(mod$SATVERBALEXAMSCORE) & is.na(mod$SATQUANTITATIVEEXAMSCORE) 
                     & is.na(mod$ACTCOMPOSITEEXAMSCORE) & is.na(mod$TOEFLCOMP) & is.na(mod$IELTSSCORE)  & is.na(mod$UGRDINTLGPA)== TRUE)  & mod$ADMITTYPEADJ == "FTF" & mod$ADMIT == 1 ),])
#0
#Now that we can understand that some attributes are present to students that get an admit!

write.csv(mod, "modifiedcsv3242016.csv")

#------------------------------------------------------------------------------------------------
#creating a model for admit
#------------------------------------------------------------------------------------------------
columns = 
  c(
    "APPNUMBERS",
    "TERMCODE",
    "ADMITTYPEADJ",
    "CENSUSAPPCOLLEGECODE",
    "CENSUSAPPDEPTADJCODE",
    "CENSUSAPPDEGREECODE",
    "CENSUSAPPACADEMICPLANCODE",
    "GENDERCODE",
    "MKT",
    "HIGHSCHOOLCODE",
    "HIGHSCHOOLGPA",
    "HIGHSCHOOLABORGPA",
    "HIGHSCHOOLRANKPERCENT",
    "HIGHSCHOOLGRADUATIONYEAR",
    "SATCOMBINEDEXAMSCORE",
    "SATCOMBINEDEXAMSCOREADJ",
    "SATVERBALEXAMSCORE",
    "SATQUANTITATIVEEXAMSCORE",
    "ACTCOMPOSITEEXAMSCORE",
    "CALCULATEDINDEX",
    "CALCULATEDINDEXGROUP",
    "LASTTRANSFERINSTCODE",
    "CUMULATIVETRANSFERGPA",
    "STUDENTCAMPUSDESCRIPTION",
    "ADMIT",
    "PROVINCEDATA",
    "ASUADSHSCHNAME",
    "MONTHYEAR",
    "ASUCITZNCOUNTRYLD",
    "VISADESCR",
    "VISAPERMIT",
    "UGRDINTLGPA",
    "IELTSSCORE",
    "TOEFLCOMP",
    "ADMEVALCOMPLETEDT",
    "FIRSTAPPROG",
    "FIRSTAPPLAN",
    "FIRSTADPROG",
    "FIRSTADPLAN",
    "APPLICATIONYEAR",
    "STUDENTCOMPLETEDATE"
  )


length(which(mod$APPLICATIONCOMPLETE == 1 & mod$ADMEVALCOMPLETEDT == ''))
length(which(mod$APPLICATIONCOMPLETE == 1 & mod$STUDENTCOMPLETEDATE == '' & mod$APPLICATIONYEAR == 2015))

names(mod)
ml = mod[,columns]

dim(ml)

library(rpart)
library(rpart.plot)

names(ml)

summary(mod$ADMIT)
mod$ADMIT = factor(mod$ADMIT)
rf = rpart(ADMIT ~ ., data = ml[,-c(2,28,1,3,9,42,40,23,15)])
varImp(rf)
prp(rf)

predict(rf)
table(mod$ADMIT,predict(rf)>0.5)
#FALSE  TRUE
#0 12144  1494
#1   623 10614

#22758/nrow(mod)
#[1] 0.9148945

new = ml
train = ml[which(ml$APPLICATIONYEAR != 2015 & ml$APPLICATIONYEAR != 2014),]
nrow(train)
#15473
test = ml[which(ml$APPLICATIONYEAR == 2015 | ml$APPLICATIONYEAR == 2014),]
nrow(test)
#9402
rf = rpart(ADMIT ~ ., data = train[,-c(2,28,1,3,9,42,40,23,15,22)])
length(predict(rf,test))
#[1] 9402

table(test$ADMIT,predict(rf,test) > 0.5)
#FALSE TRUE
#0  3813 1218
#1   333 4038

(3813+4038)/nrow(test)
#[1] 0.8350351

varImp(rf)

names(ml)
?scale
#SCALING THE DATA
new[,c(11,12,13,14,15,16,17,18,19,20,32)] = scale(new[,c(11,12,13,14,15,16,17,18,19,20,32)])
names(new)
train = new[which(new$APPLICATIONYEAR != 2015 & new$APPLICATIONYEAR != 2014),]
nrow(train)
#15473
test = new[which(new$APPLICATIONYEAR == 2015 | new$APPLICATIONYEAR == 2014),]
nrow(test)
#9402

rf = rpart(ADMIT ~ ., data = train[,-c(2,28,1,3,9,42,40,23,15,22)])
length(predict(rf,test))
#[1] 9402

table(test$ADMIT,predict(rf,test) > 0.5)
varImp(rf)
library(randomForest)
prp(rf)
plot(rf, uniform=TRUE, 
     main="Classification Tree")
text(rf)

#pruning the tree
pfit<- prune(rf, cp=   rf$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

rf
names(rf)
rf$variable.importance
varImp(rf)

str(mod)
?write.csv
write.csv(mod, "modifiedcsv3242016.csv", na = "")

#-----------------------------------------------------------------------------------
#3/25/2016 working on the data
#-----------------------------------------------------------------------------------
setwd('~/capstone')
mod = read.csv("modifiedcsv3242016.csv", na.strings = c("NA",""))
#linear model for predicting UGRDINTLGPA
str(ml)

summary(ml)
length(which(is.na(ml$UGRDINTLGPA) == "FALSE"))
summary(ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),])

lm = lm(UGRDINTLGPA ~.,ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),] )
names(ml)

lm = lm(UGRDINTLGPA ~ HIGHSCHOOLGPA + HIGHSCHOOLABORGPA + IELTSSCORE + TOEFLCOMP +
        SATCOMBINEDEXAMSCORE+ SATCOMBINEDEXAMSCOREADJ + SATVERBALEXAMSCORE + SATQUANTITATIVEEXAMSCORE 
        + ACTCOMPOSITEEXAMSCORE,ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),] )

lm = lm(UGRDINTLGPA ~ HIGHSCHOOLGPA + HIGHSCHOOLABORGPA + IELTSSCORE + TOEFLCOMP +
          SATCOMBINEDEXAMSCORE+ SATCOMBINEDEXAMSCOREADJ + SATVERBALEXAMSCORE + SATQUANTITATIVEEXAMSCORE 
        ,ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),] )
sm = summary(lm)

lm = lm(UGRDINTLGPA ~ HIGHSCHOOLGPA + HIGHSCHOOLABORGPA + IELTSSCORE + TOEFLCOMP +
          SATCOMBINEDEXAMSCORE+ SATCOMBINEDEXAMSCOREADJ + SATVERBALEXAMSCORE  
        ,ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),] )
sm = summary(lm)

lm = lm(UGRDINTLGPA ~ HIGHSCHOOLGPA + HIGHSCHOOLABORGPA + IELTSSCORE + TOEFLCOMP +
          SATCOMBINEDEXAMSCORE+ SATCOMBINEDEXAMSCOREADJ   
        ,ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),] )
sm = summary(lm)

lm = lm(UGRDINTLGPA ~ HIGHSCHOOLGPA + HIGHSCHOOLABORGPA + IELTSSCORE + TOEFLCOMP 
         ,ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),] )
sm = summary(lm)

lm = lm(UGRDINTLGPA ~  IELTSSCORE + TOEFLCOMP +HIGHSCHOOLABORGPA + HIGHSCHOOLGPA
        ,ml[which(is.na(ml$UGRDINTLGPA) == "FALSE"),] )

lm = lm(UGRDINTLGPA ~ HIGHSCHOOLGPA, ml)
library(Hmisc)
rcorr(ml$HIGHSCHOOLABORGPA, ml$HIGHSCHOOLGPA, type = "pearson")
#0.83 highly correlated

rcorr(ml$UGRDINTLGPA, ml$HIGHSCHOOLGPA, type = "pearson")
#0.96 very highly correlated

table(ml$UGRDINTLGPA>2.25,ml$ADMIT)
#clustering ugrdintlgpa into groups will suffice

library(Rserve)
Rserve()

?aggregate()
aggregate(ml$UGRDINTLGPA, list(ml$CALCULATEDINDEXGROUP), mean, na.action = na.omit)
length(ml$UGRDINTLGPA[which(ml$PROVINCEDATA == "anhui")])

lm = lm(UGRDINTLGPA ~ HIGHSCHOOLGPA, ml)
aggregate(cbind(predict(lm),ml$UGRDINTLGPA), list(ml$PROVINCEDATA), mean, na.action = na.omit)

cbind(predict(lm),ml$UGRDINTLGPA)
#replacing all the NA values with 0 for all the score columns
names(ml)
mod$HIGHSCHOOLGPA = ml$HIGHSCHOOLGPA
#imputing the missign values with 0
for ( i in 1: nrow(mod)){mod$HIGHSCHOOLGPA[i] = ifelse(is.na(mod$HIGHSCHOOLGPA[i]),0,mod$HIGHSCHOOLGPA[i]) }
for ( i in 1: nrow(mod)){mod$HIGHSCHOOLABORGPA[i] = ifelse(is.na(mod$HIGHSCHOOLABORGPA[i]),0,mod$HIGHSCHOOLABORGPA[i]) }
for ( i in 1: nrow(mod)){mod$HIGHSCHOOLRANKPERCENT[i] = ifelse(is.na(mod$HIGHSCHOOLRANKPERCENT[i]),0,mod$HIGHSCHOOLRANKPERCENT[i]) }
for ( i in 1: nrow(mod)){mod$SATCOMBINEDEXAMSCORE[i] = ifelse(is.na(mod$SATCOMBINEDEXAMSCORE[i]),0,mod$SATCOMBINEDEXAMSCORE[i]) }
for ( i in 1: nrow(mod)){mod$SATCOMBINEDEXAMSCOREADJ[i] = ifelse(is.na(mod$SATCOMBINEDEXAMSCOREADJ[i]),0,mod$SATCOMBINEDEXAMSCOREADJ[i]) }
for ( i in 1: nrow(mod)){mod$SATVERBALEXAMSCORE[i] = ifelse(is.na(mod$SATVERBALEXAMSCORE[i]),0,mod$SATVERBALEXAMSCORE[i]) }
for ( i in 1: nrow(mod)){mod$SATQUANTITATIVEEXAMSCORE[i] = ifelse(is.na(mod$SATQUANTITATIVEEXAMSCORE[i]),0,mod$SATQUANTITATIVEEXAMSCORE[i]) }
for ( i in 1: nrow(mod)){mod$ACTCOMPOSITEEXAMSCORE[i] = ifelse(is.na(mod$ACTCOMPOSITEEXAMSCORE[i]),0,mod$ACTCOMPOSITEEXAMSCORE[i]) }
for ( i in 1: nrow(mod)){mod$IELTSSCORE[i] = ifelse(is.na(mod$IELTSSCORE[i]),0,mod$IELTSSCORE[i]) }
for ( i in 1: nrow(mod)){mod$TOEFLCOMP[i] = ifelse(is.na(mod$TOEFLCOMP[i]),0,mod$TOEFLCOMP[i]) }
for ( i in 1: nrow(mod)){mod$UGRDINTLGPA[i] = ifelse(is.na(mod$UGRDINTLGPA[i]),0,mod$UGRDINTLGPA[i]) }

summary(mod)

write.csv(mod,"modifiedcsv3252016.csv",na = "")

new = mod
library(rpart)
train = new[which(new$APPLICATIONYEAR != 2015 & new$APPLICATIONYEAR != 2014),]
nrow(train)
#15473
test = new[which(new$APPLICATIONYEAR == 2015 | new$APPLICATIONYEAR == 2014),]
nrow(test)
#9402

rf = rpart(ADMIT ~ ., data = train[,-c(2,28,1,3,9,42,40,23,15,22)])
length(predict(rf,test))
#[1] 9402

table(test$ADMIT,predict(rf,test)>0.5)
library(rpart.plot)
prp(rf)
plot(rf, uniform=TRUE, text = TRUE,main="Classification Tree")
text(rf, use.n=TRUE, all=TRUE, cex=0)
library(caret)
varImp(rf)

#---------------------------------------------------------------------------------------------
##################### 3/26/2016 imputing the other categorical columns with Not provided
#---------------------------------------------------------------------------------------------

setwd('~/capstone')
mod = read.csv("modifiedcsv3252016.csv",na.strings = c("NA",""))
backup = read.csv("modifiedcsv3252016.csv",na.strings = c("NA",""))
names(mod)
#mod[1] = NULL 
summary(mod)
names(mod)

summary(mod$CENSUSAPPDIVISIONADJDESCR)
c = which(is.na(mod$CENSUSAPPDIVISIONADJDESCR))
which(is.na(mod$CENSUSAPPDIVISIONADJDESCR))
levels(mod$CENSUSAPPDIVISIONADJDESCR)[6] = "Not Provided"
l = levels(mod$CENSUSAPPDIVISIONADJDESCR)
mod$CENSUSAPPDIVISIONADJDESCR = factor(mod$CENSUSAPPDIVISIONADJDESCR, levels = l)
for (i in 1: length(c)){mod$CENSUSAPPDIVISIONADJDESCR[c[i]] = "Not Provided"}
summary(mod$CENSUSAPPDIVISIONADJDESCR)

# CENSUSAPPDIVISIONADJCODE
c = which(is.na(mod$CENSUSAPPDIVISIONADJCODE))
levels(mod$CENSUSAPPDIVISIONADJCODE)[6] = "NP"
l = levels(mod$CENSUSAPPDIVISIONADJCODE)
mod$CENSUSAPPDIVISIONADJCODE = factor(mod$CENSUSAPPDIVISIONADJCODE, levels = l)
for (i in 1: length(c)){mod$CENSUSAPPDIVISIONADJCODE[c[i]] = "NP"}
summary(mod$CENSUSAPPDIVISIONADJCODE)

# Highschoolcode and description
summary(mod$HIGHSCHOOLCODE)
mod$HIGHSCHOOLCODE = backup$HIGHSCHOOLCODE
mod$HIGHSCHOOLCODE = factor(mod$HIGHSCHOOLCODE)
c = which(is.na(mod$HIGHSCHOOLCODE))
length(levels(mod$HIGHSCHOOLCODE))
levels(mod$HIGHSCHOOLCODE)[length(levels(mod$HIGHSCHOOLCODE))+1] = "NP"
l = levels(mod$HIGHSCHOOLCODE)
mod$HIGHSCHOOLCODE = factor(mod$HIGHSCHOOLCODE, levels = l)
for (i in 1: length(c)){mod$HIGHSCHOOLCODE[c[i]] = "NP"}
summary(mod$HIGHSCHOOLCODE)

# Highschooldescription
c = which(is.na(mod$HIGHSCHOOLDESCR))
length(c)
levels(mod$HIGHSCHOOLDESCR)
levels(mod$HIGHSCHOOLDESCR)[length(levels(mod$HIGHSCHOOLDESCR))+1] = "Not Provided"
l = levels(mod$HIGHSCHOOLDESCR)
length(l)
mod$HIGHSCHOOLDESCR = factor(mod$HIGHSCHOOLDESCR, levels = l)
for (i in 1: length(c)){mod$HIGHSCHOOLDESCR[c[i]] = "Not Provided"}
summary(mod$HIGHSCHOOLDESCR)

# HIghSchoolStateCode
c = which(is.na(mod$HIGHSCHOOLSTATECODE))
length(c)
levels(mod$HIGHSCHOOLSTATECODE)
levels(mod$HIGHSCHOOLSTATECODE)[length(levels(mod$HIGHSCHOOLSTATECODE))+1] = "Not Provided"
l = levels(mod$HIGHSCHOOLSTATECODE)
length(l)
mod$HIGHSCHOOLSTATECODE = factor(mod$HIGHSCHOOLSTATECODE, levels = l)
for (i in 1: length(c)){mod$HIGHSCHOOLSTATECODE[c[i]] = "Not Provided"}
summary(mod$HIGHSCHOOLSTATECODE)

# Firstgeneration flag
summary(mod$FIRSTGENERATIONFLAG)
c = which(is.na(mod$FIRSTGENERATIONFLAG))
length(levels(mod$FIRSTGENERATIONFLAG))
levels(mod$FIRSTGENERATIONFLAG)[length(levels(mod$FIRSTGENERATIONFLAG))+1] = "NP"
l = levels(mod$FIRSTGENERATIONFLAG)
mod$FIRSTGENERATIONFLAG = factor(mod$FIRSTGENERATIONFLAG, levels = l)
for (i in 1: length(c)){mod$FIRSTGENERATIONFLAG[c[i]] = "NP"}
summary(mod$FIRSTGENERATIONFLAG)

# NSC School 
#For Enrolled students
mod$NSCSCHOOL = backup$NSCSCHOOL
summary(factor(mod$NSCSCHOOL[which(mod$ENROLLED == 1)]))
c = which(is.na(mod$NSCSCHOOL[which(mod$ENROLLED == 1)]))
length(c)
levels(mod$NSCSCHOOL)[length(levels(mod$NSCSCHOOL))+1] = "ASU"
l = levels(mod$NSCSCHOOL)
mod$NSCSCHOOL = factor(mod$NSCSCHOOL, levels = l)
for (i in 1: length(c)){mod$NSCSCHOOL[which(mod$ENROLLED == 1)][c[i]] = "ASU"}
summary(factor(mod$NSCSCHOOL[which(mod$ENROLLED == 1)]))

#For non enrolled students 
summary(factor(mod$NSCSCHOOL[which(mod$ENROLLED == 0)]))
c = which(is.na(mod$NSCSCHOOL[which(mod$ENROLLED == 0)]))
length(c)
levels(mod$NSCSCHOOL)[length(levels(mod$NSCSCHOOL))+1] = "Not Provided"
levels(mod$NSCSCHOOL)
l = levels(mod$NSCSCHOOL)
mod$NSCSCHOOL = factor(mod$NSCSCHOOL, levels = l)
levels(mod$NSCSCHOOL)
for (i in 1: length(c)){mod$NSCSCHOOL[which(mod$ENROLLED == 0)][c[i]] = "Not Provided"}
summary(factor(mod$NSCSCHOOL[which(mod$ENROLLED == 0)]))

#NSC SChool Type
mod$NSCSCHOOL = backup$NSCSCHOOL
summary(factor(mod$NSCSCHOOLTYPE))
c = which(is.na(mod$NSCSCHOOLTYPE))
length(c)
levels(mod$NSCSCHOOLTYPE)[length(levels(mod$NSCSCHOOLTYPE))+1] = "NP"
l = levels(mod$NSCSCHOOLTYPE)
mod$NSCSCHOOLTYPE = factor(mod$NSCSCHOOLTYPE, levels = l)
for (i in 1: length(c)){mod$NSCSCHOOLTYPE[c[i]] = "NP"}
summary(factor(mod$NSCSCHOOLTYPE))

# NSC School State
#For Enrolled students
#mod$NSCSCHOOL = backup$NSCSCHOOL
summary(factor(mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 1)]))
c = which(is.na(mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 1)]))
length(c)
#levels(mod$NSCSCHOOLSTATE)[length(levels(mod$NSCSCHOOLSTATE))+1] = "ASU"
#l = levels(mod$NSCSCHOOL)
#mod$NSCSCHOOL = factor(mod$NSCSCHOOL, levels = l)
for (i in 1: length(c)){mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 1)][c[i]] = "Arizona"}
summary(factor(mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 1)]))

#For non enrolled students 
summary(factor(mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 0)]))
c = which(is.na(mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 0)]))
length(c)
levels(mod$NSCSCHOOLSTATE)[length(levels(mod$NSCSCHOOLSTATE))+1] = "Not Provided"
levels(mod$NSCSCHOOLSTATE)
l = levels(mod$NSCSCHOOLSTATE)
mod$NSCSCHOOLSTATE = factor(mod$NSCSCHOOLSTATE, levels = l)
levels(mod$NSCSCHOOLSTATE)
for (i in 1: length(c)){mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 0)][c[i]] = "Not Provided"}
summary(factor(mod$NSCSCHOOLSTATE[which(mod$ENROLLED == 0)]))

# NSC School Control
#For Enrolled students
#mod$NSCSCHOOL = backup$NSCSCHOOL
summary(factor(mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 1)]))
c = which(is.na(mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 1)]))
length(c)
levels(mod$NSCSCHOOLCONTROL)[length(levels(mod$NSCSCHOOLCONTROL))+1] = "Public"
l = levels(mod$NSCSCHOOLCONTROL)
mod$NSCSCHOOLCONTROL = factor(mod$NSCSCHOOLCONTROL, levels = l)
for (i in 1: length(c)){mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 1)][c[i]] = "Public"}
summary(factor(mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 1)]))

#For non enrolled students 
summary(factor(mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 0)]))
c = which(is.na(mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 0)]))
length(c)
levels(mod$NSCSCHOOLCONTROL)[length(levels(mod$NSCSCHOOLCONTROL))+1] = "Not Provided"
levels(mod$NSCSCHOOLCONTROL)
l = levels(mod$NSCSCHOOLCONTROL)
mod$NSCSCHOOLCONTROL = factor(mod$NSCSCHOOLCONTROL, levels = l)
levels(mod$NSCSCHOOLCONTROL)
for (i in 1: length(c)){mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 0)][c[i]] = "Not Provided"}
summary(factor(mod$NSCSCHOOLCONTROL[which(mod$ENROLLED == 0)]))

#NSC Enrollment Type
#mod$NSCSCHOOL = backup$NSCSCHOOL
summary(factor(mod$NSCENROLLMENTTYPE))
c = which(is.na(mod$NSCENROLLMENTTYPE))
length(c)
levels(mod$NSCENROLLMENTTYPE)[length(levels(mod$NSCENROLLMENTTYPE))+1] = "Not Provided"
l = levels(mod$NSCENROLLMENTTYPE)
mod$NSCENROLLMENTTYPE = factor(mod$NSCENROLLMENTTYPE, levels = l)
for (i in 1: length(c)){mod$NSCENROLLMENTTYPE[c[i]] = "Not Provided"}
summary(factor(mod$NSCENROLLMENTTYPE))


#Hschname
#mod$NSCSCHOOL = backup$NSCSCHOOL
summary(factor(mod$ASUADSHSCHNAME))
c = which(is.na(mod$ASUADSHSCHNAME))
length(c)
levels(mod$ASUADSHSCHNAME)[length(levels(mod$ASUADSHSCHNAME))+1] = "Not Provided"
l = levels(mod$ASUADSHSCHNAME)
mod$ASUADSHSCHNAME = factor(mod$ASUADSHSCHNAME, levels = l)
for (i in 1: length(c)){mod$ASUADSHSCHNAME[c[i]] = "Not Provided"}
summary(factor(mod$ASUADSHSCHNAME))

#ASUCITIZENCOUNTRYCODE
summary(factor(mod$ASUCITZNCOUNTRY))
c = which(is.na(mod$ASUCITZNCOUNTRY))
length(c)
levels(mod$ASUCITZNCOUNTRY)[length(levels(mod$ASUCITZNCOUNTRY))+1] = "NP"
l = levels(mod$ASUCITZNCOUNTRY)
mod$ASUCITZNCOUNTRY = factor(mod$ASUCITZNCOUNTRY, levels = l)
for (i in 1: length(c)){mod$ASUCITZNCOUNTRY[c[i]] = "NP"}
summary(factor(mod$ASUCITZNCOUNTRY))

#ASUCITIZENCOUNTRYCODEld
summary(factor(mod$ASUCITZNCOUNTRYLD))
c = which(is.na(mod$ASUCITZNCOUNTRYLD))
length(c)
levels(mod$ASUCITZNCOUNTRYLD)[length(levels(mod$ASUCITZNCOUNTRYLD))+1] = "Not Provided"
l = levels(mod$ASUCITZNCOUNTRYLD)
mod$ASUCITZNCOUNTRYLD = factor(mod$ASUCITZNCOUNTRYLD, levels = l)
for (i in 1: length(c)){mod$ASUCITZNCOUNTRYLD[c[i]] = "Not Provided"}
sort(summary(factor(mod$ASUCITZNCOUNTRYLD)))

#ASUCITIZENCOUNTRYCODEld
summary(factor(mod$VISADESCR))
c = which(is.na(mod$VISADESCR))
length(c)
levels(mod$VISADESCR)[length(levels(mod$VISADESCR))+1] = "Not Provided"
l = levels(mod$VISADESCR)
mod$VISADESCR = factor(mod$VISADESCR, levels = l)
for (i in 1: length(c)){mod$VISADESCR[c[i]] = "Not Provided"}
sort(summary(factor(mod$VISADESCR)))

#ASUCITIZENCOUNTRYCODEld
summary(factor(mod$VISAPERMIT))
c = which(is.na(mod$VISAPERMIT))
length(c)
levels(mod$VISAPERMIT)[length(levels(mod$VISAPERMIT))+1] = "Not Provided"
l = levels(mod$VISAPERMIT)
mod$VISAPERMIT = factor(mod$VISAPERMIT, levels = l)
for (i in 1: length(c)){mod$VISAPERMIT[c[i]] = "Not Provided"}
sort(summary(factor(mod$VISAPERMIT)))

# I-20 issued flag 
# not required for USA citizens
summary(factor(mod$I20ISSUEDFLG[which(mod$ASUCITZNCOUNTRY == "USA")]))
summary(factor(mod$I20ISSUEDFLG))
c = which(is.na(mod$I20ISSUEDFLG)[which(mod$ASUCITZNCOUNTRY == "USA")])
length(c)
levels(mod$I20ISSUEDFLG)[length(levels(mod$I20ISSUEDFLG))+1] = "Not Required"
l = levels(mod$I20ISSUEDFLG)
mod$I20ISSUEDFLG = factor(mod$I20ISSUEDFLG, levels = l)
for (i in 1: length(c)){mod$I20ISSUEDFLG[which(mod$ASUCITZNCOUNTRY == "USA")][c[i]] = "Not Required"}
sort(summary(factor(mod$I20ISSUEDFLG[which(mod$ASUCITZNCOUNTRY == "USA")])))

# for enrollled students and non USA ones
summary(factor(mod$I20ISSUEDFLG[which(mod$ENROLLED == 1)]))
c = which(is.na(mod$I20ISSUEDFLG)[which(mod$ENROLLED == 1)])
length(c)
#levels(mod$I20ISSUEDFLG)[length(levels(mod$I20ISSUEDFLG))+1] = "Y"
#l = levels(mod$I20ISSUEDFLG)
#mod$I20ISSUEDFLG = factor(mod$I20ISSUEDFLG, levels = l)
for (i in 1: length(c)){mod$I20ISSUEDFLG[which(mod$ENROLLED == 1)][c[i]] = "Y"}
sort(summary(factor(mod$I20ISSUEDFLG[which(mod$ENROLLED == 1)])))

# for non enrollled students and non USA ones
summary(factor(mod$I20ISSUEDFLG[which(mod$ENROLLED == 0)]))
c = which(is.na(mod$I20ISSUEDFLG)[which(mod$ENROLLED == 0)])
length(c)
#levels(mod$I20ISSUEDFLG)[length(levels(mod$I20ISSUEDFLG))+1] = "Y"
#l = levels(mod$I20ISSUEDFLG)
#mod$I20ISSUEDFLG = factor(mod$I20ISSUEDFLG, levels = l)
for (i in 1: length(c)){mod$I20ISSUEDFLG[which(mod$ENROLLED == 0)][c[i]] = "N"}
sort(summary(factor(mod$I20ISSUEDFLG[which(mod$ENROLLED == 0)])))
summary(mod$I20ISSUEDFLG)

#FirstAPPPROG
summary(factor(mod$FIRSTAPPROG))
c = which(is.na(mod$FIRSTAPPROG))
length(c)
levels(mod$FIRSTAPPROG)[length(levels(mod$FIRSTAPPROG))+1] = "NP"
l = levels(mod$FIRSTAPPROG)
mod$FIRSTAPPROG = factor(mod$FIRSTAPPROG, levels = l)
for (i in 1: length(c)){mod$FIRSTAPPROG[c[i]] = "NP"}
sort(summary(factor(mod$FIRSTAPPROG)))

summary(factor(mod$FIRSTAPPROGDESCR))
c = which(is.na(mod$FIRSTAPPROGDESCR))
length(c)
levels(mod$FIRSTAPPROGDESCR)[length(levels(mod$FIRSTAPPROGDESCR))+1] = "Not Provided"
l = levels(mod$FIRSTAPPROGDESCR)
mod$FIRSTAPPROG = factor(mod$FIRSTAPPROGDESCR, levels = l)
for (i in 1: length(c)){mod$FIRSTAPPROGDESCR[c[i]] = "Not Provided"}
sort(summary(factor(mod$FIRSTAPPROGDESCR)))

#FirstAPPplan
summary(factor(mod$FIRSTAPPLAN))
c = which(is.na(mod$FIRSTAPPLAN))
length(c)
levels(mod$FIRSTAPPLAN)[length(levels(mod$FIRSTAPPLAN))+1] = "NP"
l = levels(mod$FIRSTAPPLAN)
mod$FIRSTAPPLAN = factor(mod$FIRSTAPPLAN, levels = l)
for (i in 1: length(c)){mod$FIRSTAPPLAN[c[i]] = "NP"}
sort(summary(factor(mod$FIRSTAPPLAN)))

summary(factor(mod$FIRSTAPPLANDESCR))
c = which(is.na(mod$FIRSTAPPLANDESCR))
length(c)
levels(mod$FIRSTAPPLANDESCR)[length(levels(mod$FIRSTAPPLANDESCR))+1] = "Not Provided"
l = levels(mod$FIRSTAPPLANDESCR)
mod$FIRSTAPPLANDESCR = factor(mod$FIRSTAPPLANDESCR, levels = l)
for (i in 1: length(c)){mod$FIRSTAPPLANDESCR[c[i]] = "Not Provided"}
sort(summary(factor(mod$FIRSTAPPLANDESCR)))

#FirstADPROG
summary(factor(mod$FIRSTADPROG))
c = which(is.na(mod$FIRSTADPROG))
length(c)
levels(mod$FIRSTADPROG)[length(levels(mod$FIRSTADPROG))+1] = "NP"
l = levels(mod$FIRSTADPROG)
mod$FIRSTADPROG = factor(mod$FIRSTADPROG, levels = l)
for (i in 1: length(c)){mod$FIRSTADPROG[c[i]] = "NP"}
sort(summary(factor(mod$FIRSTADPROG)))

summary(factor(mod$FIRSTADPROGDESCR))
c = which(is.na(mod$FIRSTADPROGDESCR))
length(c)
levels(mod$FIRSTADPROGDESCR)[length(levels(mod$FIRSTADPROGDESCR))+1] = "Not Provided"
l = levels(mod$FIRSTADPROGDESCR)
mod$FIRSTADPROGDESCR = factor(mod$FIRSTADPROGDESCR, levels = l)
for (i in 1: length(c)){mod$FIRSTADPROGDESCR[c[i]] = "Not Provided"}
sort(summary(factor(mod$FIRSTADPROGDESCR)))

#FirstADPLAN
summary(factor(mod$FIRSTADPLAN))
c = which(is.na(mod$FIRSTADPLAN))
length(c)
levels(mod$FIRSTADPLAN)[length(levels(mod$FIRSTADPLAN))+1] = "NP"
l = levels(mod$FIRSTADPLAN)
mod$FIRSTADPLAN = factor(mod$FIRSTADPLAN, levels = l)
for (i in 1: length(c)){mod$FIRSTADPLAN[c[i]] = "NP"}
sort(summary(factor(mod$FIRSTADPLAN)))

summary(factor(mod$FIRSTADPLANDESCR))
c = which(is.na(mod$FIRSTADPLANDESCR))
length(c)
levels(mod$FIRSTADPLANDESCR)[length(levels(mod$FIRSTADPLANDESCR))+1] = "Not Provided"
l = levels(mod$FIRSTADPLANDESCR)
mod$FIRSTADPLANDESCR = factor(mod$FIRSTADPLANDESCR, levels = l)
for (i in 1: length(c)){mod$FIRSTADPLANDESCR[c[i]] = "Not Provided"}
sort(summary(factor(mod$FIRSTADPLANDESCR)))

names(mod)

write.csv(mod[,-c(1,2,3,54,39,40,41,96,67,74)], "modifiedcsv3262016.csv",na = "")

#Applying random forest to predict the number of enrollments
library(randomForest)
library(rpart)
library(rpart.plot)
rf = rpart(ENROLLED ~. , mod[,-c(1,2,3,4,5,46,60,47,48,49,50)])
library(caret)
varImp(rf)
plot(rf)

columns = 
  c(
    "APPNUMBERS",
    "TERMCODE",
    "ADMITTYPEADJ",
    "CENSUSAPPCOLLEGECODE",
    "CENSUSAPPDEPTADJCODE",
    "CENSUSAPPDEGREECODE",
    "CENSUSAPPACADEMICPLANCODE",
    "GENDERCODE",
    "MKT",
    "HIGHSCHOOLCODE",
    "HIGHSCHOOLGPA",
    "HIGHSCHOOLABORGPA",
    "HIGHSCHOOLRANKPERCENT",
    "HIGHSCHOOLGRADUATIONYEAR",
    "SATCOMBINEDEXAMSCORE",
    "SATCOMBINEDEXAMSCOREADJ",
    "SATVERBALEXAMSCORE",
    "SATQUANTITATIVEEXAMSCORE",
    "ACTCOMPOSITEEXAMSCORE",
    "CALCULATEDINDEX",
    "CALCULATEDINDEXGROUP",
    "LASTTRANSFERINSTCODE",
    "CUMULATIVETRANSFERGPA",
    "STUDENTCAMPUSDESCRIPTION",
    "ADMIT",
    "PROVINCEDATA",
    "ASUADSHSCHNAME",
    "MONTHYEAR",
    "ASUCITZNCOUNTRYLD",
    "VISADESCR",
    "VISAPERMIT",
    "UGRDINTLGPA",
    "IELTSSCORE",
    "TOEFLCOMP",
    "ADMEVALCOMPLETEDT",
    "FIRSTAPPROG",
    "FIRSTAPPLAN",
    "FIRSTADPROG",
    "FIRSTADPLAN",
    "APPLICATIONYEAR",
    "STUDENTCOMPLETEDATE"
  )


names(mod)
ml = mod[,columns]
#rf = randomForest(ADMIT ~. ,  ml)
plot(rf)
varImp(rf)

summary(ml)
names(ml)
rf = rpart(factor(ADMIT) ~. ,  ml[,-c(1,10,22,23,24,35,41,14)])
plot(rf)
text(rf,all = TRUE)
prp(rf,uniform = FALSE, varlen = -20,tweak = 0.5)
rf$splits
sort(unique(ml$IELTSSCORE))