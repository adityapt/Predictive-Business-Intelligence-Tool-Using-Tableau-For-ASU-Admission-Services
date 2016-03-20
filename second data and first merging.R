setwd("~/capstone")
second = read.csv("second dataset.csv")
first = read.csv("modifiedcsv3152016.csv")
#dummy = read.csv("second dataset.csv")
names(first)
dim(second)
dim(first)
names(second)
name = as.data.frame(names(second))
library(sqldf)
names(name)[1] = "column"
names(name)
name = sqldf("select a.*, replace(column,'.','') as newname from name as a")
name

# replacing the names of columns in second dataset with this new colummn
name[,2]
names(second)  = name[,2]
names(second)

name2 = data.frame(names(second))
name1 = data.frame(names(first))
names(name1) = "first"
names(name2) = "second"
names(second) = toupper(names(second))

#taking only the new columns
newcolumns = sqldf("select upper(second)  from name2 where upper(second) not in (select upper(first) from name1) ")

# merging datasets on appnumbers column

first$APPNUMBERS = as.factor(first$APPNUMBERS)
#second$APPNUMBERS = dummy$App.Number.s

# replacing comma in the appnumbers column in the second dataset
second$APPNUMBERS= as.factor(gsub(",","",second$APPNUMBERS))

sqldf("select count(APPNUMBERS) from first where appnumbers in (select APPNUMBERS from second)")
second$APPNUMBERS
first$APPNUMBERS

# creating a new dataset to merge newcolumns with the first
#for (i in 1: newcolumns){print(summary(second[i]))}

# adding appnumbers column to columnnames list
columnnames = c(1) # 1 is for appnumbers
for (i in 1:length(newcolumns[,1])){columnnames[i+1] = (which(names(second)== newcolumns[i,1]))}

# subsetting the seconddataset 

columnnames
names(second[,columnnames])

mod = merge(first, second[,columnnames],by = "APPNUMBERS")

# a small correction

mod$PROVINCEDATA[which(mod$PROVINCEDATA == "jiang su")]
mod$PROVINCEDATA[which(mod$PROVINCEDATA == "jiang su")] = "jiangsu"
mod$PROVINCEDATA[which(mod$PROVINCEDATA == "jiang su")]
levels(mod$PROVINCEDATA)
mod$PROVINCEDATA = factor(mod$PROVINCEDATA)
levels(mod$PROVINCEDATA)

#writing the dataset to directory
write.csv(mod, "modifiedcsv3182016.csv")

#Exploring the merged dataset
names(mod)
summary(mod$ASUCITZNCOUNTRYLD)

table(mod$ASUCITZNCOUNTRYLD,mod$PROVINCEDATA)

length(which(mod$ASUCITZNCOUNTRYLD == ''))
#15
#thats awesome only 15 missing values for country

summary(mod$UGRDINTLGPA)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   3.080   3.500   3.396   3.800   4.000   19449 

mod$ADMIT[which(mod$UGRDINTLGPA == 0)]
#[1] 0 1 0 0 0 0 0 0 0 0 0 1 0 0

mod$APPNUMBERS[which(mod$ADMIT[which(mod$UGRDINTLGPA == 0)] == 1)]
#[1] 1000260 1001079
# these 2 guys have 0 internationalgpa but have an admit

hist(mod$UGRDINTLGPA)
sort(table(mod$UGRDINTLGPA))

country =factor(mod$ASUCITZNCOUNTRYLD[which(tolower(mod$ASUCITZNCOUNTRYLD) == "china")])
province = factor(mod$PROVINCEDATA[which(tolower(mod$ASUCITZNCOUNTRYLD) == "china")])

table(country,province)
# a small correction
mod$PROVINCEDATA[which(mod$PROVINCEDATA == "jiang su")] = "jiangsu"
mod$PROVINCEDATA = factor(mod$PROVINCEDATA)
levels(mod$PROVINCEDATA)

# checking the provincedata for the nocountry data

unique(mod$PROVINCEDATA[which(mod$ASUCITZNCOUNTRYLD == '')])
#Not Provided

View(as.data.frame(sort(names(mod))))
mod$IELTSSCORE[which(mod$ADMIT==1)]

unique(mod$ADMIT[which(mod$I20ISSUEDFLG == 'Y')])
#0 1

#people who didnt get an admoit also have received an I20??
# lets see who they are!

length(which(mod$ADMIT[which(mod$I20ISSUEDFLG == 'Y')] == 0))
#421
i20noadmit = which(mod$I20ISSUEDFLG == 'Y' & mod$ADMIT == 0)
i20noadmit[1]
mod$ADMIT[i20noadmit]
# people who have not received an admit but have an i20- checking if they have something in common!
# couldn't find any though
View(mod[i20noadmit,])

mod$CENSUSAPPDEGREEDESCR[which(mod$ADMIT[which(mod$I20ISSUEDFLG == 'Y')] == 0)]
mod$CENSUSAPPDEPTADJDESCR[which(mod$ADMIT[which(mod$I20ISSUEDFLG == 'Y')] == 0)]

unique(mod$ENROLLED[which(mod$I20ISSUEDFLG == 'N')])
#0 1
length(which(mod$I20ISSUEDFLG == 'N' & mod$ENROLLED == 1) )
#392

noi20enrolled = which(mod$I20ISSUEDFLG == 'N' & mod$ENROLLED == 1)
View(mod[noi20enrolled,])

length(unique(mod$APPNUMBERS))
#33112
nrow(mod)
#33112

#checking the new columns
newcolumns$`upper(second)`
library(sqldf)
sqldf("select distinct admit, IELTSSCORE,TOEFLCOMP from mod order by 1,2,3")

#getting the column names and datatypes
class(names(mod))
a = list()
for ( i in 1: length(names(mod)))
{
  a[i] = class(mod[,names(mod)[i]])
}
b = cbind(names(mod),unlist(a))
print(b)

library(randomForest)
names(mod)

rf = randomForest(ADMITTEDFLAG ~ ., mod, method = "class")
library(rpart)
rf = rpart(ADMITTEDFLAG ~ CALCULATEDINDEX+CALCULATEDINDEXGROUP+HIGHSCHOOLGPA+HIGHSCHOOLABORGPA+HIGHSCHOOLRANKPERCENT+HIGHSCHOOLDESCR+CENSUSAPPDIVISIONADJDESCR+APPLICATIONCOMPLETE+ SATQUANTITATIVEEXAMSCORE , mod, method = "class")
library(rpart.plot)
prp(rf)
predict(rf,mod)
table(mod$ADMIT,predict(rf,mod)[,2]>0.5)
