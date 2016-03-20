# modified dataset with modified names
setwd("~/capstone")
mod = read.csv("modifieddata.csv")
names(mod)
mod$ApplicationYear = format(as.Date(mod$ApplicationDate, format = '%m/%d/%Y'), '%Y')
mod$ApplicationYear = as.numeric(mod$ApplicationYear)
summary(mod$ApplicationYear)
write.csv(mod, "modifieddata.csv")
library(sqldf)
# to get the count of persons who applied and enrolled
frequencyapplycount = sqldf("select a.*, (enrolled*100/applied)||'%' as enrollperc from 
                            (select ApplicationYear,count(AdmittedFlag) as applied, sum(Enrolled) as enrolled 
                            from mod group by ApplicationYear) as a
                            group by a.ApplicationYear") 

#ApplicationYear applied enrolled enrollperc
#1            2009      857      123        14%
#2            2010     3253      452        13%
#3            2011     3862      655        16%
#4            2012     4931      804        16%
#5            2013     7246     1128        15%
#6            2014     7337     1316        17%
#7            2015     5626      856        15%

frequencyadmitcount = sqldf("select a.*, (enrolled*100/admitted)||'%' as enrollperc from (select ApplicationYear,sum(Admit) as admitted, sum(Enrolled) as enrolled from mod group by ApplicationYear) as a group by a.ApplicationYear") 
frequencyapplyadmitcount = sqldf("select a.*, (enrolled*100/admitted)||'%' as enrolladmitperc, (enrolled*100/applied)||'%' as enrollapplyperc from (select ApplicationYear,sum(Admit) as admitted,count(AdmittedFlag) as applied, sum(Enrolled) as enrolled from mod group by ApplicationYear) as a group by a.ApplicationYear") 
#     ApplicationYear admitted applied enrolled          enrolladmitperc enrollapplyperc
#  1            2009      436     857      123             28%             14%
#  2            2010     1297    3253      452             34%             13%
#  3            2011     1701    3862      655             38%             16%
#  4            2012     2227    4931      804             36%             16%
#  5            2013     3080    7246     1128             36%             15%
#  6            2014     3338    7337     1316             39%             17%
#  7            2015     2243    5626      856             38%             15%

# to find the count of freshmen & every year

# inconsistency in levels in ASUADSFprovince
length(levels(mod$AsuAdsFprovince))
#[1] 699
mod$AsuAdsFprovince = as.factor(tolower(mod$AsuAdsFprovince))
length(levels(mod$AsuAdsFprovince)) 
#[1] 522
# lots of cleaning to be done for this column ! lol it contains 522 levels in which some are the same and are just repeating
length(levels(mod$AsuAdsHschname))
#[1] 7908
mod$AsuAdsHschname = as.factor(tolower(mod$AsuAdsHschname))
length(levels(mod$AsuAdsHschname)) 
#[1] 7438

# till now lets write the data to a file called modifiedcsv(3/7/2016)
write.csv(mod, "modifiedcsv372016.csv")
mod = read.csv("modifiedcsv372016.csv")
name = data.frame(names(mod))
#trying to find the rows with 'province' in them in AsuAdsFprovince, replacing with ''
#mod = sqldf("#select b.*, 
#            #case 
#            #when b.Fnoprovince like "%select%" then '' 
#            #when b.Fnoprovince like "%-%" then ''
#            #when trim(b.Fnoprovince) like 
mod = sqldf("select a.*, trim(replace(replace(replace(replace(replace(AsuAdsFprovince,' province',''),' city',''),' district',''),' provide',''),' china',''))  as Fnoprovince from mod as a")
mod$Fnoprovince = as.factor(mod$Fnoprovince)
mod$AsuAdsFprovince  = as.factor(mod$AsuAdsFprovince)
levels(mod$Fnoprovince)
length(levels(mod$AsuAdsFprovince))
#[1] 522
length(levels(mod$Fnoprovince)) 
#456
# creating a column for month year
mod$monthYear = NULL
mod$monthYear = paste(format(as.Date(mod$ApplicationDate, format = '%m/%d/%Y'), '%b'), mod$ApplicationYear,sep = " ")
mod$monthYear = as.factor(mod$monthYear)
mod$month = format(as.Date(mod$ApplicationDate, format = '%m/%d/%Y'), '%m')
mod$month = as.factor(mod$month)
length(levels(mod$monthYear))
#[1] 73
length(levels(mod$month))
monthwisedata = sqldf("select monthYear, count(*) from mod group by monthYear order by ApplicationYear, month ")
# plotting the number of applications received for each year in a cross tab format
# for the students only who have completed the application (hence using application received column)
AppliedCandidateData = subset(mod , ApplicationComplete == 1)
monthwisedata = table(AppliedCandidateData$month,format(as.Date(AppliedCandidateData$ApplicationDate, format = '%m/%d/%Y'), '%Y'))
#     2009 2010 2011 2012 2013 2014 2015
#01    0  422  502  598 1103  987 1315
#02    0  234  299  571 1017  624  983
#03    0  243  321  694  607  514  787
#04    0  175  191  254  421  385  502
#05    0   76   45   66   82   88  103
#06    0   27   10   34   41   61   56
#07    0    5   11   31   48   55   17
#08    8   15   30   32   65   65    1
#09   20   33   44   63  144  134    0
#10   61   67  133  152  235  281    0
#11  133  150  211  338  324  395    0
#12  270  266  483  621  597  760    0

names(mod)
# this tells us if there is atleast one attribute present for all the students
which(is.na(mod$HighSchoolGpa)== 'TRUE'& is.na(mod$HighSchoolAborGpa)== 'TRUE'& is.na(mod$HighSchoolRankPercent)== 'TRUE' )
summary(mod$SatCombinedExamScore)
which(mod$SatCombinedExamScore== '')
mod$Admit[which(is.na(mod$HighSchoolGpa)== 'TRUE'& is.na(mod$HighSchoolAborGpa)== 'TRUE'& is.na(mod$HighSchoolRankPercent)== 'TRUE' & mod$SatCombinedExamScore== '' & mod$SatCombinedExamScoreAdj== '' )]
nrow(mod)
#------------------------------------------------------------------------------------
#3/9/2016 looking at other columns 
#------------------------------------------------------------------------------------
setwd("~/capstone")

mod = read.csv("modifiedcsv372016.csv")
nrow(mod)
#[1] 33112

# creating a dataframe which contains the names of all the columns of the dataset
name = data.frame(names(mod))

summary(mod$MKT)
#AZ INTER   WUE 
#111 32993     8

summary(mod$HighSchoolCode)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#1.100e+09 1.000e+11 1.000e+11 8.577e+10 1.000e+11 1.000e+11       393 

summary(mod$HighSchoolDescr)
length(which(mod$HighSchoolDescr == ""))
#[1] 393

# Just a Quality check!
length(which(mod$HighSchoolDescr == "" & is.na(mod$HighSchoolCode) == "TRUE"))
#[1] 393

summary(mod$HighSchoolStateCode)
length(which(mod$HighSchoolStateCode == ""))
#[1] 28964  28964 empty cells in Highschoolstatecode

summary(mod$HighSchoolGraduationYear)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1964    2010    2012    2011    2013    2015   22803 

## lol! this is comedy. 6783 students who havent provided their graduation year have gotten an admit
length(which(is.na(mod$HighSchoolGraduationYear) == "TRUE" & mod$AdmittedFlag == "Y"))
#[1] 6783

summary(mod$AdmittedFlag)
#N     Y 
#18790 14322 

table(mod$Admit)
#0     1 
#18790 14322 

length(which(mod$AdmittedFlag == "Y" & mod$Admit == 1))
#[1] 14322

summary(mod$CalculatedIndex)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#51.0    89.0   103.0   100.5   113.0   144.0   30598 

table(mod$CalculatedIndex, mod$CalculatedIndexGroup)
summary(mod$CalculatedIndexGroup)
#         < 86 103 - 107 108 - 110 111 - 120 121 - 128 129 - 146   86 - 93  94 - 102  No Index 
#8183       507       320       214       477       195        95       291       415     22415 
#Makes sense

# 12588 people whose index wasn't calculated got an admit (OMG)
length(which(is.na(mod$CalculatedIndex) == "TRUE" & mod$Admit == 1))
#[1] 12588

#replacing the non existent index with 0 (for easy analysis purpose)
c = which(is.na(mod$CalculatedIndex == 'TRUE'))
for (i in 1:length(c)) {mod$CalculatedIndex[c[i]] = 0}

length(which(is.na(mod$CalculatedIndex) == "TRUE" & mod$Admit == 1))
#0
summary(mod$CalculatedIndex)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.00    7.63    0.00  144.00 

#--------------------------------------------------------------------------------------------------
# It can be seen that people who previously had NA in CalculatedIndex (Now 0) are in two categories
# some in "" and others in "No Index" .. The ones in "" are transfer students. 
# We are creating a new level called "Transfer"
#--------------------------------------------------------------------------------------------------
c = which(mod$CalculatedIndexGroup == '')
length(which(mod$CalculatedIndexGroup == '') & which(mod$AdmitTypeAdj == "TRN"))
#8183
c
length(c)
#8183
levels(mod$CalculatedIndexGroup)
# Trying to create a new level for Transfer students
mod$CalculatedIndexGroup = factor(mod$CalculatedIndexGroup, levels = c("Transfer","< 86","103 - 107", "108 - 110", "111 - 120", "121 - 128", "129 - 146", "86 - 93", "94 - 102", "No Index"))
levels(mod$CalculatedIndexGroup)
#[1] "Transfer" "< 86" "103 - 107" "108 - 110" "111 - 120" "121 - 128" "129 - 146" "86 - 93"   "94 - 102"  "No Index" 

summary(mod$CalculatedIndexGroup)
#Transfer      < 86 103 - 107 108 - 110 111 - 120 121 - 128 129 - 146   86 - 93  94 - 102  No Index      NA's 
#     0         507       320       214      477       195        95       291       415     22415      8183 
c = which(is.na(mod$CalculatedIndexGroup) == "TRUE")
length(c)
#8183

for (i in 1:length(c)) {mod$CalculatedIndexGroup[c[i]] = "Transfer"}
levels(mod$CalculatedIndexGroup)
#[1] "Transfer"  "< 86"      "103 - 107" "108 - 110" "111 - 120" "121 - 128" "129 - 146" "86 - 93"   "94 - 102" 
#[10] "No Index"

summary(mod$CalculatedIndexGroup)
#Transfer      < 86 103 - 107 108 - 110 111 - 120 121 - 128 129 - 146   86 - 93  94 - 102  No Index 
#   8183       507       320       214       477       195        95       291       415     22415 

table(mod$CalculatedIndex, mod$CalculatedIndexGroup)
plot(table(mod$CalculatedIndex, mod$CalculatedIndexGroup))

table(mod$CalculatedIndexGroup, mod$Admit)
  
#CalculatedIndexGroup     Admit?
#                       0     1
#Transfer             5152  3031
#< 86                 273   234
#103 - 107            63   257
#108 - 110            40   174
#111 - 120            76   401
#121 - 128            22   173
#129 - 146            7    88
#86 - 93              152   139
#94 - 102             147   268
#No Index             12858  9557

write.csv(mod[-1], "modifiedcsv392016.csv")

#------------------------------------------------------------------------------------------------
# We have to ask why index is not calculated for transfer students!
#________________________________________________________________________________________________

#-------------------------------------------------------------------------------------------------
#As the index increases the chances of getting an admit increases!
#-------------------------------------------------------------------------------------------------
mod = read.csv("modifiedcsv392016.csv")
names(mod)
names(mod)[1] = "Row"
names(mod)
# Transfer columns
summary(mod$AdmitTypeAdj)
#FTF   TRN 
#24929  8183 

summary(mod$CumulativeTransferGpa)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   2.620   3.010   2.983   3.440   4.000   28943 

nrow(mod) - 28943
#4169

#4169 transfer students out of 8183 have provided their transfer gpa!

library(sqldf)
 sqldf("select Admit,AdmitTypeAdj, count(AdmitTypeAdj)  from mod where CumulativeTransferGpa is null group by Admit, AdmitTypeAdj ")
 #    Admit AdmitTypeAdj    count(AdmitTypeAdj)
 #1     0          FTF               13586
 #2     0          TRN                3876
 #3     1          FTF               11137
 #4     1          TRN                 344
 
#344 transfer students who didnt give their cumulative transfer gpa have received an admit
 length(levels(mod$Fnoprovince))
 #456
 names(mod)
 #mod$X = NULL
 mod$row = NULL
 names(mod)
 names(mod)[1] = "Row"

sqldf("select Admit,AdmitTypeAdj,CalculatedIndex,CalculatedIndexGroup,SatVerbalExamScore,
      SatCombinedExamScore, ActCompositeExamScore,SatCombinedExamScoreAdj,SatQuantitativeExamScore, count(AdmitTypeAdj)  from mod where CumulativeTransferGpa = 0 group by Admit, AdmitTypeAdj,CalculatedIndex ")
#Admit AdmitTypeAdj CalculatedIndex CalculatedIndexGroup count(AdmitTypeAdj)
#1     0          TRN               0             Transfer                   1
#2     1          TRN               0             Transfer                   3

sqldf("select * from mod where CumulativeTransferGpa = 0")
# They are exchange students! may be they have come in their 4th year of college from some other country

summary(mod$SatQuantitativeExamScore)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#200.0   540.0   610.0   605.9   680.0   800.0   27797

summary(mod$ActCompositeExamScore)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#7.00   20.00   22.00   22.34   25.00   35.00   31614

summary(mod$CalculatedIndex[which(mod$CalculatedIndex > 0 & mod$Admit == 1)])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#51.0    96.0   107.0   104.6   116.0   144.0 

summary(mod$CalculatedIndex[which(mod$CalculatedIndex > 0 & mod$Admit == 1)])
names(mod)


