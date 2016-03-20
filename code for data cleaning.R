a = read.csv("data.csv")
names(a)
str(a)
summary(a)

# removing data with no application ID
a = a[-c(which(a$App.Number.s == '')),]
which(a$App.Number.s == '')

# imputation required before cleaning
a$Term.Code = as.factor(a$Term.Code)

summary(a$Admit.Type.Adj.Descr)
#First-Time Freshman First-Time Freshmen            Transfer 
#13145               11784                          8183 

c = which(a$Admit.Type.Adj.Descr == 'First-Time Freshmen')
for (i in 1:length(c)) {a$Admit.Type.Adj.Descr[c[i]] = 'First-Time Freshman'}

a$Admit.Type.Adj.Descr = factor(a$Admit.Type.Adj.Descr, levels = c('First-Time Freshman','Transfer'))
summary(a$Admit.Type.Adj.Descr)
#First-Time Freshman            Transfer 
#24929                          8183 

# Census.App.Division.Adj.Descr contains null data (26561 null values)
which(a$Census.App.Division.Adj.Descr == '')
which(a$Census.App.Division.Adj.Code == '')
# checking if where ever there is a null value in the Description there is a null value in the code
which((which(a$Census.App.Division.Adj.Descr == '') == which(a$Census.App.Division.Adj.Code == '')) == 'FALSE')

names(a)

summary(a[20])
#Minority.Status.Descr
#International:33112    
summary(a[21])
#MKT       
#AZ   :  111  
#INTER:32993  
#WUE  :    8  
a$High.School.Code = as.factor(a$High.School.Code)

# looking at the GPA of the students who are admitted (not enrolled)
table(a$High.School.Gpa, a$Admitted.Flag == 'Y')
table(a$High.School.Abor.Gpa, a$Admitted.Flag == 'Y')
table(a$Cumulative.Transfer.Gpa, a$Admitted.Flag == 'Y')

#frame = as.data.frame(table(a$Sat.Combined.Exam.Score,is.na(a$High.School.Gpa) == 'TRUE', a$Admitted.Flag ))

for (i in 1:length(which(is.na(a[54] == 'TRUE')))) {a$Application.Complete[c[i]] = 0}
c = which(is.na(a[55] == 'TRUE'))
for (i in 1:length(c)) {a[,55][c[i]] = 0}
c = which(is.na(a[56] == 'TRUE'))
for (i in 1:length(c)) {a[,56][c[i]] = 0}
c = which(is.na(a[57] == 'TRUE'))
for (i in 1:length(c)) {a[,57][c[i]] = 0}
c = which(is.na(a[58] == 'TRUE'))
for (i in 1:length(c)) {a[,58][c[i]] = 0}
c = which(is.na(a[66] == 'TRUE'))
for (i in 1:length(c)) {a[,66][c[i]] = 0}
write.csv(a ,"modifieddata.csv")


mod = read.csv("modifieddata.csv")

summary(mod$Census.App.College.Code)
summary(mod$Census.App.College.Short.Descr)

#AS    BA    CS    ES    HI    LA    LS    NH    NU    PP    SU    TB    TE    TS    UC 
#511 10700   212  9014  2250  6551   741   591   234   499    33    32   335  1223   186 

mod$ApplicationYear = format(as.Date(mod$Application.Date, format = '%m/%d/%Y'), '%Y')
mod$ApplicationYear = as.numeric(mod$ApplicationYear)
table(mod$ApplicationYear)
#2009 2010 2011 2012 2013 2014 2015 
#857  3253 3862 4931 7246 7337 5626

applied = table(mod$ApplicationYear, mod$Application.Complete)
appliedperc = round(prop.table(a,1),3)

table = data.frame(prop.table(table(mod$ApplicationYear, mod$Application.Complete, mod$Admitted.Flag),1))
table1 = data.frame(table(mod$ApplicationYear, mod$Application.Complete, mod$Admitted.Flag))
names(table) = c("Year", "Applied", "Admitted", "%centage")
names(table1) = c("Year", "Applied", "Admitted", "count")
plot(table1$Year[which(table1$Admitted == 'Y')], table1$count[which(table1$Admitted == 'Y')])
plot(table1$Year[which(table1$Admitted == 'N')], table1$count[which(table1$Admitted == 'N')])
#the number of admits in 2015 fell down
plot(table1$Year[which(table1$Admitted == 'Y' & table1$Applied == '1' )], table1$count[which(table1$Admitted == 'Y' & table1$Applied == '1' )])


#No of applications in 2015?
#use aggregate
completedappl = data.frame(table(mod$ApplicationYear,mod$Application.Complete))
names(completedappl) = c("Year", "Completed", "Count")
plot(completedappl$Year[which(completedappl$Completed == 1)], completedappl$Count[which(completedappl$Completed == 1)])

#less number of applications were received in 2015
#2013 peaked the number of international applications

matrixdataframe = as.data.frame.matrix(table(mod$Census.App.College.Short.Descr[which(mod$Application.Complete ==1)], mod$ApplicationYear[which(mod$Application.Complete ==1)]))
plot(table(mod$Census.App.College.Code[which(mod$Application.Complete ==1)], mod$ApplicationYear[which(mod$Application.Complete ==1)]))
