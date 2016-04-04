setwd('~/capstone')
dir()
mod = read.csv("modifiedcsv3182016.csv")
mod = mod[mod$ADMITTYPEADJ == "TRN",]
nrow(mod)
#8183

# Working on all the columns
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
for ( i in 1: nrow(mod)){mod$CUMULATIVETRANSFERGPA[i] = ifelse(is.na(mod$CUMULATIVETRANSFERGPA[i]),0,mod$CUMULATIVETRANSFERGPA[i]) }
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
c
length(levels(mod$FIRSTGENERATIONFLAG))
levels(mod$FIRSTGENERATIONFLAG)[length(levels(mod$FIRSTGENERATIONFLAG))+1] = "NP"
l = levels(mod$FIRSTGENERATIONFLAG)
l
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

summary(mod)
