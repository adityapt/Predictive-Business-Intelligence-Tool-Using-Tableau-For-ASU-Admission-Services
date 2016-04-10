#read the data
setwd("~/PT/capstone")
dir()
mod = read.csv("modifiedcsv3302016.csv", na.strings = c("","NA"))[,c(-1,-97,-98)]
names(mod)

summary(mod$X.2)

#for admit
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
    "STUDENTCOMPLETEDATE",
    "EVALADMIT"
  )

# Deep learning using R

ml = mod[,columns]
names(ml)

install.packages("h2o")
library(h2o)

# creating a cluster

localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '2g')

#creating a dataframe in cluster
dat = ml[,-1]
dat_h2o <- as.h2o( ml)
ml$ADMIT = factor(ml$ADMIT)
#dat_h2o <- h2o.importFile(localH2O, path = "C:/Users/Aditya/Documents/capstone/modifiedcsv3262016.csv")
model <-  h2o.deeplearning(x = c(3:24,26:41),  # column numbers for predictors
                   y = 25,   # column number for label
                   training_frame  = dat_h2o, # data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(50,50,50), # three layers of 50 nodes
                   epochs = 100) # max. no. of epochs

names(ml)

yhat <- h2o.predict(model, dat_h2o)
predict = as.data.frame(yhat[,1])
nrow(predict)
nrow(ml)
names(predict)
model
table(ml$ADMIT,predict$predict)
View(cbind(ml$ADMIT,factor(predict)))
#21628/nrow(predict)
#[1] 0.8694673
