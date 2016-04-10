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
    "FIRSTAPPROG",
    "FIRSTAPPLAN",
    "FIRSTADPROG",
    "FIRSTADPLAN",
    "APPLICATIONYEAR",
    "EVALADMIT"
  )

# Deep learning using R

ml = mod[,columns]
ml = ml[which(ml$ADMITTYPEADJ == "FTF"),]
ml = ml[,c(11,12,13,15,16,17,18,19,32,33,34,39)]
summary(ml)
names(ml)

install.packages("caret")
library(caret)
## Remove zero and close to zero variance

names(ml)
nzv <- nearZeroVar(ml, saveMetrics = TRUE)
range(nzv$percentUnique)

# how many have no variation at all
print(length(nzv[nzv$zeroVar==T,]))

print(paste('Column count before cutoff:',ncol(ml)))

# how many have less than 0.1 percent variance
dim(nzv[nzv$percentUnique > 0.1,])

# remove zero & near-zero variance from original data set
ml_nzv <- ml[c(rownames(nzv[nzv$percentUnique > 0.1,])) ]
print(paste('Column count after cutoff:',ncol(ml_nzv)))

View(ml_nzv)
str(ml_nzv)

library(caret)
install.packages("PCAmixdata")
library(PCAmixdata)
names(ml)
pp <- preProcess(ml[,-c(which(names(ml)== "ADMIT"))], method="pca")
ml[,-c(which(names(ml)== "ADMIT"))] = predict (pp, ml[,-c(which(names(ml)== "ADMIT"))])
names(ml)
knnFit1 <- train(UGRDINTLGPA~., ml[,c(11,12,13,14,15,16,17,18,19,32,33,34,39)], method = "knn", preProcess=c("pca"), 
                 trControl = trainControl(method = "cv"))

library(randomForest)
library(rpart)

summary(ml[,c(11,12,13,14,15,16,17,18,19,32,33,34,39)])

rf = rpart(UGRDINTLGPA~., ml[,c(11,12,13,14,15,16,17,18,19,32,33,34,39)])
rf$variable.importance
names(ml[,c(11,12,13,14,15,16,17,18,19,32,33,34,39)])
pred = predict(rf,ml)
data = as.data.frame(cbind(pred,ml$UGRDINTLGPA))
names(data) = c("pred","actual")
data$diff = data[2]-data[1]
summary(data$diff)
which(data$diff == min(data$diff))

names(data)
class(data)
varImp
#HIGHSCHOOLGRADUATIONYEAR 100.0000
#TOEFLCOMP                 24.9947
#APPLICATIONYEAR           18.1827
#HIGHSCHOOLGPA             16.5850
#IELTSSCORE                 4.7368
#HIGHSCHOOLABORGPA          3.0791
#HIGHSCHOOLRANKPERCENT      0.7720
#SATVERBALEXAMSCORE         0.7174
#SATCOMBINEDEXAMSCORE       0.7119
#SATCOMBINEDEXAMSCOREADJ    0.6873
#SATQUANTITATIVEEXAMSCORE   0.6485
#ACTCOMPOSITEEXAMSCORE      0.0000
names(ml_nzv)
names(knnFit2)
knnFit2$bestTune
varImp(knnFit2)

#PC6  100.00
#PC3   93.42
#PC2   92.02
#PC5   65.57
#PC4   32.50
#PC1    0.00

knnFit1
library(randomForest)

knnFit3 <- train(UGRDINTLGPA~., ml_nzv, method = "RFlda", preProcess=c("pca"), 
                 trControl = trainControl(method = "cv"))

names(ml_nzv)
train = ml_nzv[which(ml$APPLICATIONYEAR != 2015),]
test = ml_nzv[which(ml$APPLICATIONYEAR == 2015),]
nrow(train)
train = as.matrix(train)
test = as.matrix(test)
dimnames(train)[2]
library(xgboost)
str(train)
str(ml)
xg = xgboost(data = train,label = train[,9], param <- list("objective" = "reg:linear",
                                                     "num_class" = 9,
                                                     "nthread" = 8,
                                                     "eta" = 0.08,
                                                     "subsample"=0.8,
                                                     "gamma" = 1,
                                                     "min_child_weight" = 2,
                                                     "max_depth"= 12,
                                                     "colsample_bytree" = 1
), watchlist = list(validation1 = test), verbose = 2, nrounds = 16)

summary(xg)
pred = predict(xg,test)
as.data.frame(cbind(round(pred,2),test[,9]))
nrow(test)
nrow(train)
dimnames(test)
dimnames(ml)[[2]]
ml@dimnames[[2]]
imp_matrix <- xgb.importance(feature_names = dimnames(train)[[2]], model = xg)
print(imp_matrix)

#Feature         Gain        Cover   Frequence
#1:              UGRDINTLGPA 9.999999e-01 9.409670e-01 0.943866944
#2:            HIGHSCHOOLGPA 3.657261e-08 4.475233e-02 0.022869023
#3:    HIGHSCHOOLRANKPERCENT 9.822082e-09 4.706039e-03 0.002079002
#4:                TOEFLCOMP 8.053077e-09 9.469771e-03 0.012474012
#5:          APPLICATIONYEAR 5.828880e-09 7.492500e-06 0.003118503
#6:    ACTCOMPOSITEEXAMSCORE 4.319277e-09 5.244750e-06 0.002079002
#7:     SATCOMBINEDEXAMSCORE 4.045704e-09 2.903344e-05 0.003118503
#8: SATQUANTITATIVEEXAMSCORE 3.349219e-09 2.210288e-05 0.002079002
#9:        HIGHSCHOOLABORGPA 2.528649e-09 8.991000e-06 0.003118503
#10:               IELTSSCORE 1.166328e-09 8.991000e-06 0.003118503
#11:  SATCOMBINEDEXAMSCOREADJ 4.028740e-10 2.303944e-05 0.002079002

#final model with very less error

#               Feature         Gain        Cover   Frequence

#1:          UGRDINTLGPA 9.999999e-01 0.9984017786 0.970000000
#2:        HIGHSCHOOLGPA 7.645545e-08 0.0007941361 0.016666667
#3:      APPLICATIONYEAR 1.662110e-08 0.0001266276 0.006666667
#4:            TOEFLCOMP 6.716341e-09 0.0001013021 0.003333333
#5: SATCOMBINEDEXAMSCORE 7.072606e-10 0.0005761557 0.003333333

names(ml)

summary(ml)
#########################################################################
#reading the data into R 
#########################################################################

install.packages("xlsx")
library(xlsx)

install.packages("rJava")
library(rJava)
dir()

mod = read.csv("modifiedcsv04062016.csv",na.strings = c("","NA"))
mod = mod[which(mod$ADMITTYPEADJ == "FTF"),]

columns = 
  c(
    "TERMCODE",
    "ADMITTYPEADJ",
    "CENSUSAPPCOLLEGECODE",
    "CENSUSAPPDEPTADJCODE",
    "CENSUSAPPDEGREECODE",
    "CENSUSAPPACADEMICPLANCODE",
    "GENDERCODE",
    "HIGHSCHOOLCODE",
    "HIGHSCHOOLGPA",
    "HIGHSCHOOLABORGPA",
    "HIGHSCHOOLRANKPERCENT",
    "SATCOMBINEDEXAMSCORE",
    "SATCOMBINEDEXAMSCOREADJ",
    "SATVERBALEXAMSCORE",
    "SATQUANTITATIVEEXAMSCORE",
    "ACTCOMPOSITEEXAMSCORE",
    "CALCULATEDINDEX",
    "CALCULATEDINDEXGROUP",
    "STUDENTCAMPUSDESCRIPTION",
    "ADMITTEDFLAG",
    "PROVINCEDATA",
    "ASUADSHSCHNAME",
    "MONTHYEAR",
    "ASUCITZNCOUNTRYLD",
    "VISADESCR",
    "VISAPERMIT",
    "UGRDINTLGPA",
    "IELTSSCORE",
    "TOEFLCOMP",
    "FIRSTAPPROG",
    "FIRSTAPPLAN",
    "FIRSTADPROG",
    "FIRSTADPLAN",
    "APPLICATIONYEAR",
    "EVALADMIT",
    "AppStrToStudComp",
    "StudCompToEvalComp"
      )

ml = mod[,columns]

rf = rpart(ADMITTEDFLAG ~ ., ml)
rf$variable.importance
plot(rf)
pred = predict(rf,ml, type = "class")
pred
length(pred)
table(pred, ml$ADMITTEDFLAG)
#pred     N     Y
#N    12935   658
#Y    703 10631

(12935+10631)/nrow(ml)
#0.9454006

install.packages("ROCR")
library(ROCR)

pred = predict(rf,ml)
l = prediction(pred[,2], ml$ADMITTEDFLAG)
perf = performance(l,"tpr","fpr")
(auc = as.numeric(performance(l,"auc")@y.values))
#[1] 0.9501584

#AUC curve
plot(perf)
names(ml)
nrow(ml)
dummies <- dummyVars(ADMIT ~ ., data = ml)
names(dummies)
predict(dummies, newdata = ml)
nzv = nearZeroVar(dummies, saveMetrics = TRUE)
    
fit <- train(ADMITTEDFLAG~., dummies, method = "nnet", preProcess=c("pca"), 
                 trControl = trainControl(method = "cv"))


