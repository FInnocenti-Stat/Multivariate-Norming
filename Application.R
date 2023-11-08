############################################################################################################################
############################## APPLICATION
#############################################################################################################################
rm(list=ls(all=T))

library(haven)
library(car)
library(pastecs)
library(mvnormtest)
library(mvoutlier)
library(mnormt)
library(rgl)
library(MASS)
library(moments)
library(heplots)
library(dplyr)
library(MVN)
library(psych)

MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr <- read_sav("D:\\data_application\\MAAS_6-10-2022\\MAAS_Stroop+Demographics_AxF0_PIN_no_Regnr.sav")

#MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr <- read_sav("I:/FI_PhD_Project/4_Paper_4/2_Versions_Submitted/5_Submission_5/MAAS_6-10-2022/MAAS_Stroop+Demographics_AxF0_PIN_no_Regnr.sav")
#MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr <- read_sav("Z:/FI_PhD_Project/4_Paper_4/2_Versions_Submitted/5_Submission_5/MAAS_6-10-2022/MAAS_Stroop+Demographics_AxF0_PIN_no_Regnr.sav")

View(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr)

############################################################
head(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr)

#### DESCRIPTIVES
# outcome variables
par(mfrow=c(2,2))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(str1))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(str2))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(str3))


par(mfrow=c(2,2))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, boxplot(str1))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, boxplot(str2))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, boxplot(str3))

par(mfrow=c(2,2))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, qqp(str1))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, qqp(str2))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, qqp(str3))


summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr)


pairs.panels(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6], 
             method = "pearson", # correlation method
             hist.col = "blue",
             density = TRUE,  # show density plots
             ellipses =  F # show correlation ellipses
)

# Stroop test rationale: time to complete a task increases with task complexity
colMeans(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6])
summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,5]-MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4])
summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,6]-MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,5])

MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,5]-MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4]<0),]
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,6]-MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,5]<0),]

summary(mahalanobis(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6],colMeans(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6]),cov(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6])))
mahalanobis(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6],colMeans(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6]),cov(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[,4:6]))[373]



# predictors
summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Age)
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Sex
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Education
summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Education)
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$strstat
sum(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$strstat!=1)

par(mfrow=c(2,2))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(Age))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(Sex))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(Education))
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(strstat))

# recode sex: 1=M, 0=F
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Sex=ifelse(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Sex==2,0,1)
# Low Education = 1 (elementary, lower vocational), Average Education = 2 (intermediate secondary and vocational, higher secondary)
# High Education = 3 (higher vocational, university, scientific)
Edu_cat<-ifelse(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Education<3,1,ifelse(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Education>2 & MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Education<6,2,3))
par(mfrow=c(1,1))
hist(Edu_cat)

par(mfrow=c(2,2))
hist(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Age, main="Distribution of Age",xlab = "Age")
hist(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$Sex, main="Distribution of Sex",xlab = "Sex")
hist(Edu_cat, main="Distribution of Education",xlab = "Educational Level")



####### DATA CLEANING 

# remove subjects to whom the administration of the test was not complete i.e. strstat is not 1
par(mfrow=c(1,1))

# according to https://andi.nl/tests/aandacht-en-werkgeheugen/stroop/ minimum and maximum allowable values for Stroop test scores are:
# Stroop task 1: min = 15, max = 100
summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1)
sum(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1<15 | MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1>100)
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1<15 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1>100,]
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(str1),xlim=c(0,max(str1)))
abline(v=c(15,100),col="red")
# Stroop task 2: min = 15, max = 100
summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2) # one observation close to lower boundary
sum(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2<15 | MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2>100)
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2<15 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2>100,]
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(str2))
abline(v=c(15,100),col="red")
which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2==16.3)
# Stroop task 3: min = 15, max = 400
summary(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3)
sum(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3<15 | MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3>400)
MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr[MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3<15 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3>400,]
with(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, hist(str3))
abline(v=c(15,400),col="red")

filter_imposs_values=ifelse(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$strstat>1,1,ifelse(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1>100 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2>100 | MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2<16.5, 1,0 ))
which(filter_imposs_values==1)
which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1<15 | MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1>100)
which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2<16.5 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2>100)
which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3<15 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3>400)
which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$strstat>1)
all(which(filter_imposs_values==1)==sort(unique(c(which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1>100),which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2<16.5 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2>100),which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3>400),which(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$strstat>1)))))

setwd("C:\\Users\\finno\\OneDrive\\Desktop\\Lavoro\\FI_PhD_Project\\4_Paper_4\\2_Versions_Submitted\\4_Submission_5\\Figures application")
jpeg(file="MinimumMaximumStroop.jpg",width=5000, height=5000, res=600)
par(mfrow=c(3,3), mar = c(4, 4, 4, 1))

hist(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1,xlim = c(0,120),breaks=c(seq(0,120,10)),xlab="Stroop 1",main = "Distribution of Stroop 1")
abline(v=c(15,100),col="red")
hist(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2,xlim = c(0,120),breaks=c(seq(0,120,10)),xlab="Stroop 2",main = "Distribution of Stroop 2")
abline(v=c(15,100),col="red")
hist(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3,xlim = c(0,500),breaks=c(seq(0,500,50)),xlab="Stroop 3",main = "Distribution of Stroop 3")
abline(v=c(15,400),col="red")

boxplot(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1,horizontal = T,ylim=c(0,120),xlab="Stroop 1",main="Distribution of Stroop 1")
abline(v=c(15,100),col="red")
boxplot(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2,horizontal = T,ylim=c(0,120),xlab="Stroop 2",main="Distribution of Stroop 2")
abline(v=c(15,100),col="red")
boxplot(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str3,horizontal = T,ylim=c(0,500),xlab="Stroop 3",main="Distribution of Stroop 3")
abline(v=c(15,400),col="red")
dev.off()


# Exclude 23 observations with unreliable or impossible or extreme values
Stroop_data_possval=subset(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, subset = filter_imposs_values==0 )
dim(Stroop_data_possval)
summary(Stroop_data_possval)
View(Stroop_data_possval)


par(mfrow=c(2,2))
with(Stroop_data_possval, boxplot(str1))
with(Stroop_data_possval, boxplot(str2))
with(Stroop_data_possval, boxplot(str3))


# outcome variables given the predictors
pairs.panels(Stroop_data_possval[,c(4:6)], 
             method = "pearson", # correlation method
             hist.col = "blue",
             density = TRUE,  # show density plots
             ellipses =  F # show correlation ellipses
)


#### NEW VARIABLES

Age_cent<-Stroop_data_possval$Age-mean(Stroop_data_possval$Age)
Age2_cent<-Age_cent^2

quantile(Stroop_data_possval$Education)
# Low Education = 1 (elementary, lower vocational), Average Education = 2 (intermediate secondary and vocational, higher secondary)
# High Education = 3 (higher vocational, university, scientific)
Edu_cat<-ifelse(Stroop_data_possval$Education<3,1,ifelse(Stroop_data_possval$Education>2 & Stroop_data_possval$Education<6,2,3))

par(mfrow=c(2,2))
hist(Stroop_data_possval$Age, main="Distribution of Age",xlab = "Age")
hist(Stroop_data_possval$Sex, main="Distribution of Sex",xlab = "Sex")
hist(Edu_cat, main="Distribution of Education",xlab = "Educational Level")

Edu_cat<-factor(Edu_cat,c(1,2,3),labels = c("Low Education", "AVerage Education", "High Education"))
Edu_cat
class(Edu_cat)

attach(Stroop_data_possval)

######################################### MULTIVARIATE MULTIPLE LINEAR REGRESSION #########################################################


############## Full model
fullmodel<-lm(cbind(str1,str2,str3)~ Sex+Age_cent+Age2_cent+Edu_cat+Sex*Age_cent+Sex*Age2_cent+Sex*Edu_cat+Edu_cat*Age_cent+Edu_cat*Age2_cent,  data=data.frame(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat))
summary(fullmodel)

manova_fullmodel<-Anova(fullmodel,type="III")
manova_fullmodel
summary(manova_fullmodel)


mod5<-lm(cbind(str1,str2,str3)~ Sex+Age_cent+Age2_cent+Edu_cat,  data=data.frame(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat))
Anova(mod5,type="III")

# overall test for all interactions
anova(mod5,fullmodel)

# Final model:
summary(mod5)

###### Sensitivity analysis: Model selection with observation removed because str2=16.3
filter_Sens=ifelse(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$strstat>1,1,ifelse(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str1>100 |MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr$str2>100, 1,0 ))

data_Sens=subset(MAAS_Stroop_Demographics_AxF0_PIN_no_Regnr, subset = filter_Sens==0 )
dim(data_Sens)

Age_cent_Sens<-data_Sens$Age-mean(data_Sens$Age)
Age2_cent_Sens<-Age_cent_Sens^2
Edu_cat_Sens<-ifelse(data_Sens$Education<3,1,ifelse(data_Sens$Education>2 & data_Sens$Education<6,2,3))
Edu_cat_Sens<-factor(Edu_cat_Sens,c(1,2,3),labels = c("Low Education", "AVerage Education", "High Education"))


fullmodel_Sens<-lm(cbind(str1,str2,str3)~ Sex+Age_cent_Sens+Age2_cent_Sens+Edu_cat_Sens+Sex*Age_cent_Sens+Sex*Age2_cent_Sens+Sex*Edu_cat_Sens+Edu_cat_Sens*Age_cent_Sens+Edu_cat_Sens*Age2_cent_Sens,  data=data.frame(data_Sens,Age_cent_Sens,Age2_cent_Sens,Edu_cat_Sens))
mod5_Sens<-lm(cbind(str1,str2,str3)~ Sex+Age_cent_Sens+Age2_cent_Sens+Edu_cat_Sens,  data=data.frame(data_Sens,Age_cent_Sens,Age2_cent_Sens,Edu_cat_Sens))
anova(mod5_Sens,fullmodel_Sens)
summary(mod5_Sens)

rm(filter_Sens, data_Sens, Age_cent_Sens, Age2_cent_Sens, Edu_cat_Sens, fullmodel_Sens, mod5_Sens)

###### Assumptions

# Outliers
# univariate
influenceIndexPlot(lm(Stroop_data_possval$str1~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),vars =c("Cook", "Studentized"),main="Stroop 1" )
influenceIndexPlot(lm(Stroop_data_possval$str2~ Sex+Age_cent+Age2_cent+Edu_cat,  data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),vars =c("Cook", "Studentized"),main="Stroop 2" )
influenceIndexPlot(lm(Stroop_data_possval$str3~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),vars =c("Cook", "Studentized"),main="Stroop 3" )

dim(cooks.distance(mod5))
summary(cooks.distance(mod5)) # all Cook's distances below 1
dim(rstudent(mod5))
summary(rstudent(mod5))


# multivariate
summary(mahalanobis(mod5$residuals,center=colMeans(mod5$residuals),cov=cov(mod5$residuals)))
par(mfrow=c(1,1))
qqPlot(mahalanobis(mod5$residuals,center=colMeans(mod5$residuals),cov=cov(mod5$residuals)),dist="chisq", df=3,xlab="Chisq df=3 quantiles",ylab="squared Mahalanobis distance")
abline(v=qchisq(p=c(0.99),df=3))

par(mfrow=c(2,2))
Boxplot(rstudent(mod5)[,1],ylab="Stud. Residuals Stroop 1")
Boxplot(rstudent(mod5)[,2],ylab="Stud. Residuals Stroop 2")
Boxplot(rstudent(mod5)[,3],ylab="Stud. Residuals Stroop 3")
qqPlot(mahalanobis(mod5$residuals,center=colMeans(mod5$residuals),cov=cov(mod5$residuals)),dist="chisq", df=3,xlab="Chisq df=3 quantiles",ylab="Mahalanobis distance")

Outliers=which(abs(rstudent(mod5))[,1]>3.5 | abs(rstudent(mod5))[,2]>3.5 | abs(rstudent(mod5))[,3]>3.5 
      & mahalanobis(mod5$residuals,center=colMeans(mod5$residuals),cov=cov(mod5$residuals))>qchisq(p=c(0.99),df=3))
Outliers
Stroop_data_possval[Outliers,]

## Normality
# 1) univariate
par(mfrow=c(2,2), mar = c(5, 5, 4, 2)-1)
qqPlot(mod5$residuals[,1],dist="norm", ylab="Residuals Stroop 1")
qqPlot(mod5$residuals[,2],dist="norm", ylab="Residuals Stroop 2")
qqPlot(mod5$residuals[,3],dist="norm", ylab="Residuals Stroop 3")
mtext("Univariate Normality", side = 3, line = - 2, outer = TRUE)

par(mfrow=c(1,1))
mvn(mod5$residuals,mvnTest = "royston",univariateTest = "SW", multivariatePlot  = "qq")


# 2) Bivariate
par(mfrow=c(2,2), mar = c(5, 5, 4, 2)-1)
qqPlot(mahalanobis(mod5$residuals[,-1],center=colMeans(mod5$residuals[,-1]),cov=cov(mod5$residuals[,-1])),dist="chisq", df=2,xlab="Chisq df=2 quantiles, Stroop 2 & 3",ylab="squared Mahalanobis distance")
qqPlot(mahalanobis(mod5$residuals[,-2],center=colMeans(mod5$residuals[,-2]),cov=cov(mod5$residuals[,-2])),dist="chisq", df=2,xlab="Chisq df=2 quantiles, Stroop 1 & 3",ylab="squared Mahalanobis distance")
qqPlot(mahalanobis(mod5$residuals[,-3],center=colMeans(mod5$residuals[,-3]),cov=cov(mod5$residuals[,-3])),dist="chisq", df=2,xlab="Chisq df=2 quantiles, Stroop 1 & 2",ylab="squared Mahalanobis distance")
mtext("Bivariate Normality", side = 3, line = - 2, outer = TRUE)

mardia(mod5$residuals[,-1]) # multivariate skewness and kurtosis
mardia(mod5$residuals[,-2]) # multivariate skewness and kurtosis
mardia(mod5$residuals[,-3]) # multivariate skewness and kurtosis

mvn(mod5$residuals[,-1],mvnTest = "hz") # Henze-Zirkler test of multivariate normality
mvn(mod5$residuals[,-2],mvnTest = "hz") 
mvn(mod5$residuals[,-3],mvnTest = "hz") 

mvn(mod5$residuals[,-1],mvnTest = "royston") # Royston test of multivariate normality
mvn(mod5$residuals[,-2],mvnTest = "royston") 
mvn(mod5$residuals[,-3],mvnTest = "royston") 

#Multivariate
par(mfrow=c(1,1))
qqPlot(mahalanobis(mod5$residuals,center=colMeans(mod5$residuals),cov=cov(mod5$residuals)),dist="chisq", df=3,xlab="Chisq df=3 quantiles",ylab="squared Mahalanobis distance")
mtext("Multivariate Normality", side = 3, line = - 2, outer = TRUE)
mardia(mod5$residuals) # multivariate skewness and kurtosis
mvn(mod5$residuals,mvnTest = "hz",multivariatePlot = "qq") # Henze-Zirkler test of multivariate normality
mvn(mod5$residuals,mvnTest = "royston") # royston test of multivariate normality


## Homoscedasticity
# Variances
par(mfrow=c(2,2))
residualPlot(lm(str1~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),type="rstandard",quadratic=F)
residualPlot(lm(str2~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),type="rstandard",quadratic=F)
residualPlot(lm(str3~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),type="rstandard",quadratic=F)

scatterplot((mod5$fitted.values[,1]-colMeans(mod5$fitted.values)[1])/sd(mod5$fitted.values[,1]),rstandard(mod5)[,1],xlab="Stand. Predicted Values Stroop 1",ylab = "Stand. Residuals Stroop 1", boxplots = F,smooth  = F)
scatterplot((mod5$fitted.values[,2]-colMeans(mod5$fitted.values)[2])/sd(mod5$fitted.values[,2]),rstandard(mod5)[,2],xlab="Stand. Predicted Values Stroop 2",ylab = "Stand. Residuals Stroop 2", boxplots = F,smooth  = F)
scatterplot((mod5$fitted.values[,3]-colMeans(mod5$fitted.values)[3])/sd(mod5$fitted.values[,3]),rstandard(mod5)[,3],xlab="Stand. Predicted Values Stroop 3",ylab = "Stand. Residuals Stroop 3", boxplots = F,smooth  = F)


# Correlations
# Stroop 1 and 2
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,2] | Stroop_data_possval$Sex,given.values = c(0,1),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Sex"),show.given = F)
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,2] | Age_cent,number=6,ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Age"),show.given = F)
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,2] | Edu_cat,given.values = c(1,2,3),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Educational Level"),show.given = F)
# Stroop 1 and 3
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,3] | Stroop_data_possval$Sex,given.values = c(0,1),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Sex"),show.given = F)
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,3] | Age_cent,number=6,ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Age"),show.given = F)
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,3] | Edu_cat,given.values = c(1,2,3),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Educational Level"),show.given = F)
# Stroop 3 and 2
coplot(rstandard(mod5)[,3]~rstandard(mod5)[,2] | Stroop_data_possval$Sex,given.values = c(0,1),ylab="Stand. Res. Stroop 3",xlab=c("Stand. Res. Stroop 2", "Sex"),show.given = F)
coplot(rstandard(mod5)[,3]~rstandard(mod5)[,2] | Age_cent,number=6,ylab="Stand. Res. Stroop 3",xlab=c("Stand. Res. Stroop 2", "Age"),show.given = F)
coplot(rstandard(mod5)[,3]~rstandard(mod5)[,2] | Edu_cat,given.values = c(1,2,3),ylab="Stand. Res. Stroop 3",xlab=c("Stand. Res. Stroop 2", "Educational Level"),show.given = F)

#1) divide sample in quartiles based on the dist. of mahalanobis distance of predicted values
MD_fitted=mahalanobis(mod5$fitted.values,center = colMeans(mod5$fitted.values), cov=cov(mod5$fitted.values))
quantile(MD_fitted)
groups=rep(0,times=length(MD_fitted))
groups=ifelse(MD_fitted>quantile(MD_fitted)[4],4,ifelse(MD_fitted>quantile(MD_fitted)[3],3,ifelse(MD_fitted>quantile(MD_fitted)[2],2,ifelse(MD_fitted>quantile(MD_fitted)[1],1,1))))
all(MD_fitted[groups==4]>quantile(MD_fitted)[4])
all(MD_fitted[groups==3]>quantile(MD_fitted)[3])
all(MD_fitted[groups==2]>quantile(MD_fitted)[2])
all(MD_fitted[groups==1]<quantile(MD_fitted)[2])
length(MD_fitted[groups==4]);length(MD_fitted[groups==3]);length(MD_fitted[groups==2]);length(MD_fitted[groups==1])

#2) Test Homogeneity of covariance matrix with the Box-M test

boxM(mod5$residuals,group=groups)

# Correlations
# Stroop 1 and 2
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,2] | groups,given.values = c(1:4),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Mahalanobis distance of predicted scores"),show.given = F)
# Stroop 1 and 3
coplot(rstandard(mod5)[,1]~rstandard(mod5)[,3] | groups,given.values = c(1:4),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Mahalanobis distance of predicted scores"),show.given = F)
# Stroop 3 and 2
coplot(rstandard(mod5)[,3]~rstandard(mod5)[,2] | groups,given.values = c(1:4),ylab="Stand. Res. Stroop 3",xlab=c("Stand. Res. Stroop 2", "Mahalanobis distance of predicted scores"),show.given = F)

###### TRANSFORMATION FOR MULTIVARIATE NORMALITY
?powerTransform()

#1) Find best power with Box-Cox  
powerTransform(mod5) #(i.e. the Box-Cox procedure is applied to the conditional distribution of the response given the predictors )
#2) Apply the Box-Cox transformation
Stroop1_trans=((Stroop_data_possval$str1^(-0.923) -1)/-0.923)
Stroop2_trans=((Stroop_data_possval$str2^(-0.543) -1)/-0.543)
Stroop3_trans=((Stroop_data_possval$str3^(-0.746) -1)/-0.746)


#3) To improve interpretability: standardization
Stroop1_stand=(Stroop1_trans-mean(Stroop1_trans))/sd(Stroop1_trans)
Stroop2_stand=(Stroop2_trans-mean(Stroop2_trans))/sd(Stroop2_trans)
Stroop3_stand=(Stroop3_trans-mean(Stroop3_trans))/sd(Stroop3_trans)

TRANSFScore=data.frame(Stroop1_stand,Stroop2_stand,Stroop3_stand)
colnames(TRANSFScore)=c("Stroop 1","Stroop 2","Stroop 3")
pairs.panels(TRANSFScore, 
             method = "pearson", # correlation method
             hist.col = "blue",
             density = TRUE,  # show density plots
             ellipses =  F# show correlation ellipses 
)

par(mfrow=c(2,2), mar = c(5, 5, 4, 2)-1)
hist(Stroop1_stand,xlim = c(-4,4),xlab="Stroop 1",main = "Transformed Stroop 1")
hist(Stroop2_stand,xlim = c(-4,4),xlab="Stroop 2",main = "Transformed Stroop 2")
hist(Stroop3_stand,xlim = c(-4,4),xlab="Stroop 3",main = "Transformed Stroop 3")


fullmodelTrans<-lm(cbind(Stroop1_stand,Stroop2_stand,Stroop3_stand)~ Sex+Age_cent+Age2_cent+Edu_cat+Sex*Age_cent+Sex*Age2_cent+Sex*Edu_cat+Edu_cat*Age_cent+Edu_cat*Age2_cent,  data=data.frame(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat))
dim(fullmodelTrans$residuals)
mod5Trans<-lm(cbind(Stroop1_stand,Stroop2_stand,Stroop3_stand)~ Sex+Age_cent+Age2_cent+Edu_cat,  data=data.frame(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat,Stroop1_stand,Stroop2_stand,Stroop3_stand))
dim(mod5Trans$residuals)
summary(mod5Trans)
Anova(mod5Trans,type="III")
anova(mod5Trans,fullmodelTrans)

# Outliers
# univariate
influenceIndexPlot(lm(Stroop1_stand~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),vars =c("Cook", "Studentized"),main="Stroop 1" )
influenceIndexPlot(lm(Stroop2_stand~ Sex+Age_cent+Age2_cent+Edu_cat,  data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),vars =c("Cook", "Studentized"),main="Stroop 2" )
influenceIndexPlot(lm(Stroop3_stand~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)),vars =c("Cook", "Studentized"),main="Stroop 3" )

dim(cooks.distance(mod5Trans))
summary(cooks.distance(mod5Trans)) # all Cook's distances below 1
dim(rstudent(mod5Trans))
summary(rstudent(mod5Trans))


# multivariate
summary(mahalanobis(mod5Trans$residuals,center=colMeans(mod5Trans$residuals),cov=cov(mod5Trans$residuals)))
qchisq(p=c(0.99),df=3)
par(mfrow=c(1,1))
qqPlot(mahalanobis(mod5Trans$residuals,center=colMeans(mod5Trans$residuals),cov=cov(mod5Trans$residuals)),dist="chisq", df=3,xlab="Chisq df=3 quantiles",ylab="squared Mahalanobis distance")

par(mfrow=c(2,2))
Boxplot(rstudent(mod5Trans)[,1],ylab="Stud. Residuals Stroop 1")
Boxplot(rstudent(mod5Trans)[,2],ylab="Stud. Residuals Stroop 2")
Boxplot(rstudent(mod5Trans)[,3],ylab="Stud. Residuals Stroop 3")
qqPlot(mahalanobis(mod5Trans$residuals,center=colMeans(mod5Trans$residuals),cov=cov(mod5Trans$residuals)),dist="chisq", df=3,xlab="Chisq df=3 quantiles",ylab="Mahalanobis distance")



# Normality
# 1) univariate
par(mfrow=c(2,2), mar = c(5, 5, 4, 2)-1)
qqPlot(mod5Trans$residuals[,1],dist="norm", ylab="Residuals Stroop 1")
qqPlot(mod5Trans$residuals[,2],dist="norm", ylab="Residuals Stroop 2")
qqPlot(mod5Trans$residuals[,3],dist="norm", ylab="Residuals Stroop 3")
mtext("Univariate Normality", side = 3, line = - 2, outer = TRUE)

par(mfrow=c(1,1))
mvn(mod5Trans$residuals,mvnTest = "royston",univariateTest = "SW", multivariatePlot  = "qq")

# 2) Bivariate
par(mfrow=c(2,2), mar = c(5, 5, 4, 2)-1)
qqPlot(mahalanobis(mod5Trans$residuals[,-1],center=colMeans(mod5Trans$residuals[,-1]),cov=cov(mod5Trans$residuals[,-1])),dist="chisq", df=2,xlab="Chisq df=2 quantiles, Stroop 2 & 3",ylab="squared Mahalanobis distance")
qqPlot(mahalanobis(mod5Trans$residuals[,-2],center=colMeans(mod5Trans$residuals[,-2]),cov=cov(mod5Trans$residuals[,-2])),dist="chisq", df=2,xlab="Chisq df=2 quantiles, Stroop 1 & 3",ylab="squared Mahalanobis distance")
qqPlot(mahalanobis(mod5Trans$residuals[,-3],center=colMeans(mod5Trans$residuals[,-3]),cov=cov(mod5Trans$residuals[,-3])),dist="chisq", df=2,xlab="Chisq df=2 quantiles, Stroop 1 & 2",ylab="squared Mahalanobis distance")
mtext("Bivariate Normality", side = 3, line = - 2, outer = TRUE)

mardia(mod5Trans$residuals[,-1]) # multivariate skewness and kurtosis
mardia(mod5Trans$residuals[,-2]) # multivariate skewness and kurtosis
mardia(mod5Trans$residuals[,-3]) # multivariate skewness and kurtosis

mvn(mod5Trans$residuals[,-1],mvnTest = "hz",multivariatePlot = "qq") # Henze-Zirkler test of multivariate normality
mvn(mod5Trans$residuals[,-2],mvnTest = "hz",multivariatePlot = "qq") 
mvn(mod5Trans$residuals[,-3],mvnTest = "hz",multivariatePlot = "qq") 


mvn(mod5Trans$residuals[,-1],mvnTest = "royston",multivariatePlot = "qq") # Royston test of multivariate normality
mvn(mod5Trans$residuals[,-2],mvnTest = "royston",multivariatePlot = "qq") 
mvn(mod5Trans$residuals[,-3],mvnTest = "royston",multivariatePlot = "qq") 

#Multivariate
par(mfrow=c(1,1))
qqPlot(mahalanobis(mod5Trans$residuals,center=colMeans(mod5Trans$residuals),cov=cov(mod5Trans$residuals)),dist="chisq", df=3,xlab="Chisq df=3 quantiles",ylab="squared Mahalanobis distance")
mtext("Multivariate Normality", side = 3, line = - 2, outer = TRUE)

mardia(mod5Trans$residuals) # multivariate skewness and kurtosis

mvn(mod5Trans$residuals,mvnTest = "hz",multivariatePlot = "qq")      # Henze-Zirkler test of multivariate normality
mvn(mod5Trans$residuals,mvnTest = "royston",multivariatePlot = "qq") # Royston test

## Homoscedasticity
# variances

par(mfrow=c(2,2))
scatterplot((mod5Trans$fitted.values[,1]-colMeans(mod5Trans$fitted.values)[1])/sd(mod5Trans$fitted.values[,1]),rstandard(mod5Trans)[,1],xlab="Stand. Predicted Values Stroop 1",ylab = "Stand. Residuals Stroop 1", boxplots = F,smooth  = F)
scatterplot((mod5Trans$fitted.values[,2]-colMeans(mod5Trans$fitted.values)[2])/sd(mod5Trans$fitted.values[,2]),rstandard(mod5Trans)[,2],xlab="Stand. Predicted Values Stroop 2",ylab = "Stand. Residuals Stroop 2", boxplots = F,smooth  = F)
scatterplot((mod5Trans$fitted.values[,3]-colMeans(mod5Trans$fitted.values)[3])/sd(mod5Trans$fitted.values[,3]),rstandard(mod5Trans)[,3],xlab="Stand. Predicted Values Stroop 3",ylab = "Stand. Residuals Stroop 3", boxplots = F,smooth  = F)


# correlations

coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,2] | Stroop_data_possval$Sex,given.values = c(0,1),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Sex"),show.given = F)
coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,2] | Age_cent,number=6,ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Age"),show.given = F)
coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,2] | Edu_cat,given.values = c(1,2,3),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Educational Level"),show.given = F)

coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,3] | Stroop_data_possval$Sex,given.values = c(0,1),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Sex"),show.given = F)
coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,3] | Age_cent,number=6,ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Age"),show.given = F)
coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,3] | Edu_cat,given.values = c(1,2,3),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Educational Level"),show.given = F)


coplot(rstandard(mod5Trans)[,2]~rstandard(mod5Trans)[,3] | Stroop_data_possval$Sex,given.values = c(0,1),ylab="Stand. Res. Stroop 2",xlab=c("Stand. Res. Stroop 3", "Sex"),show.given = F)
coplot(rstandard(mod5Trans)[,2]~rstandard(mod5Trans)[,3] | Age_cent,number=6,ylab="Stand. Res. Stroop 2",xlab=c("Stand. Res. Stroop 3", "Age"),show.given = F)
coplot(rstandard(mod5Trans)[,2]~rstandard(mod5Trans)[,3] | Edu_cat,given.values = c(1,2,3),ylab="Stand. Res. Stroop 2",xlab=c("Stand. Res. Stroop 3", "Educational Level"),show.given = F)


#1) divide sample in quartiles based on the dist. of mahalanobis distance of predicted values
MD_fitted=mahalanobis(mod5Trans$fitted.values,center = colMeans(mod5Trans$fitted.values), cov=cov(mod5Trans$fitted.values))
quantile(MD_fitted)
groups=rep(0,times=length(MD_fitted))
groups=ifelse(MD_fitted>quantile(MD_fitted)[4],4,ifelse(MD_fitted>quantile(MD_fitted)[3],3,ifelse(MD_fitted>quantile(MD_fitted)[2],2,ifelse(MD_fitted>quantile(MD_fitted)[1],1,1))))
all(MD_fitted[groups==4]>quantile(MD_fitted)[4])
all(MD_fitted[groups==3]>quantile(MD_fitted)[3])
all(MD_fitted[groups==2]>quantile(MD_fitted)[2])
all(MD_fitted[groups==1]<quantile(MD_fitted)[2])
length(MD_fitted[groups==4]);length(MD_fitted[groups==3]);length(MD_fitted[groups==2]);length(MD_fitted[groups==1])



#2) Test Homogeneity of covariance matrix with the Box-M test

boxM(mod5Trans$residuals,group=groups)

coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,2] | groups,given.values = c(1:4),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 2", "Mahalanobis distance of the predicted scores"),show.given = F)
coplot(rstandard(mod5Trans)[,1]~rstandard(mod5Trans)[,3] | groups,given.values = c(1:4),ylab="Stand. Res. Stroop 1",xlab=c("Stand. Res. Stroop 3", "Mahalanobis distance of the predicted scores"),show.given = F)
coplot(rstandard(mod5Trans)[,3]~rstandard(mod5Trans)[,2] | groups,given.values = c(1:4),ylab="Stand. Res. Stroop 3",xlab=c("Stand. Res. Stroop 2", "Mahalanobis distance of the predicted scores"),show.given = F)


# Multicollinearity
vif(lm(Stroop1_stand~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)))
vif(lm(Stroop2_stand~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)))
vif(lm(Stroop3_stand~ Sex+Age_cent+Age2_cent+Edu_cat, data=cbind(Stroop_data_possval,Age_cent,Age2_cent,Edu_cat)))


################################################################################################################ 
############################# Classification of multivariate performance ###################################### 
################################################################################################################ 

Zscores=rstandard(mod5Trans)
colnames(Zscores)=c("Stroop 1","Stroop 2","Stroop 3")

pairs.panels(Zscores, 
             method = "pearson", # correlation method
             hist.col = "blue",
             density = TRUE,  # show density plots
             ellipses =  F# show correlation ellipses 
)

cor(Zscores) # pairwise correlations between standardized residuals

# Squared Mahalanobis distance
MD=mahalanobis(mod5Trans$residuals,center=colMeans(mod5Trans$residuals),cov=cov(mod5Trans$residuals))
quantile(MD,p=seq(.80,1,by=.02))
sort(MD)[round(977*seq(.80,1,by=.02))]

ID=c(70,118,761,842,825,543,147,912,226,23,55) # take from 80th to 100th percentile of squared Mahalanobis distance
ID
cbind(Zscores[ID,],sqrt(MD[ID]))

setwd("C:\\Users\\finno\\OneDrive\\Desktop\\Lavoro\\FI_PhD_Project\\4_Paper_4\\2_Versions_Submitted\\4_Submission_5\\Figures application")
jpeg(file="Classification.jpg",width=5000, height=5000, res=600)
par(mfrow=c(2,2), mar = c(4, 4, 4, 1))

##### Mahalanobis Distance for P=3
dotchart(sqrt(MD[ID]),pch=c(0:6,8,15,17,19),main = "Mahalanobis distance",lwd=2,xlim=c(2,4.2),xlab = "Mahalanobis distance, P=3",ylab="Subject Id number")
abline(v=c(sqrt(qchisq(c(.9,.95),3))),lty=c(2,1))

###### Hotelling's T

SampSize=dim(Zscores)[1]
SampSize
P=dim(Zscores)[2]
P
k=dim(model.matrix(mod5Trans))[2]-1 # number of predictors
k
DesMatrix=model.matrix(mod5Trans) # design matrix
DesMatrix
#Multiplier=(1/((SampSize+1)/SampSize))*((SampSize-P)/((SampSize-1)*P)) # Wrong
Pred_var=ID
Multiplier=ID
T2=ID
for(i in 1:length(Pred_var)){
Pred_var[i]=t(DesMatrix[ID[i],])%*%solve(t(DesMatrix)%*%DesMatrix)%*%(DesMatrix[ID[i],])

Multiplier[i]=(SampSize-k-P)*(1/P)*(1/(SampSize-k-1))*(1/(1+Pred_var[i]))

T2[i]=Multiplier[i]*MD[ID[i]]
}
T2

T2Wrong=(1/((SampSize+1)/SampSize))*((SampSize-P)/((SampSize-1)*P))*MD[ID] # based on Wrong correction
cbind(T2,T2Wrong)

dotchart(T2,pch=c(0:6,8,15,17,19),lwd=2,xlim=c(1,6),main = "Hotelling's T2",xlab = "Hotelling's T2, P=3",ylab="Subject Id number")
abline(v=qf(c(.9,.95),P,SampSize-k-P),lty=c(2,1))

###### Disjunctive and conjunctive rule

plot(1:3,Zscores[ID[1],],
     ylim=c(-4,3),
     main = "Disjunctive and Conjunctive Rule",
     xaxt="n",
     ylab="Z score",
     xlab="Stroop test sub-task",
     type="b",
     lty=3,
     pch=0,
     lwd=1)

lines(1:3,Zscores[ID[2],], type="b",pch=1, lwd=1,lty=3)
lines(1:3,Zscores[ID[3],], type="b",pch=2, lwd=1,lty=3)
lines(1:3,Zscores[ID[4],], type="b",pch=3, lwd=1,lty=3)
lines(1:3,Zscores[ID[5],], type="b",pch=4, lwd=1,lty=3)
axis(side = 1, at = seq(1,3,1), labels = c("Stroop 1", "Stroop 2", "Stroop 3"))
abline(h=c(qnorm(c(.025,.975))),lty=1)
legend("bottom",legend=c(ID[1:5]),lty=0,
       pch=0:4,lwd=2, ncol=2, bty="n",cex=1.2)

plot(1:3,Zscores[ID[6],],
     ylim=c(-4,3),
     main = "Disjunctive and Conjunctive Rule",
     xaxt="n",
     ylab="Z score",
     xlab="Stroop test sub-task",
     type="b",
     lty=3,
     pch=5,
     lwd=1)
lines(1:3,Zscores[ID[7],], type="b",pch=6, lwd=1,lty=3)
lines(1:3,Zscores[ID[8],], type="b",pch=8, lwd=1,lty=3)
lines(1:3,Zscores[ID[9],], type="b",pch=15, lwd=1,lty=3)
lines(1:3,Zscores[ID[10],], type="b",pch=17, lwd=1,lty=3)
lines(1:3,Zscores[ID[11],], type="b",pch=19, lwd=1,lty=3)
axis(side = 1, at = seq(1,3,1), labels = c("Stroop 1", "Stroop 2", "Stroop 3"))
abline(h=c(qnorm(c(.025,.975))),lty=1)
legend("bottom",legend=c(ID[6:11]),lty=0,
       pch=c(5,6,8,15,17,19),lwd=2, ncol=2, bty="n",cex=1.2)

dev.off()

################################################################################################################ 
######################################## Sample size calculation ############################################### 
################################################################################################################ 
library(chi)
P=3
PR<-c(0.90,0.95,0.99) # percentile rank score
PR
qchi(p=PR, df=P, ncp = 0, lower.tail = TRUE, log.p = FALSE)
sqrt(qchisq(p=PR,df=P))

# Function to compute the sample size for the Mahalanobis distance based approach
N_star_Mahala<-function(alpha,gamma,k,Mahala_c,delta_Mahala){
  
  N<-((qnorm(1-alpha)*sqrt(k+1+((Mahala_c^2)/2))+qnorm(1-gamma)*sqrt(k+1+(((Mahala_c+delta_Mahala)^2)/2)))/delta_Mahala)^2
  return(N)
}

# cut-off points for P=3: 2.5 (90th percentile), 2.8 (95th percentile)

ME=(2.8-2.5)/2 # margin of error = 50% of the distance between 90th and 95th percentile
N_star_Mahala(alpha=0.05/2,gamma=0.5,k=5,Mahala_c=2.5,delta_Mahala=ME) 
N_star_Mahala(alpha=0.05/2,gamma=0.5,k=5,Mahala_c=2.8,delta_Mahala=ME) 
