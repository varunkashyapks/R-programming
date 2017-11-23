setwd("D:/sois 2nd sem/MLRLAB")
d=read.csv("chd.csv",header=TRUE)
View(d)
dim(d)
library(MASS)

#first we consider AGE

#formula defines relation
#by dewfault considers link as logit
logmod=glm(formula=CHD~AGE,data=d,family=binomial(link="logit"))
summary(logmod) 
#here, we do not get the value for e^beta, where it is used for the interpretation

#extracting coefficiants
logmod$coefficients
#now extract residuals and fitted values
logmod$residuals
logmod$fitted.values

#in null deviance - we initially have residual and null deviance..
#null deviance- it is a model using oli beta not, -2 log liklihood.  
#in residual deviance.. we fit the model with covariance taking -2 log liklihood
#in chisquare- we take the difference of df
#pearson residual y-pi 
#and if y-pi is divided by sqrt(pi*(1-pi)) it becomes standardised pearson residuals
logmod$null.deviance
logmod$aic

plot(logmod)

#anova
anova(logmod,test="Chisq")


#NOW CONSIDER AGRP

logmod1=glm(formula=CHD~AGRP,data=d,family=binomial(link="logit"))
summary(logmod1)



#for multiple linear regression
data(mtcars)
help(mtcars)
#here considering vs and am (response variables)
mod=glm(formula=am~cyl+hp+wt,data=mtcars,family=binomial())
summary(mod)
anova(mod,test="Chisq")


#covraiance is dichotomous
