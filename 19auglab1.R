x=matrix(c(2,3,4,8,11,13,4,8,3),nrow=3,ncol=3,byrow=TRUE)
print(x)
x1=c(2,3,4)
x2=c(8,11,13)
x3=c(4,8,13)
a=rbind(x1,x2,x3)
a
b=cbind(x1,x2,x3)
b
transpose=t(x)
transpose
ex=x[2,3]
ex
ex1=x[3,1]
ex1
ex2=x[,1]
ex2
ex3=x[1,]
ex3
mult=a*b
mult
library(MASS)
inv=ginv(x%*%t(x))
inv
data=read.csv("1.csv",header=TRUE)
data
m=as.matrix(data)
m
m1=m[,2]
m1
d=dim(data)
d
s=summary(data)
s
cat("the required matrix is \n")
print("the required matrix is ")
print(data) 

y=read.csv("2.csv",header=TRUE)
y
cat("The given data set is \n")
print(y) 





###### Simple linear regression ########
d=read.csv("2.csv",header=TRUE)
d
x=d[,1]
y=d[,2]
mean(x)
mean(y)
x1=x-mean(x)
x1
y1=y-mean(y)
y1
b1=sum(x1*y1)/sum(x1^2)
b1
b0=mean(y)-b1*mean(x)
b0
cat("the estimations of regression coefficiants are",b0,"and",b1)
yc=b0+b1*x
yc
y

r=cor(x,y)
cat("correlation coefficiant is",r)
res=y-yc
res
cat("The residuals are")
print(res)

#ss due to regression
ssreg=sum((yc-mean(y))^2)
ssreg
cat("regression sum of squares is ",ssreg)

#ss due to residuals
ssres=sum(res^2)
ssres
cat("residual sum of squares is",ssres)

#total ss
totalss=sum(y1^2)
totalss
cat("total sum of squares is",totalss)

#if having ssreg and ssres ..can find totalss by adding ssreg and ssres without the formula
totalsss=ssreg+ssres
totalsss

#mean ss due to regression
meanssreg=ssreg/1
meanssreg
cat("mean sum of squares due to regresson is",meanssreg)

#to find n...can use length()
n=length(y)
n
cat("total number of observations is",n)

#mean ss due to residuals
meanssres=ssres/(n-2)
meanssres
cat("mean sum of squares due to residuals is",meanssres)


#fratio
f=meanssreg/meanssres
f
cat("f-ratio is",f)
#x1 is xi-xbar
s.e.b1=sqrt(meanssres/sum(x1^2))
s.e.b1

#standard error of b0cap
s.e.b0=sqrt((meanssres/n)*(1+(n*(mean(x)^2))/sum(x1^2)))
s.e.b0
#for question (a) t=b1cap/S.E(b1cap)....testing H0:b1=0
t1=b1/s.e.b1
t1
#0.975 is 1-alpha/2
table1=qt(0.975,n-2)
cat("the calculated value is",t1,"and the tabulated value is",table1)
if(t1>table1)
  cat("Reject H0 since calculated value is greater than critical value.\n") else
    cat("Accept H0 since calculated value is lesser than critical value")

#for question (b) t=r/sqrt(1-(r^2))  * sqrt(n-2)...testing H0:rho=0
t2=(r/sqrt(1-r^2))*(sqrt(n-2))
t2
table2=qt(0.975,n-2)
cat("the calculated value is",t2,"and the tabulated value is",table2)
if(t2>table2)
  cat("Reject H0 since calculated value is greater than critical value.\n") else
    cat("Accept H0 since calculated value is lesser than critical value")


# question (c) - CONFIDENCE INTERVAL for b1
c.i=c(b1-s.e.b1*table1,b1+s.e.b1*table1)
cat("The 95% confidence interval for b1 is (",c.i,")")
