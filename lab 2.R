#1 exercise 
#Q1.consider the data on mtcars in  terms of millage per gallon,
#cylinder disp, hp, wt of car, fit a multiple linear regression model
#and test for overall fit of model using anova model

#Q2. test for the significance each of the regression coefficients

#Q3. compute the value of r2 and adj.r2

#Q4. estimate the mpg when cylinder disp,hp and wt of the car are 175,120,3.11
#respectively.



#### 2 complete theusing the inbuilt function
# 1 load the data and extract the data



########ANSWER 1############

getwd()
data("mtcars")
View(mtcars)
head(mtcars)

y = mtcars$mpg
x1 = mtcars$disp
x2 = mtcars$hp
x3 = mtcars$wt
n = length(y)
x0 = matrix(1,n,1)
x = cbind(x0,x1,x2,x3)

# Bcap regression coefficients

library(MASS)
bcap = ginv(t(x)%*%x)%*%t(x)%*%y
cat("the regression coefficients are ",bcap)
k = ncol(x)

#### fitted model ycap = x *bcap

ycap = x%*%bcap
print(ycap)


yc = bcap[1]+x1*bcap[2]+x2*bcap[3]+x3*bcap[4]

###### ANOVA TABLE FOR MULTIPLE LINEAR REGRESSION ##########

##sum of sqaures
ss_regression = sum((ycap- mean(y))^2)
ss_residual = sum((y-ycap)^2)




Source=c("Regression","Residual","Total")
df = c(k-1,n-k,n-1)
sumof_square = c(ss_regression,ss_residual,ss_regression+ss_residual)
mean_sumof_squares = c(sumof_square[1:2]/df[1:2],NA)
f_ratio = c(mean_sumof_squares[1]/mean_sumof_squares[2],NA,NA)

ANOVA_table = data.frame(Source,df,sumof_square,mean_sumof_squares,f_ratio)

### tabulted value

ftabulted = qf(0.95,k-1,n-k)
ftabulted

if(f_ratio[1]>ftabulted)
  cat("we reject Ho since calcualated greater")else
  cat(" we accept the hypothesis")


########## SECOND QUESTIOn ########

# inverse of the elements 
iv = ginv(t(x)%*%x)
# calculating the standard error
se = sqrt(diag(iv)*mean_sumof_squares[2])

# calcuating the t statistics
t = bcap/se

### table value for the t statistics
# we use the t distribution on the test t

ttab = qt(0.975,n-k) # beacuse of two tailed test



source1 = c("intercept","disp","hp","wt")
betacap = c(bcap)
SE_bacp = c(se)
tcalculated = c(t)


table_anova = data.frame(source1,betacap,SE_bacp,tcalculated)


########## q3 ######
#### R squared values
r_squred = sumof_square[1]/sumof_square[3]

cat("the r squared value is",r_squred)

Adj_r_squared = 1- (mean_sumof_squares[2]/(sumof_square[3]/df[3]))


####### Q4 ##############

#Q4. estimate the mpg when cylinder disp,hp and wt of the car are 175,120,3.11
#respectively.

Q4 = c(1,175,120,3.11)
#### ycap =x* bcap
q4_pred = sum(bcap*Q4)


######## inbuilt function for the lm
input = lm(y~x1+x2+x3)
summary(input)
coef(input)

