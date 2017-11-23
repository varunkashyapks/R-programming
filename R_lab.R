getwd()
data("stackloss")
View(stackloss)
head(stackloss)

y = stackloss$stack.loss
x1 = stackloss$Air.Flow
x2 = stackloss$Water.Temp
x3 = stackloss$Acid.Conc.

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
yc
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

ttab = qt(0.975,n-k) 
source1 = c("intercept","Air.Flow","Water.Temp","Acid.Conc")
betacap = c(bcap)
SE_bacp = c(se)
tcalculated = c(t)


table_anova = data.frame(source1,betacap,SE_bacp,tcalculated)


