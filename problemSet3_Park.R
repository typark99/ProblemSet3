###############
#Problem Set 3#
#Taeyong Park # 
#Feb 13, 2014 #
###############

## Set my directory.
rm(list=ls())
setwd("C:/Users/Taeyong/Documents/GitHub/ProblemSet3/")


########## I. SAMPLING DISTRIBUTIONS AND P-VALUES ##########


### 1. Make a three dimensional array with dim=c(20,5, 1000) and fill it with random data ###

## Make an array that contains 20*5*1000 random values following the standard normal distribution.
set.seed(0520) #Set a seed to produce consistent random values.
myArray <- array(rnorm(100000), dim=c(20, 5, 1000)) #1000 datasets; each dataset has a matrix of 20 rows and 5 columns


### 2. Make a function to create Y values ###

Beta <- matrix(c(1,2,0,4,0), ncol=1) #The vector of covariates

## Create a function that will be used in the apply function.
myFunction.q2 <- function (x) { 
  x %*% Beta + rnorm(20) #Make error terms that follow the standard normal distribution; It should be 20 by 1 vector to be conformable to x%*%Beta 
}

y <- apply(X=myArray, MARGIN=3, FUN=myFunction.q2) #Since our data is an array, the apply function may be used. MARGIN=3 makes linearFunction apply to the 1000 datasets of 20*5 matrix.
is.array(y) #This returns TRUE.
dim(y) #This returns 20 1000


### 3. Run 1,000 regressions across all of this simulated data ###

## 6 coefficnets should include one intercept and five slope coefficients. And, we want to run 1000 regressions.
## Hence, each regression should be a regression of one column of y on 20*5 matrix of myArray.
myFunction.q3 <- function (i) {
  coef(lm(y[,i] ~ myArray[,,i]))
}
myFunction.q3(1) # For each i, the function returns 6 numeric values.

## We want to make the final output as a 1000*6 matrix. Hence, we may use the sapply function that will return a matrix, given an output of all vectors having the same length.
coefficient <- t(sapply(1:1000, FUN=myFunction.q3)) #Since myFunction.q3 is a function of i, the data should be 1:1000; We should transpose the output to make a 1000*6 matrix.
dim(coefficient) #Dimension is 1000 by 6.
coefficient[1,] #Each row returns one set of regression coefficients.


### 4. Create a density plot for each of the 6 coefficients ###

## For each i, output[,i] returns 1000 simulated coefficients.
## We want to create a density plot for each coefficient from the 1000 simulated values.

## Since no for loops are allowed, let's create the 6 densities one by one.
plot(density(coefficient[,1]), main="Density plot for intercept",
     ylab="Kernel density (Gaussian)")
plot(density(coefficient[,2]), main="Density plot for coefficient1",
     ylab="Kernel density (Gaussian)")
plot(density(coefficient[,3]), main="Density plot for coefficient2",
     ylab="Kernel density (Gaussian)")
plot(density(coefficient[,4]), main="Density plot for coefficient3",
     ylab="Kernel density (Gaussian)")
plot(density(coefficient[,5]), main="Density plot for coefficient4",
     ylab="Kernel density (Gaussian)")
plot(density(coefficient[,6]), main="Density plot for coefficient5",
     ylab="Kernel density (Gaussian)")

## We find that every distribution has the form of normal distribution.
## Each distribution represents the sampling distribution, 
## in that each represents the probability density of the estimate derived from the random samples. 


### 5. Alter the code to obtain t-statistics for all 1,000 regressions ###

## Alter the code for myFunction.q3 to obtain t-statistics.
myFunction.q5 <- function (i) {
  summary(lm(y[,i] ~ myArray[,,i]))$coef[,3] # Use the summary function to obtain the t-statistics
} 

## Just as we did for question 3, use the sapply function that will return a matrix, given an output of all vectors having the same length.
tStat <- t(sapply(1:1000, FUN=myFunction.q5)) #Since myFunction.q5 is a function of i, the data should be 1:1000; We should transpose the output to make a 1000*6 matrix.
dim(tStat) #Dimension is 1000 by 6.
tStat[1,] #Each row returns one set of t-statistics for the six coefficients.


### 6. Calculate how many t-statistics are statistically ¡°significant¡± ###

## Degrees of freedom = The number of samples - the number of parameters (including the intercept) = 14.
criticalVal <- qt(c(0.025, 0.975), 14) # This returns the critical values for the 0.05 significance level given the t distribution with 14 degrees of freedom.

## t-statistics for each variable
myFunction.q6 <- function (i) {
  abs(tStat[,i]) - criticalVal[2] # We want to compare the absolute value of a t-statistic and the upper bound critical value  
} 
## If the the absolute value of a t-statistic is greater than or equal to the upper bound of the critical value, 
## then it means the t-statistic is statistically significant at the 0.05 significance level.
length(which(myFunction.q6(1)>=0)) #Intercept: 53 t-statistics are significant 
length(which(myFunction.q6(2)>=0)) #Coefficient 1: 913 t-statistics are significant
length(which(myFunction.q6(3)>=0)) #Coefficient 2: 999 t-statistics are significant
length(which(myFunction.q6(4)>=0)) #Coefficient 3: 47 t-statistics are significant
length(which(myFunction.q6(5)>=0)) #Coefficient 4: 1000 t-statistics are significant
length(which(myFunction.q6(6)>=0)) #Coefficient 5: 59 t-statistics are significant

