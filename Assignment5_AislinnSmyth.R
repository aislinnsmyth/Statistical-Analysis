p_val <- 0 

TwoSampTest <-function(type=NULL, tails=NULL, meanProportion=NULL, alpha, n1, n2, x_bar1, x_bar2, StandDev1, sd2, Pop_D)
{
  
  if (meanProportion=="mean") {
    
    #test statistic calculation
    
    if (StandDev1 == sd2) {
      se = sqrt(((StandDev1*StandDev1)/n1)+((sd2*sd2)/n2))
    } else {
      se = (((StandDev1*StandDev1) / n1) + ((sd2*sd2)/n2))   / (((StandDev1*StandDev1*StandDev1*StandDev1)/((n1*n1)*(n1-1))) + ((sd2*sd2*sd2*sd2)/((n2*n2)*(n2-1))))
    }
  } else if (meanProportion=="prop") {
    se = sqrt(((x_bar1*(1-x_bar1)) / n1)+( (x_bar2*(1-x_bar2)) / n2))
  } else {stop("please choose the mean proportion as proportion or mean")}
  
  test_stat <- (x_bar1-x_bar2 - Pop_D)/se*(x_bar1 - x_bar2)
  
  #the z-type
  if (type=="z") { 
    
    #calculation the p-value from the test statistic
    
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as either two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n1+n2-2  
    #calculate p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as either two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("Two Sample", type, "test.", tails, "tailed"), n1=n1, n2=n2, Pop_D=Pop_D, diff=x_bar1-x_bar2, se_est=se, test_stat=test_stat, p = p_val, alpha = alpha, significance = sig )
  #ret the list
  return( ret )
}



# Simulate the two vectors

population1 <- rnorm(100, 4, 5)
population2 <- rnorm(80, 3.5, 2)

# get sd, mean, and n from both vectors

StandDev1 <- sd(population1)
length1 <- length(population1)
mean1 <- mean(population1)

StandDev2 <- sd(population2)
length2 <- length(population2)
mean2 <- mean(population2)

# 1)	H0: the difference in means of the populations are equal to 1. 
# HA: the difference in means of the populations are not equal to 1. 
# Use a Z-test with the true population SDs. Use alpha = 0.05

TwoSampTest("z", "two", "mean", 0.05, length1, length2, mean1, mean2, StandDev1, StandDev2, 1)


# $Pop_D
# [1] 1
# 
# $diff
# [1] 0.6573936
# 
# $se_est
# [1] 511.3074
# 
# $test_stat
# [1] -0.0004404929
# 
# $p
# [1] 0.9996485
# 
# $alpha
# [1] 0.05
# 
# $significance
# [1] "not significant"

# fail to reject the H0


# 2)	H0: the means of the populations are equal. 
# HA: the means of the populations are not equal. 
# Use a Z-test with the sample SDs. Use alpha = 0.05

TwoSampTest("z", "two", "mean", 0.05, length1, length2, mean1, mean2, 5, 2, 0)

# ret FOR Q2

# $type
# [1] "Two Sample z test. two tailed"
# 
# $n1
# [1] 100
# 
# $n2
# [1] 80
# 
# $Pop_D
# [1] 0
# 
# $diff
# [1] 0.6573936
# 
# $se_est
# [1] 452.5169
# 
# $test_stat
# [1] 0.000955028
# 
# $p
# [1] 0.999238
# 
# $alpha
# [1] 0.05
# 
# $significance
# [1] "not significant"

# fail to reject H0

# 3)	H0: the means of the populations are equal. 
# HA: the means of the populations are not equal. 
# Use a t-test. Use alpha = 0.05.

TwoSampTest("t", "two", "mean", 0.05, length1, length2, mean1, mean2, StandDev1, StandDev2, 0)

# $Pop_D
# [1] 0
# 
# $diff
# [1] 0.6573936
# 
# $se_est
# [1] 511.3074
# 
# $test_stat
# [1] 0.0008452181
# 
# $p
# [1] 0.4996633
# 
# $alpha
# [1] 0.05
# 
# $significance
# [1] "not significant"

# Fail to reject the H0

install.packages('Rlab')
library(Rlab)

# 4)	Repeat the previous task using the t.test function in the package 
# {stats}

t.test(population1, population2, var.equal=TRUE)

# Two Sample t-test
# data:  population1 and population2
# t = 1.1654, df = 178, p-value = 0.2454
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4557817  1.7705688
# sample estimates:
#   mean of x mean of y 
# 3.833502  3.176109 

# fail to reject the H0


# 5)	Repeat the previous task using the t.test function in the package 
# {stats}, without the assumption of equal variance.

t.test(population1, population2, var.equal=FALSE)


# Welch Two Sample t-test
# 
# data:  population1 and population2
# t = 1.2602, df = 139.13, p-value = 0.2097
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3739741  1.6887612
# sample estimates:
#   mean of x mean of y 
# 3.833502  3.176109 

# fail to reject the H0


# Vector with 100 bernoulli trials, probability 0.3

bernoulliVector <- rbern(100, 0.3)


# Vector with 85 bernoulli trials, probability 0.7

secondBernoulliVector <- rbern(85, 0.7)

StandDev1 <- sd(bernoulliVector)
length1 <- length(bernoulliVector)
mean1 <- 0.3

StandDev2 <- sd(secondBernoulliVector)
length2 <- length(secondBernoulliVector)
mean2 <- 0.7

# 6)	H0: the population proportions are equal. 
# HA: the population proportions are not equal 

TwoSampTest("z", "two", "prop", 0.05, length1, length2, mean1, mean2, StandDev1, StandDev2, 0)

# ret FOR Q6
# 
# $type
# [1] "Two Sample z test. two tailed"
# 
# $n1
# [1] 100
# 
# $n2
# [1] 85
# 
# $Pop_D
# [1] 0
# 
# $diff
# [1] -0.4
# 
# $se_est
# [1] 0.06760613
# 
# $test_stat
# [1] 2.366649
# 
# $p
# [1] 0.01794993
# 
# $alpha
# [1] 0.05
# 
# $significance
# [1] "significant"


# accept the HA

# 7)	write code to calculate the 95% Confidence Interval for your 
# estimate of the difference between the two population proportions.


se = sqrt(((mean1*(1-mean1)) / length1)+( (mean2*(1-mean2)) / length2))
sideOne <- (mean1 - mean2) + se*(mean1 - mean2)
sideTwo <- (mean1 - mean2) - se*(mean1 - mean2)
paste("95 percent confidence interval with difference 0 :")
paste(sideOne)
paste(sideTwo)

# "95 percent confidence interval with difference 0 :
# "-0.427042450289259"
# "-0.372957549710741"
# proportion difference lies between these two values

