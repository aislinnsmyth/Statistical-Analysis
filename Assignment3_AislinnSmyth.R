#A population of 10,000 with a variable from a Normal(4,5) distribution.
populationNorm <- rnorm(10000,4,5)


#A population of 10,000 w/ a variable from an Exponential(1) distribution.
expPopulation <- rexp(populationNorm,1) 

#record the size, mean and sd of 'population'.
n_populationNorm <- length(populationNorm)
mean_populationNorm <- mean(populationNorm)
sd_populationNorm <- sd(populationNorm)

#record the size,mean and sd of 'populationDistribution'.
n_expPopulation <- length(expPopulation)
mean_expPopulation <- mean(expPopulation)
sd_expPopulation <- sd(expPopulation)

#normality of populationNorm plot
qqnorm(populationNorm)
qqline(populationNorm)
hist(populationNorm, freq = FALSE)
xfit <- seq(min(populationNorm), max(populationNorm), length = 40) 
yfit <- dnorm(xfit, mean = mean(populationNorm), sd = sd(populationNorm))
lines(xfit, yfit)

#normality of expPopulation plot
qqnorm(expPopulation)
qqline(expPopulation)
hist(expPopulation, freq = FALSE)
xfit <- seq(min(expPopulation), max(expPopulation), length = 40) 
yfit <- dnorm(xfit, mean = mean(expPopulation), sd = sd(expPopulation))
lines(xfit, yfit)

#Take a random sample of size 10, 50, 500 for populationNorm
n10 <- sample(populationNorm, 10)
n50 <- sample(populationNorm, 50)
n500 <- sample(populationNorm, 500)

#Take a random sample of size 10, 50, 500 f0r expPopulation.
e10 <- sample(expPopulation, 10)
e50 <- sample(expPopulation, 50)
e500 <- sample(expPopulation, 500)

#plots for sample 10 normPopulation
qqnorm(n10)
qqline(n10)
hist(n10, freq = FALSE)
xfit <- seq(min(n10), max(n10), length = 40) 
yfit <- dnorm(xfit, mean = mean(n10), sd = sd(n10))
lines(xfit, yfit)

#plots for sample 50 normPopulation
qqnorm(n50)
qqline(n50)
hist(n50, freq = FALSE)
xfit <- seq(min(n50), max(n50), length = 40) 
yfit <- dnorm(xfit, mean = mean(n50), sd = sd(n50))
lines(xfit, yfit)

#plots for sample 500 normPopulation
qqnorm(n500)
qqline(n500)
hist(n500, freq = FALSE)
xfit <- seq(min(n500), max(n500), length = 40) 
yfit <- dnorm(xfit, mean = mean(n500), sd = sd(n500))
lines(xfit, yfit)

#plots for sample 10 expPopulation
qqnorm(e10)
qqline(e10)
hist(e10, freq = FALSE)
xfit <- seq(min(e10), max(e10), length = 40) 
yfit <- dnorm(xfit, mean = mean(e10), sd = sd(e10))
lines(xfit, yfit)

#plots for sample 50 expPopulation
qqnorm(e50)
qqline(e50)
hist(e10, freq = FALSE)
xfit <- seq(min(e50), max(e50), length = 40) 
yfit <- dnorm(xfit, mean = mean(e50), sd = sd(e50))
lines(xfit, yfit)

#plots for sample 500 expPopulation
qqnorm(e500)
qqline(e500)
hist(e500, freq = FALSE)
xfit <- seq(min(e500), max(e500), length = 40) 
yfit <- dnorm(xfit, mean = mean(e500), sd = sd(e500))
lines(xfit, yfit)

#mean of sample normal population 50
mean_populationN50 <- mean(n50)

#mean of sample exp population 50
mean_populatione50 <- mean(e50)

#record the sd of x0
sd_populationN50 <- sd(n50)

#record the sd of x0
sd_populatione50 <- sd(e50)

#confidence interval for 95% confidence interval
Z1 <- qnorm(0.975)

# calculating z_score for 95% confidence interval
Z_score <- qnorm(0.975)

#calculate the standard error of mean1 using known and estimated sd0
se_kn <- sd_populationN50/sqrt(mean_populationN50)

#calculate the standard error of mean1 using known and estimated sd0
se_kn1 <- sd_populatione50/sqrt(mean_populatione50)

#calculating z-scores for known population
left_Z95_kn <- mean_populationN50-Z_score*se_kn
right_z95_kn <- mean_populationN50+Z_score*se_kn

#result of z-score for populationNorm
paste("Z-distribution w/known pop sd:", left_Z95_kn, right_z95_kn)

#calculating z_score for known population expPopulation
left_Z95_kn <- mean_populatione50-Z_score*se_kn
right_z95_kn <- mean_populatione50+Z_score*se_kn

#result of z-score for expPopulation
paste("Z-distribution w/known pop sd:", left_Z95_kn, right_z95_kn)

#length of sample  of distribution 50 populationNorm
n_populationN50 <- length(n50)

#length of sample of distribution 50 expPopulation
n_populatione50 <- length(e50)

#t-distribution for 95% confidence interval
t1 <- qt(0.975, 10)

#Calculate 95% critical value for populationNorm
t_score <- qt(0.975,n_populationN50 -1)

#Calculating 95% confidence intervals
left_t95_kn <- mean_populationN50-t_score*se_kn
right_t95_kn <- mean_populationN50+t_score*se_kn

#Print values for populationNorm
paste("t-distribution w/known pop sd:", left_t95_kn, right_t95_kn)

#Calculate 95% critical value for expPopulation
t_score <- qt(0.975,n_populatione50 -1)

#Calculating 95% confidence intervals
left_t95_kn <- mean_populatione50-t_score*se_kn1
right_t95_kn <- mean_populatione50+t_score*se_kn1

#Print values for expPopulation
paste("t-distribution w/known pop sd:", left_t95_kn, right_t95_kn)

