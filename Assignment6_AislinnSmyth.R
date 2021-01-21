survey <-read.csv(file="survey.csv") # importing and reading data 
tbl <- table(survey$Smoke, survey$Exer) # reading data from the smoke category and exer category
chisq.test(tbl) # testing the chi squared


# Q1. How were the degrees of freedom for this test calculations.
#(rows - 1) * (cols - 1) = degrees of freedom.
#(4-1) * (3-1) = 6.


#Q2. Assumptions of the chi squared test?
#HYPOTHESIS TESTING NOTES.
#Two-Sample test ~ Independent groups ~ One tailed.
#Every observations must contribute to only one cell.
#All expected values must be >5 in a 2x2 table.
#In a larger table, n must be >20.
#All expected values must be >1 and no more than 20% of the expected values can be <5.
#Chi-square test is highly sensitive to sample size.
#Data in the cells should be frequencies, for counts of cases and not percentages.
#Categories are mutually exclusive 2 variables both measured as categories.


#(3) the code gave an error message because many of the expected values are very small, 
#the approximation may be poor, the test statistic relies on roughly normal distribution.


#(4) the null hypothesis is that no relationship exists on the categorical variables of exercise and smoking in the population,
#they are independent
#the alternative hypothesis is that a relationship exists on the categorical variables of exercise and smoking in the population,
#they are not independent.


#(5) Using signifcance level of 0.05,
#12.59 from  table as degrees of freedom = 6, 5.4885 calculated x^2.
#Because 5.4885 <= 12.59, we fail to reject the H0/ null hypothesis.
#The conclusion being that no relationship exists on the categorical variables of people who exercise and people who smoke
