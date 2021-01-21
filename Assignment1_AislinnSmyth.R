Lab1<-read.csv(file="Lab1.csv")
summary(Lab1_true$EARN) #summary statistics for the variables.
table(Lab1$Job.class) #frequency of the variable Job.class
ftable(Lab1$EDUC, Lab1$Gender, Lab1$Job.class) # three-way-tabulation of the proportions of variables.
hist(Lab1$EARN) #a basic histogram of the variable EARN
boxplot(Lab1$EARN~Lab1$Job.class) #a basic boxplot of the variable EARN by Job Class.
Lab1$EARNx10000 = Lab1$EARN/10000 #new variable EARNx10000 that is equal to Earnings divided by 10,000.
plot(Lab1$EARNx10000, Lab1$AGE) #scatterplot with EARNx10000 on the x axis and AGE on the Y axis
