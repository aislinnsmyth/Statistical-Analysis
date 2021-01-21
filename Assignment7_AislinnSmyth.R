head(faithful)

cor(faithful$eruptions, faithful$waiting) # checking correlation between eruptions and waiting

par(mfrow=c(1, 2))  # divide graph area in 2 columns
scatter.smooth(x=faithful$eruptions, y=faithful$waiting, main= "Eruption ~ Waiting") #smooth scatterplot


boxplot(faithful$eruptions, main="Eruptions", sub=paste("Outlier rows: ", boxplot.stats(faithful$eruptions)$out))  # box plot for 'eruptions'
boxplot(faithful$waiting, main="Waiting", sub=paste("Outlier rows: ", boxplot.stats(faithful$waiting)$out))  # box plot for 'waiting'

par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(faithful$eruptions), main="Density Plot: Eruptions", ylab="Frequency")  # density plot for 'eruption'
plot(density(faithful$waiting), main="Density Plot: Waiting", ylab="Frequency") # density plot for 'waiting'

faithful.lm <- lm(eruptions ~ waiting, data=faithful)  # build linear regression model on full data
print(faithful.lm)

summary(faithful.lm) #inspecting results
plot(faithful$eruptions, faithful$waiting) #inspecting regression with a scatterplot of values
abline(faithful.lm)

#(7)the equation that describes the linear model I hae fitted is
# Y = a + bX, where Y is the Waiting variable(dependent variable), X is the Eruption variable(independent variable)
#b is the slope of the line, a is the y-intercept.

#(8) As the p-value is much less than 0.05 we reject the null hypothesis that ?? = 0, 
#Hence, there is a significant relationship between between the variables waiting and eruption of the data set faithful.

#(9) The p-value od waiting variable between waiting and F-value are similar because the regression model fits 
#the data better than the model with no independent variables as waiting is the dependant variable.

