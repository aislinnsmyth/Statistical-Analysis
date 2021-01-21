head(faithful)

faithful.lm <- lm(eruptions~waiting, data=faithful) # build linear regression model on full data
print(faithful.lm)

faithful.res <- resid(faithful.lm)

plot(faithful$eruptions, faithful.res,
     ylab = "Residuals", xlab="Eruptions",
     main = "Faithful Linear Model")
abline(0, 0)    # the horizon

plot(density(faithful.res), main = "Density Plot: residuals",
     ylab = "Frequency")

par(mfrow=c(2,2))
plot(faithful.lm)

# Q3. Comment on whether or not you feel the model is appropriate...

#In my opinion the model is not appropriate, the density plots last week
#for starters there were two density plots, eruptions and waiting, both plots had a big difference in ranges
#for the graphs last and this week they have the same N value of 272, but each have different Bandwidths and thus makes it hard to compare each to each other.

# The graphs last week had a greater dip in terms of they hit a peak and dipped to a trough quite a substantial amount.
#The density plot from this week has a lot less of a drop, at about 0.6 in the y and -0.4 there is a 
#marginal dip but then going straight back up to reach its peak just after 1 in the x

#All graphs are plotting the frequency to a different variable, ironically both eruptions graphs are very close in values, their y axis are the same apart from the 0.8
#However the x axis differs in numbers with last week going up in 2s and this weeks in 0.5.

#The bandwith of the Density plot this week is almost helf the bandwidth of the eruption density plot last week.
#It is quite unclear and that is why i believe the model is not appropriate.

#The residuals vs fitted graph does not show us a straight red line which indicates a problem, 
#The Scale, Location Plot of fitted vs standardized results shows a curved red line which also indicates a problem.

#The Residuals vs Leverage Plot - there is no red dotted line on the graph which indicates that the points are all in the range.