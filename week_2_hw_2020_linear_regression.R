# Siddhant Bhardwaj


#### BACKGROUND
# this week's assignment revolves around understanding how weather conditions influence the number of bike rides in a given day. This is real 2018 data from Chicago's DIVVY bikeshare system that I've connected to  2018 weather data.  

# you have the following relevant columns:
# number_rides - number of rides in a given day
# AWND - Average wind speed in the day
# TAVG - Average temperature in Fahrenheit
# SNOW - Total snowfall in the day
# PRCP - Total rainfall in the day

# load libraries 
library(tidyverse)

# load data
bikes <- read_csv("https://docs.google.com/spreadsheets/d/1DK8ZSmIgvZ1eVVF33NCNLyLxvYFA8t1jeGrJppaBDpI/gviz/tq?tqx=out:csv")
bikes <- data.frame(bikes)
############## EDA

# QUESTION - Explore your data with a summary.  In a few sentences describe the data, if you think they make sense, and why that is.  

# ANSWER - 
summary(bikes)
#The summary of the dataset bikes firstly shows that the day column should be converted to
#datetime object since it consists of dates and therefore should not be subject to finding min,1st quartile etc.
#The weather patterns like AWND show minimum value of 2.910,1st quartile of 6.710,3rd quartile of 11.860 and max.value
#of 21.030. PRCP shows a minimum and 1st quartile of 0.00 and a max of 2.3600, which I presume is
#largely due to very low precipitation patterns in the region.


# QUESTION - Make some histograms to understand the distribution of the columns mean_trip_time and mean_distance.  Describe a few features you see in each figure?

# ANSWER - 
hist(bikes$mean_trip_time)
#The mean trip time histogram is a right skewed distribution which shows that the mode of mean_trip_time
#is greater than the median than the mean.
hist(bikes$mean_distance)
#The mean_distance histogram is a bimodal distribution which means that it has 2 modes.
# QUESTION - Use a method to figure out the weather conditions on the day that had the most rides.  Also, what date did this occur on? 

# ANSWER - 
bikes[which(bikes$number_rides == max(bikes$number_rides)),c(6,7,8,9,10)]
bikes[which(bikes$number_rides == max(bikes$number_rides)),'day']

############## Linear regression

# QUESTION -  fit a linear regression model using TAVG as a feature to explain your target, number_rides.  Remember the format is lm(targer ~ feature, data = ...)

# ANSWER - 
info <- lm(number_rides~TAVG,data = bikes)


# QUESTION - How much variation is being explained in your target? Please use code to get the answer from the summary and store it in an object. CAll that object vs. just writing out the answer.  

# ANSWER - 
variation_info <- summary(info)
variation_info

# QUESTION - Calculate the confidence interval for B1 - Do it in a way that works if model structure or data gets added to or removed!  You can use the model coefficients from the model summary vs. calculating from scratch.

# ANSWER - 
cf <- c(variation_info$coefficients[2,1] - variation_info$coefficients[2,2],variation_info$coefficients[2,1] + variation_info$coefficients[2,2])


# QUESTION - Interpret your B1 coefficient in 'plain English.'

# ANSWER - 
#The B1 coefficient in this case refers to the coefficient of the TAVG variable and shows the
#value that is to be multipled with the variable TAVG value in order to get the predicted
#value of the number_rides variable.

# QUESTION - Calculate the predicted number of rides if the average temperature is 68 degrees

# ANSWER - 
#Since the eqn can be setup as number_rides = 251.201 * TAVG -2967.387
res <- 251.201 * 68 - 2967.387
res
# QUESTION - Make a figure showing the relationship between TAVG and number_rides. This is two continuous variables which should tell you what type of plot you need.  You can then make a vector of x-values.  Then extract the coefficients from the model to predict y.  You then have all you need to add a geom_line() to your plot!  Also, I know that ggplot can fit this line for you, but please don't do that.  The goal is to demonstrate you understand the different parts of a regression model.

# ANSWER - 
x_values <- seq(from = min(bikes$TAVG), to = max(bikes$TAVG), length.out = nrow(bikes))
b0 <- variation_info$coefficients[1,1]
b1 <- variation_info$coefficients[2,1]
y_predictions <- b0 + b1 * x_values
plot(x_values,y_predictions)
  
############## Comparing two linear regression models

# QUESTION - Fit another regression model using AWND as a feature but leave number_rides as the target

# ANSWER - 
new_model <- lm(AWND~TAVG,data = bikes)


# QUESTION - Which is a better model, this one or the first one you fit?  Use two pieces of evidence from the model summaries to justify your answer.

# ANSWER - 
summary(new_model)
#TAVG is a better predictor than AWND of number_rides since it can explain upto 78.53% of the variation
#as opposed to AWND model which can only explain upto 26.16% of the variation. (Based on r-squared)
#Secondly, the TAVG model had a p-value of 2.2e-16 as opposed to the AWND model which had a p-value 
#of 0.001128. Thus, the TAVG model has a lower p value than the AWND model which implies that there is a 
#stronger evidence of the null hypothesis. 


############## Multiple regression

# QUESTION -  fit a multiple regression model with number of rides as the target and then AWND, PRCP, SNOW, and TAVG as features.  Remember, multiple regression models have all these features in a single model.  

# ANSWER - 
multiple_model <- lm(formula = number_rides ~ AWND + PRCP + SNOW + TAVG, data = bikes)


# QUESTION - How much extra variation did you explain by including these other features compared to just the simple linear model with only TAVG as a feature? 

# ANSWER - 
summary(multiple_model)
#The multiple model has a higher adjusted R-Squared of 0.8518 which shows that it can explai upto 
#85.17% of the variation as opposed to the TAVG model whic could only explain upto 78.53% of the variation.

# QUESTION - Were any of the additional features not important?  Use two pieces of evidence to justify your answer.


# ANSWER
#The SNOW feature appears to have been less important than others since its p-value is 0.899,
#which means that since it is above 0.05, it proves the null hypothesis. Moreover,its coefficient
#is also, in absolute value terms less than other coefficients, which means that it would
#have less effect on the final predicted value.


############# Multiple regression - interaction models

# finally, we're going to fit an interaction model
# before that, run this line of code that creates a binary snow feature so that it's just a 1 if there's any snow that day, and a zero if there's not
bikes$SNOW_B <- as.factor(ifelse(bikes$SNOW > 0, 1, 0))


# QUESTION - fit an interaction model between TAVG and SNOW_B

# ANSWER - 
interaction_1 <- lm(number_rides ~ TAVG * SNOW_B,data = bikes)
ggplot(bikes, aes(x = SNOW_B,y = number_rides)) + geom_point() + geom_line(aes(x = TAVG, y = number_rides))

# QUESTION - Interaction models are hard to interpret, so make a plot with two fit lines instead.  Remember, make a sequence of X values, then use these to estimate the Y values... one Y vector for if it snowed, the other if it didn't.  Again, do this from scratch and not using ggplot to fit the line itself. 

# ANSWER -
information <- summary(interaction_1)
x_values_1 <- seq(from = min(bikes$TAVG), to = max(bikes$TAVG), length.out = nrow(bikes))
b0 <- information$coefficients[1,1]
b1 <- information$coefficients[2,1]
b2 <- information$coefficients[3,1]
y_predictions_0 <- b0 + b1 * x_values_1 + b2 * 0
y_predictions_1 <- b0 + b1 * x_values_1 + b2 * 1
ggplot(bikes,
       aes(x = TAVG, y = number_rides)) +
  geom_point(aes(color = SNOW_B)) +
  geom_line(aes(x = x_values_1, y = y_predictions_0)) +
  geom_line(aes(x = x_values_1, y = y_predictions_1),color = 'green')



# QUESTION -  Based on the plot you created, interpret how snow and temperature interact to influence the number of rides. In other words, how does it snowing or not influence the relationship between temperature and the number of rides in a day?

# ANSWER -
#From the plot, we can see that with a rise in temperature, the number of 
#days with snow declines which shows that there is an inverse relationship
#between average temperature and snow presence. Since, as we seen from the 
#plot, the number of rides increases with higher number of snow-free days as opposed
#to days with snow.Since snow-free days are positively related with average temperature,
#we can say that an increase in average temperature which interacts with the snow presence
#factor in the form of higher frequency of snow-free days to result in higher number of 
#days.

############ ONE LAST QUESTION

# QUESTION - Make a model to determine how average temperature influences the average age of the rider on a given day.

# ANSWER - 
interaction_2 <- lm(number_rides ~ TAVG * mean_rider_age,data = bikes)


# QUESTION - Given this model, what can you say about how temperature influences the age demographics of the bikeshare users? 

# ANSWER - 

information_2 <- summary(interaction_2)
#Since the adjusted r-Squared is 0.7954,we can safely conclude that it explains upto 79.54%
#of the variation in the data. Moreover, since, the p-value is  much lower than 0.05,
#hence, it can stated that it disproves the null hypothesis. Now when we look at the TAVG:mean_rider_age
#row, the estimated stands out to be -12.47, which means that the average temperature has a negative
#effect on the mean_rider_age. Furthermore, since the p-value is 0.0076 which is less than 0.05,
#it can stated that the null hypothesis has been disproven.
