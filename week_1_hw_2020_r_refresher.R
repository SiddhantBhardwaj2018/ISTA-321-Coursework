
# Name - Siddhant Bhardwaj
library(tidyverse)




bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")
bikes <- data.frame(bikes)
# For this assignment you're going to do some simple exploration and manipulation of the 2018 Chicago DIVVY bikeshare data.  Each row represents a single bike rental.

# QUESTION - Use your data exploration techniques to look at both a summary AND a glimpse of bikes.

# ANSWER - We 
summary(bikes) # This function can be used to obtain a summary of the data present in the bikes data.
glimpse(bikes) # This function can be used to obtain a sum of the data present in the bikes data.

# QUESTION - What are the two data types present? How many rows and columns are present? Pick two columns and describe what you find in them (e.g. summary stats, character levels, etc. )

# ANSWER
#The 2 datatypes present are character and double. The number of rows is 154,966 abd columns are 12. The column 'birthyear' is of datatype double
# -  it has a minimum value of 1900,maximum value of 2003 and a median of 1985. The second column which is distance_miles has a median of 0.995, a mean of 1.331 and a 3rd quantile of 1.647 


# QUESTION - How many unique values are present in the column bikeid?

# ANSWER
length(unique(bikes$bikeid))
#The bikeid column has 6104 unique values


# QUESTION - Use some visual exploratory data analysis by making histograms of two of the numeric columns (your choice).  NOTE: Do one in base R and one using ggplot.

# ANSWER
hist(bikes$birthyear) #This one was done is base R 
ggplot(bikes,aes(x = birthyear)) + geom_histogram(bins = 10) #This one was done in ggplot



# QUESTION - Use base R to slice the data frame several different ways. Specifically, to do ALL OF the following operations: Grab just a single row, a single column, a series of rows, a series of columns, a list of rows, and a list of columns.

# ANSWER
#Grabbing a single column
bikes[,10]
#Grabbing a single row
bikes[10,]
#Grabbing a series of rows
bikes[10:15,]
#List of rows
bikes[c(500,700,900),]
#List of columns
bikes[,c(5,10,11)]


# QUESTION - Use base R to extract JUST the columns usertype, gender, and distance_miles to a dataframe called bikes_1.  Do this again using tidyverse.

# ANSWER
#Using base r
bikes1 <- bikes[,c(8,9,12)]


# QUESTION - Do the same as above but using tidyverse.  Assign it to an object called bikes_2.

# ANSWER
bikes_2 <- bikes %>% select(c(8,9,12))



# QUESTION - Use base R to create a dataframe of just subscribers.  Call the resulting dataframe bikes_subs.

# ANSWER

bikes_subs <- bikes[bikes$usertype == 'Subscriber',]


# Question - Now use tidyverse to make a dataframe of just subscribers.  Name it whatever you want.

# ANSWER
bikes_subs_tidyverse <- bikes %>% filter(usertype == 'Subscriber')



# QUESTION - Use tidyverse to create a dataframe of just Customers who are also Male  Name this data frame bikes_male_cust

# ANSWER
bikes_male_cust <- bikes %>% filter(usertype == 'Customer' & gender == 'male')



# QUESTION - What's the average distance ridden by male customers?

# ANSWER
mean(bikes$distance_miles,na.rm=TRUE)



# QUESTION - Birthyear isn't super useful right now.  Having actual rider age would be better.  Use either base R or tidyverse to create a new column that calculates the rider's age (these data were collected in 2018, btw).  After it's made explore your new column (using whatever method you'd like) to make sure it makes sense.

# ANSWER
bikes$current_age <- 2020 - bikes$birthyear



# QUESTION - I'm guessing you see some strange values in your newly created age column.  Why don't you create a new dataframe called bikes_realages where it only contains riders who ages are less than or equal to some age that seems realistic to you.

# ANSWER
bikes_realages <- bikes[bikes$current_age < 30,]



# QUESTION - Make a histogram of rider ages using ggplot.  Based on this, what age range used the bikeshare the most?

# ANSWER
ggplot(bikes,aes(x = current_age)) + geom_histogram(bins = 10) #This one was done in ggplot




# QUESTION - Some of these data types could or need to be changed.  There are three variables that are currently numeric but should be a factor.  What are they and why?

# ANSWER
#The 3 variables are bikeid,from_station_id and to_station_id. Because these are ids, this means
#that they should not be fractions or decimals. Thus, if it remains numeric, then it may be amenable
#for calculating statistical properties which it is not meant to do. Thus, its datatype should be factor instead of numeric.



# QUESTION - Now change those three variables in the bikes dataframe to factors

# ANSWER
bikes$bikeid <- as.factor(bikes$bikeid)
bikes$from_station_id <- as.factor(bikes$from_station_id)
bikes$to_station_id <- as.factor(bikes$to_station_id)

# QUESTION - Use some method to figure out what was the most frequently used bike

# ANSWER
bikes[which.max(bikes$distance_miles),]



# QUESTION - How many miles in total was the most frequently used bike ridden.  You're going to need to filter and then do a sum on a column.

# ANSWER
bikes[which.max(bikes$distance_miles),12]



# QUESTION - What was the least used bike?  How many miles was it ridden?

# ANSWER
bikes[which.min(bikes$distance_miles),12]




# QUESTION - How many rides in our data set were returned to the same station they were checked out from?  Remember that you can do a logical comparison between values using ==.  You can then sum your true/false values!

# ANSWER
length(bikes$from_station_name == bikes$to_station_name)

# QUESTION - Use base R to select just the column distance_miles from bikes and assign it to an object called target_1

# ANSWER
target_1 <- bikes$distance_miles




# QUESTION - Now do the same but use tidyverse's select() function.  Assign to target_2

# ANSWER
target_2 <- bikes %>% select(distance_miles) 




# QUESTION - Now get the mean of target_1 and target_2 using mean().  Don't forget you might need to specify na.rm = TRUE as an argument in mean().

# ANSWER
mean(target_1,na.rm = TRUE)

mean(target_2,na.rm = TRUE)



# QUESTION - Did you get a return for both?  Why or why not?  What happens when you run is.data.frame() on target_1 and then target_2?  What about is.vector()?  What is this telling you about the way in which both methods extracted the column?

# ANSWER

#No, I did not get a return for both target_1 and target_2 I got a return for target_1 since it
# was a vector type but did not for target_2 since it was a dataframe type.

data.frame(target_1)
data.frame(target_2)
#When I run data.frame on target_1 and target_2, I am able to get dataframes of target_1 and target_2.

vector(target_1)
vector(target_2)

#When I run vector(target_1), I get 'invalid mode argument' error.
#When I run vector(target_2), I get an error saying it cannot make a vector of mode c(0.8034770562, 0.8039925157, 0.5159299021, 0.3611667516, NA,...)
#This shows that I can only convert to vectors only if I have a numeric datatype which means that 
#since, the target_1 and target_2 include N.A., they cannot be converted to vectors.