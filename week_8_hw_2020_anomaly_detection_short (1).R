
#Name - Siddhant Bhardwaj

# HW 9 - Anomaly Detection

library(tidyverse)
library(anomalize)

# For this homework you're going to try and find anomalies on some time-series data of all the taxi rides taken in New York City in 2018.  


###########################################################
# Taxi ride anomalies 

# Now we're going to look at anomalies in the NYC taxi ride data.  NYC publishes the data for EVERY SINGLE TAXI RIDE.  It's a super rich dataset that you can do all sorts of fun stuff with.  For this dataset I pulled down all rides taken in 2018 (it was about 12gb of data) and then took daily summaries of how many rides.  The goal is to decompose the data as it has time series trends and then identify the outliers.  

# Our data

rides <- read_csv("https://docs.google.com/spreadsheets/d/1ZrjIK6H-0NNSWvQF82pfEhzXnh2NjmyaEmI6wnGTzDM/gviz/tq?tqx=out:csv")

# Explore and then make a plot of the number of rides over time

summary(rides)

glimpse(rides)

ggplot(rides, aes(x = day, y = total_pass)) + geom_point()

# Do any datapoints jump out at you as being potential anomalies? 

#The datapoints around 2e + 05 at Jan 2018 and Jan 2019 stand out as stark 
#outliers.


# First, decompose the data as was done in the RPubs lesson. 
#Given this is daily data what do you think the frequency of the cycle 
#should be?  Generally you want it to follow the more fine-scale pattern, 
#while the trend should follow a longer one or more seasonal patterns.  
#You can specify the length of them with a number and then units 
#(e.g. '7 days', '1 month').  Stick with method = 'stl' 

rides_decomp <- rides %>% time_decompose(total_pass, method = 'stl', 
frequency = '7 days', trend = '1 months')

# Now make another plot with the remainder over time.  

ggplot(rides_decomp,
       aes(x = day, y = remainder)) +
  geom_point() + geom_line()

# Great, so you should see that the weekly and yearly patterns are gone.  
#Now go and use the anomalize function to calculate which of these remainder 
#values are anomalies. You can use the method 'gesd' which is sort of like a
#rolling t-test.  Keep alpha at 0.05.   

rides_anom <- rides_decomp %>%
  anomalize(remainder, method = 'gesd', alpha = 0.05, max_anoms = 0.2)

# You should now have a new column called anomaly.  
#Go and make the same plot of remainder over time but this time color 
#the points by if they're anomalies or not. FYI, you'll need to specify the 
#color = anomaly in your geom_point call (play with it in the main aes() call 
#to see why!).

ggplot(rides_anom,
       aes(x = day, y = remainder)) +
  geom_line(color = 'blue', alpha = 0.2) +
  geom_line(aes(x = day, y = remainder_l1), color = 'red') +
  geom_line(aes(x = day, y = remainder_l2), color = 'red') +
  geom_point(aes(x = day, y = remainder, color = anomaly))

# OK, so now we can graphically see which points are classified as anomalies. 
#Make a dataframe of just the anomalous points using a filter (base or tidyverse). Call it anomalies. 

rides_anom %>% filter(anomaly == 'Yes')


# Use some outside knowledge or googling to figure out what drove the 
#anomalies in NYC in 2018.  For example, if you search for weather
#January 4th 2018 you get a ton of new reports about the MAJOR blizzard that 
#hit that day.  Please explore at least two sets of anomalous points.

#The January 2018 North American Blizzard tha had resulted in deaths of atleast
#22 people.

#The anomaly in April 2018 was due to a major late-season blizzard and snowstorm
#due to thunderstorm, windstorm from April 13 - 16 in Minnesota.

#The anomaly in January 2019 was due to another major snowstorm
#throughout U.S. I personally experienced this in my freshman year which was my first
#year in the U.S. and this was the first time I'd seen snow.Was a lot of fun.