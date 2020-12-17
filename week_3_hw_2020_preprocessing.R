# Name - Siddhant Bhardwaj

##### Background
#The goal of this assignment is to apply all your preprocessing skills to two new sets of data.  You'll need to explore the datasets, identify the issues, and then fix them. You'll also have to select out the columns and rows you need, drop those you don't.  Finally, there's some imputation, one hot encoding, and creating of dummy variables.  


# You'll need both tidyverse AND the caret package.  Load them up!
library(tidyverse)
library(caret)

###### Dataset # 1 - LA parking tickets
# This dataset is a slimmed down version of the 5 million or so parking tickets given in LA in 2018!  There a bunch of real-life errors within that happen when you have thousands of people writing tickets.  

# bring in the data
parking <- read_csv("https://docs.google.com/spreadsheets/d/11ahddH6snm10AuxF51MlOISx2yCsJ2WmPKFdfRFpInU/gviz/tq?tqx=out:csv")
parking <- data.frame(parking)

# QUESTION - Explore the dataset at the whole dataset level.  What columns might not be useful based off your exploration?

# ANSWER - 
glimpse(parking)
summary(parking)
#There are 9 columns and 50,000 rows in the entire dataset.Based off the exploration,
#we can conclude that color column is not useful since the color of a car
#should not have any bearing on the behavior of a car or the occurrence of the accident.
#The issue_time may also not be useful since the issue_time of a car has no bearing
#on the violaton committed or the fine charged.

# QUESTION - Explore the individual columns within the dataset.  Specifically what levels are present within make, body style, and color?  Any issues that need to be fixed?  You might need to do some googling to figure out what some of these codes mean. 

# ANSWER - 
summary(factor(parking$make))
#There are 9 levels present in this make variable - BMW, CHEV,FORD,HOND,MERC,NISS,MERZ,TOYO,TOYT
#Now, the car code for mercedes is MERC which is mislabelled as MERZ. Therefore, both MERC and MERZ both refer to the 
#same car make of Mercedes. Also, TOYO and TOYT both refer to the make TOYOTA because TOYO is a misspelling of TOYT.


summary(factor(parking$body_style))
#There are 19 levels present in this body_style variable - BU, CM, GY, LM, MC, MH, MS, OT, PA, PP, PU, SU, TK, TR, TT, VA, VN, VV, NA
summary(factor(parking$color))
#There are 10 levels present in this color variable - BK, BL, GN, GR, GY, SI, SL, WH, WT
#The levels GR, and GY are one and the same as both of them refer to Grey. Also, WH and WT refers to
#white colors and also SI and SL refers to silver colors. 


# QUESTION - Explore the agency, plate and violation_code columns. Are they useful?  Should you drop them?  If so, do it.

# ANSWER 
summary(factor(parking$agency))
summary(factor(parking$plate))
summary(factor(parking$violation_code))
#Yes, all the three columns are useful. The agency column contains information
#since it contains information about the car agency from which they are purchased.
#It would be useful to explore this dataset in relation to frequency of accidents and their types
#since it would be important to understand if a particular dealership has a specific 
#relationship with this issue.
#The plate column is also important since it allows us understand what state is most
#associated with violations and would help us to understand what state iis associated and to what extent with such issues.
#Violation code column is especially important since it would help us know what kind of issue has ocurred.
#Hence, no column can be dropped.


# QUESTION - based on your earlier exploration there are too many body styles.  Enter summary(factor(parking$body_style)) into your column to get a list of how many observations there are for each style.  Below filter your dataset so it contains only the top for most common body styles

# ANSWER - 
summary(factor(parking$body_style))
#There are 19 body styles - BU,CM,GY.LM,MC,MH,MS,OT,PA,PP,PU,SU,TK,TR,TT,VA,VN,VV,NA's
top_props <- data.frame(parking %>% group_by(body_style) %>% summarize(total_obs = n()))
top_props <- data.frame(parking %>%  group_by(body_style) %>%  summarize(total_obs = n()) %>% top_n(5))
top_props
parking <- parking %>% filter(body_style %in% top_props$body_style)


# QUESTION - When you looked at the unique values within the make column I hope you saw that there were two labels for two of the car brands (Toyota and Mercedes).  Use the summary(factor()) method like you did above to figure out which of the two in each brand is the wrong label.  Then use ifelse to correct it in this data frame.

# ANSWER - 

summary(factor(parking$make))
#TOYT is the correct label for Toyota and not TOYO and ifelse has been used to change TOYO to TOYT
parking$make <- ifelse(parking$make == 'TOYO','TOYT',parking$make)

parking$make <- ifelse(parking$make == 'MERZ','MERC',parking$make)
#MERC is the correct label for Mercedes and not MERZ and ifelse has been used to replace MERZ with MERC 



# QUESTION - Colors have some similar errors in labels such as there being both GR and GY for grey.  Do what you did above and correct the three errors in color. 

# ANSWER - 

summary(factor(parking$color))

parking$color <- ifelse(parking$color == 'GR','GY',parking$color)
#GY is the correct label for Grey and not GR and ifelse has been used to replace GR with GY 

parking$color <- ifelse(parking$color == 'SL','SI',parking$color)
#SI is the correct label for Silver and not SL and ifelse has been used to replace SL with SI 

parking$color <- ifelse(parking$color == 'WT','WH',parking$color)
#WH is the correct label for White and not WT and ifelse has been used to replace WT with WH 


# QUESTION - Our fine column has several issues.  First, there is the $ sign in the number which prevents us from using it as a numeric column.  Remove the $ from all numbers. After removal convert the column to numeric. Next, there are NA values in the data frame.  Use whatever method you like to verify that there are NA values.  Then use an ifelse statement to median impute these missing values. 

# ANSWER - 
summary(factor(parking$fine))
parking$fine <- parking$fine %>% str_remove('[$]') %>%  as.numeric()
length(is.na(parking$fine))
#There are 49761 NA values in the column
parking$fine <- ifelse(is.na(parking$fine), median(parking$fine,nna.rm = TRUE),parking$fine)

# QUESTION - The various levels in the violation column are a mess.  Replace all spaces with an underscore '_'.  Replace all forward slashes with a '-'.  Remove all periods.

# ANSWER - 
summary(factor(parking$violation))
parking$violation <- parking$violation %>% str_replace_all(' ','_') %>% str_replace_all('/','-') %>% str_remove_all('\\.')





###########################################################

# Now for part two of our assignment - preprocessing our insurance data.

# In this dataset our ultimate goal is to predict insurance costs based on the other features.  Thus the target is the charges column.

# bring in data
costs <- read_csv("https://docs.google.com/spreadsheets/d/1WUD22BH836zFaNp7RM5YlNVgSLzo6E_t-sznxzjVf9E/gviz/tq?tqx=out:csv")

# QUESTION - Explore the whole dataset quickly

# ANSWER - 
summary(costs)
glimpse(costs)


# QUESTION - Remember from our earlier lesson that bodyfat is highly collinear with BMI.  Drop bodyfat from out dataframe.

# ANSWER - 
costs <- costs %>% select(-bodyfat) 
glimpse(costs)


# QUESTION - How many levels are present in the region column?  After exploring that, use the dummyVars function to one hot encode everything.  Remember our target is charges and will be dropped after you create the dummy variables, so you'll have to remember to join that back on.  If you're struggling look back at the tutorial!

# ANSWER - 
length(summary(factor(costs$region)))
#There are 4 levels present in region column
dummies <- dummyVars(charges ~ ., data = costs, fullRank = TRUE)
dummies_pred <- predict(dummies, newdata = costs)
dummies_pred
costs <- cbind(costs %>% select(charges), dummies_pred)
glimpse(costs)

# QUESTION - Maybe all that matters is if the individual has kids or not, and not how many kids they have.  Make a binary feature for children and call it children_b in your dataframe.  Drop the original children column afterwards.

# ANSWER-
costs$children_b <- ifelse(costs$children > 0, 1,0)
costs$children_b
costs <- costs %>% select(- children)
glimpse(costs)

# QUESTION - Both age and bmi need to be scaled and centered.  Scale and center each as was done in the RPubs and assign back to their original columns

# ANSWER - 
costs$age <- scale(costs$age)
costs$bmi <- scale(costs$bmi)

# QUESTION - Make a linear regression model with charges as your target and all the other features as your predictors.  What region has the lowest healthcare costs?  How much does having children influence insurance costs?  Given we scaled and centered age and bmi, which one has a bigger effect on costs for a single SD increase in the respective feature?

# ANSWER - 
charges_model <- lm(charges ~ ., data = costs)
summary(charges_model)
#South East region has the lowest healthcare costs since the summary of the lm 
#model for it shows that it has the lowest coefficient of all the models.
#Having children as opposed to not having them causes insurance costs to increase by 999.6 
#which is the coefficient of the variable in the estimated lm model.
#Age has a bigger impact than bmi on costs.

# QUESTION - Make a figure showing how age or bmi impacts insurance costs.

# ANSWER - 
plot(costs$charges ~ costs$bmi)

