# Your info

# Classification homework - the goal of this assignment is to predict if someone has 'good' or 'bad' credit.  You're going to use both a logistic regression model as well as a KNN model to try and best predict credit class.  

# NOTE - People get most tripped up at the end in either making sure their predictions are outputted as classes (or converted to them) and comparing them to the true test targets. 

# Load packages
library(tidyverse)
library(caret)
library(MuMIn)

# Bring in data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")
credit <- data.frame(credit)

# QUESTION - As always, take some time to explore your data.  What levels are present in the character columns?  

# ANSWER - 

glimpse(credit)
summary(credit)

#There are 21 columns in the dataset out of which 14 columns are character columns whereas 7 columns are 
#numeric columns. 

summary(factor(credit$checking_status))
#The levels in checking_status are <0'       >=200'    0<=X<200' no checking' .
summary(factor(credit$credit_history))
#The levels in credit_history are  all paid, critical/other existing credit, delayed previously, existing paid, no credits/all paid.
summary(factor(credit$purpose))
#The levels in purpose column are  business domestic appliance, education furniture/equipment, new car , other, radio/tv, repairs, retraining, used car.
summary(factor(credit$savings_status))
#The levels in saving_status column are  <100'           >=1000'       100<=X<500'      500<=X<1000' no known savings' 
summary(factor(credit$employment))
#The levels in employment status is <1'       >=7'    1<=X<4'    4<=X<7' unemployed .
summary(factor(credit$personal_status))
#The levels in employment status is female div/dep/mar, male div/sep, male mar/wid, male single.
summary(factor(credit$other_parties))
#The levels in other_parties is co applicant, guarantor, none. 
summary(factor(credit$property_magnitude))
#The levels in property magnitude are car, life insurance, no known property, real estate.
summary(factor(credit$other_payment_plans))
#The levels in other_payment_plans are   bank   none stores.
summary(factor(credit$housing))
#The levels in housing are for free, own, rent.
summary(factor(credit$job))
#The levels in job are high qualif/self emp/mgmt, skilled, unemp/unskilled non res', unskilled resident' 
summary(factor(credit$own_telephone))
#The levels in own_telephone are none  yes. 
summary(factor(credit$foreign_worker))
#The levels in foreign_worker are no, yes.
summary(factor(credit$class))
#The levels in class column are bad and good.

# QUESTION - Based on your exploration, pick two columns and describe how you think they might be related to credit score.  What levels within these features will have what effect on the target ?

#ANSWER - Two important columns are credit history and savings status in relation to credit score. Credit history
#refers to the past history of an individual's debt repayment status and would thus be useful to deciding an individual's credit score.
#Savings status would denote an individual's record and status of savings and would help in determining if an individual can service his loan or the amount of 
#loan that he would need to take in which would determine if his credit score is good or bad.

#Credit History - The level 'all paid' would mean that the person has paid all prior loans which would have 
#good effect on credit score, delayed previously would have a poor effect on credit history, existing 
#paid and critical/other existing credit would probably have neutral and slightly negative effect on credit score respectively.

#Savings History - The level '<100' would probably have a poor impact on credit score, '>=1000' would have a 
#the most positive impact on savings, '100<=x<50' would have only slightly positive impact, '500<=x<1000' would have 
#a more positive impact and no known savings would have avery negative impact on credit score.

# QUESTION - Start by creating your dummy variables.  

# ANSWER - 

credit_dummy <- dummyVars(class ~ ., data = credit, fullRank = TRUE)
credit_dummy <- predict(credit_dummy , newdata = credit)
credit_new <- data.frame(credit_dummy)

# QUESTION - you need to convert your target 'class' to a binary factor.  Make it so that if the value is 'good' that it's replaced with a 1, and if it's 'bad' it's replaced with a 0.  Then convert that to a factor. Do this all while overwriting the original 'class' column so that you don't have two targets. 

# ANSWER - 

credit_new$class <- factor(ifelse(credit$class == 'good',1,0))
credit_new$class

# QUESTION - Now split your data into train and test features and targets.  Use an 80/20 train/test split.

#First attaching the dummy variables to the original dataframe for eventual splitting into training and testing.

class_val <- credit %>% select(class)
credit_new <- cbind(credit_new, class_val)

# ANSWER - 
set.seed(888) # run this first and then put your answer below it. 

credit_split <- createDataPartition(credit_new$class, p = 0.8, list = FALSE)

#Creating train and test target and features.

features_train <- credit_new[ credit_split, !(names(credit_new) %in% c('class'))] 
features_test <- credit_new[-credit_split, !(names(credit_new) %in% c('class'))]

target_train <- credit_new[ credit_split, "class"]
target_test <- credit_new[-credit_split, "class"]

# QUESTION - Take a second to verify that your targets and features contain the proper data.  Check the number of rows in them.  Check to make sure the proper columns are in them as well!

# ANSWER - 

nrow(features_train) == 0.8 * length(credit_new)

nrow(features_test) == 0.2 * length(credit_new)

all(colnames(features_train) == colnames(features_test))

target_train

target_test

# QUESTION - On to preprocessing.  Preprocess your data using centering, scaling, and the knnImpute within method.  

# ANSWER - 

preprocess_object <- preProcess(features_train, method = c('center', 'scale', 'knnImpute'))
preprocess_object

features_train <- predict(preprocess_object, newdata = features_train)
features_test <- predict(preprocess_object, newdata = features_test)

# QUESTION - Why do you get your preprocessing object from only your training data? 

# ANSWER - Data leakage!



# QUESTION - Use the formula from the lesson/book to calculate your k value before we fit our kNN model.  Remember to round to an odd value.

# ANSWER

k_form <- sqrt(length(target_test))
k_form

#Since, the value is 14.14 and one should round to an odd value, therefore, k value is assumed to be 15.


# QUESTION - Fit a kNN model on your training data 


# ANSWER - 

knn_fit <- knn3(features_train, target_train, k = 15)
knn_fit


# QUESTION - Now use that to predict the classes of your test data

# ANSWER - 

knn_pred_15 <- predict(knn_fit, features_test, type = 'class' )
knn_pred_15



# QUESTION - Make a predictions data frame with your true target values and your knn_pred values

# ANSWER - 

predictions <- cbind(data.frame(target_test,knn_pred_15))
predictions

# QUESTION - Now fit a logistic regression.  Remember you have to join your features and target back together to train your model. You'll also have to rename your target back to 'class'

# ANSWER - 

full_train <- cbind(target_train,features_train)
full_train <- rename(full_train, c('class' = 'target_train'))
log_train <- glm(class ~ ., family = 'binomial', data = full_train)


# QUESTION - Check out a summary of your logistic regression model.  Are all features important?  Do the ones you made predictions about earlier pan out?

# ANSWER -

summary(log_train)

#All features are important however some features are more important than others in terms of the estimate
#Also, both savings_status and credit_history are important.

# QUESTION - Generate your predictions for your test data.  Be sure to look at the data and convert the values to classes if needed.

# ANSWER - 

log_pred <- predict(log_train, newdata = features_test, type = 'response')
log_pred <- ifelse(log_pred >= 0.5, 1, 0)



# QUESTION - Add these logistic regression predictions to your predictions data frame as a new column

# ANSWER - 

predictions$log_pred <- factor(log_pred)



# QUESTION - Calculate error rates between our true test values and the predicted values from both models.  Which model did best?


# ANSWER - 

predictions$knn_error <- ifelse(predictions$target_test != predictions$knn_pred_15, 1, 0)
predictions$log_error <- ifelse(predictions$target_test != predictions$log_pred, 1, 0)
summary(predictions)
#Logistic regression model did slightly better than KNN model with error rates of 23.5% and 27& respectively.

# QUESTION - Make confusion matrices for both models.  Which model had more true positives?  Which had more true negatives?   

# ANSWER - 

knn_conf <- confusionMatrix(predictions$knn_pred_15, predictions$target_test)
log_conf <- confusionMatrix(predictions$log_pred, predictions$target_test)

#log_conf has a higher true positive rate. 
#log_conf also has a higher true negative rate.

# QUESTION - Go dial k back to 9 and then rerun the script.  Did that improve model fit?

# ANSWER - 

knn_fit_9 <- knn3(features_train, target_train, k = 9)
knn_pred_9 <- predict(knn_fit_9, features_test, type = 'class' )
predictions$knn_pred_9 <- factor(knn_pred_9)
predictions$knn_pred_9_error <- ifelse(predictions$target_test != predictions$log_pred, 1, 0)
summary(predictions)

#No, the k-fit of 9, is not an improvement over the previous and did not improve model fit as it only reached
#the previosly acheievd error rate of 23.5%.

# QUESTION - What do you think of this in the end?  Is using these models better than naively predicting who had good or bad credit?  Are the as accurate as you would have thought?  Any thoughts on what we could do to improve fit? 

# ANSWER - 

#Yes, using the models is better than naively predicting in the end since, it allows the calculation the effects of different 
#variables on predicting good or bad credit score, as opposed to a guessing scenario which would give a 50% 
#likelihood to both good and bad options regardless of the values of the variables. They are a little less
#accurate than what I would expect them to be. Hyperparameter tuning can be very useful to improving
#the fit of the model.

