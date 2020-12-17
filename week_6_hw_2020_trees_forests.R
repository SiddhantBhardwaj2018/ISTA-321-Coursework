library(tidyverse)
library(rpart)
library(caret)
library("rpart.plot")
library(randomForest)

# OBJECTIVE - This week's homework will focus on evaluating a decision tree and random forest to classify credit scores as 'good' or 'bad'.

# Load data and set seed
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")
set.seed(888)

# QUESTION - Convert all your character strings to factors.  I did this in the rpubs lesson using the mutate_if function

# ANSWER - 

credit <- credit %>% mutate_if(is.character, as.factor)


# QUESTION - Use an exploration method to confirm that all your character columns are now factors.

# ANSWER -

summary(credit)

# QUESTION - before we starting trying to predict anything, make a decision tree with all the features and class as your target.  Also make a plot of this decision tree.

# ANSWER - 

split_index <- createDataPartition(credit$class, p = 1.0, list = F)
training <- credit[split_index,]
credit_tree <- rpart(class ~ . ,data = training)
rpart.plot(credit_tree)

# QUESTION - Are there too many terminal nodes?  Apply a method to reduce the complexity of the tree and plot the results.  Use a cost penalty of 0.05.

# ANSWER - 

credit_pruned <- prune(credit_tree, cp = 0.05)
rpart.plot(credit_pruned)


# QUESTION - Given the above plot, what are the effects of the different levels of checking account status (amount of money in it)?

# ANSWER - 

#Thus, when we see the plot, if the checking status is less than 0 or between
#0 and 200, then we go down the left branch, where if duration is less than or equal to 23,
#then it is considered bad and if savings_status is less than 100 or lies between 100 and 500,
# or lies between 500 and 1000, then it is considered bad with only 20% as opposed to good with 4%.

# QUESTION - Now let's split your data into training and test sets using an 80/20 train/test split.  Create 10 folds.

# ANSWER -

split_index <- createDataPartition(credit$class, p = 0.8, list = F,times = 10)
split_index


# QUESTION - Create an empty data frame with three columns, one for decision tree error rate, one for logistic regression error rate, and another for fold number.  The data frame should have as many columns as folds you created.  Don't forget to name the columns.

# ANSWER

error_dataframe <- data.frame(matrix(ncol = 3, nrow = ncol(split_index)))
colnames(error_dataframe) <- c('decision_tree_error', 'logistic_regression_error' , 'fold_number')

# QUESTION - Create a for loop that splits your data, then both fits a decision tree and logistic regression model with class as the target and the rest of the columns as your features.  It should also generate predictions and then calculate and store the error rates as well as the fold number. 


# ANSWER - 

for(i in 1:nrow(error_dataframe)){
  training <- data.frame(credit[split_index[,i],])
  features_train <- credit[ split_index[,i], !(names(credit) %in% c('class'))]
  features_test  <- credit[-split_index[,i], !(names(credit) %in% c('class'))]
  target_train <- credit[ split_index[,i], "class"]
  target_test <- credit[-split_index[,i], "class"]
  tree_model <- tree(class ~ ., training)
  tree_pred <- predict(tree_model,features_test,type="class")
  log_model <- glm(class ~ ., family = 'binomial', data = training)
  log_pred <- predict(log_model, features_test,type="response")
  log_pred <- as.factor(ifelse(log_pred >= 0.5, "good", "bad"))
  error_dataframe[i,'decision_tree_error'] <- mean(ifelse(target_test != data.frame(tree_pred),1,0))
  error_dataframe[i,'logistic_regression_error'] <- mean(ifelse(target_test != data.frame(log_pred),1,0))
  error_dataframe[i,'fold_number'] <- i
}

# QUESTION - Now fit a random forest model, generate predictions and mean error rate.  No need to do the for loop.  You can either generate a new single split index or just use an entry from your previously created split index

# ANSWER - 

rf_train <- randomForest(class ~ ., data = training, mtry = 5)
rf_preds <- predict(rf_train, features_test)
mean_error_rf <- mean(ifelse(target_test != data.frame(rf_preds),1,0))

# QUESTION - Calculate the mean error rate for your decision tree and logistic regression models.  How do those compare to the error rate in your random forest?  Include code to do both below.

# ANSWER

mean_error_decision_tree <- mean(error_dataframe[,'decision_tree_error'])
mean_error_logistic_regression <- mean(error_dataframe[,'logistic_regression_error'])

# QUESITON - You want to create a credit predictor for when 
#someone applies for a credit card.  When doing so they input all the
#same feature data and then you need the algorithm to return back 'good'
#or 'bad'.  Which model type would you use for this?

# ANSWER - 

#If I were to want to create a credit predictor, then I would choose
#the random forest model since it gives the lowest error rate of 
#all the chosen models.

# QUESTION - Which features were most important in your random forest model.  Give me the top three.  

# ANSWER - 

importance(rf_train)
#The top three important models are checking_status, duration, credit_history.

# QUESTION - Are you surprised by which model did best?  
#Give reason that one model performed better than the other.

# ANSWER - 

#No, I am not surprised by the fact that the random forest model did better.
#Non-parametric approaches tend to be more flexible and accurate as
#opposed to parametric approaches and moreover since the random forest model
#is  that it substitutes cross-validation with generating large number of trees
#to increase the accuracy of the model.
