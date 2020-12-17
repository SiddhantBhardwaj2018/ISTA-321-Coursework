# Siddhant Bhardwaj



# The goal of this homework is simple. You're going to take your kNN model from last homework and perform K-fold cross-validation using a for loop.

# Load packages
library(tidyverse)
library(caret)
set.seed(888)

# Bring in data
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")

# QUESTION - Perform your dummy variable conversion first.  Also remember to bind back on your target feature 'class' and then convert that to a binary factor.  Look at the homework solution if you're struggling.

# ANSWER -

credit_dummies <- dummyVars(class ~ ., data = credit, fullRank = TRUE)
credit_df_encoded <- data.frame(predict(credit_dummies, newdata = credit))
credit_df <- cbind(credit_df_encoded, credit[,'class'])

# QUESTION - Create your split index.  I want you do start with 10 folds and a training set size of 70% of the data.

# ANSWER -

split_index <- createDataPartition(credit_df$class, p = 0.7, list = FALSE, times = 10)



# QUESTION - Initialize your empty data frame.  Make sure it has two columns...  one for error and the other for fold_number

# ANSWER -

error_dataframe <- data.frame(matrix(ncol = 2, nrow = ncol(split_index)))
colnames(error_dataframe) <- c('test_error', 'folds')


# QUESTION - create your for loop.  It should run for each column within split index.  
#For each run it should use the i'th entry of the index to split the data, then preprocess, fit a model, 
#predict with that knn model (use k = 11), calculate the TEST ERROR RATE (see 2.2.3 in the book for a 
#reminder or the RPubs), then add that error to the I'th spot of the error column in the data frame.  
#Also add the fold number to the data frame.

# ANSWER -


for(i in 1:nrow(error_dataframe)){
  features_train <- credit_df[ split_index[,i], !(names(credit_df) %in% c('class'))]
  features_test  <- credit_df[-split_index[,i], !(names(credit_df) %in% c('class'))]
  target_train <- credit_df[ split_index[,i], "class"]
  target_test <- credit_df[-split_index[,i], "class"]
  preprocess_object <- preProcess(features_train, method = c('scale', 'center', 'knnImpute'))
  target_train <- as.factor(target_train)
  target_test <- as.factor((target_test))
  features_train <- predict(preprocess_object, features_train)
  features_test <- predict(preprocess_object, features_test)
  knn_fit <- knn3(features_train, target_train, k = 11)
  knn_pred <- predict(knn_fit, features_test, type = 'class' )
  error <- mean(ifelse(target_test != knn_pred, 1, 0))
  error_dataframe[i,'test_error'] <- error
  error_dataframe[i, 'folds'] <- i
}


# QUESTION - Make a figure using your data frame of errors and fold numbers.

# ANSWER -

normal_p <- error_dataframe

ggplot(normal_p, aes(x=folds, y = test_error)) + geom_line(color = 'red')

# QUESTION - Now that your for loop is running play around with the k of the kNN training or the split amount.  Can you get a lower test error rate?  What value allows you to minimize error more?

# ANSWER -

#Setting up the error calculation function

error_calculation <- function(split_pro,folds,kn){
  split_index <- createDataPartition(credit_df$class, p = split_pro, list = FALSE, times = folds)
  
  error_df <- data.frame(matrix(ncol = 2, nrow = ncol(split_index)))
  colnames(error_df) <- c('test_error', 'folds')
  
  for(i in 1:nrow(error_df)){
    # use ith column of split_index to create feature and target training/test sets
    features_train <- credit_df[ split_index[,i], !(names(credit_df) %in% c('class'))] 
    features_test  <- credit_df[-split_index[,i], !(names(credit_df) %in% c('class'))]
    target_train <- credit_df[ split_index[,i], "class"]
    target_test <- credit_df[-split_index[,i], "class"]
    # Still need to preprocess each new set!
    preprocess_object <- preProcess(features_train, 
                                    method = c('scale', 'center', 'knnImpute'))
    features_train <- predict(preprocess_object, features_train)
    features_test <- predict(preprocess_object, features_test)
    target_train <- as.factor(target_train)
    target_test <- as.factor(target_test)
    # Fit the model and predict
    knn_fit <- knn3(features_train, target_train, k = kn)
    knn_pred <- predict(knn_fit, features_test, type = 'class' )
    # Calculate error and store it
    error <- mean(ifelse(target_test != knn_pred, 1, 0))
    error_df[i,'test_error'] <- error
    error_df[i, 'folds'] <- i
  }
  return(error_df)
}

#Checking the error_rate for different values of split_pro with the same value of k = 11.
small_p <- error_calculation(split_pro = 0.3, folds = 10, kn = 11)
little_p <- error_calculation(split_pro = 0.5,folds = 10, kn = 11)
large_p <- error_calculation(split_pro = 0.85,folds = 10,kn = 11)

ggplot(normal_p,
       aes(x = folds, y = test_error)) +
  geom_line(color = 'red') +
  geom_line(aes(x = small_p$folds, y = small_p$test_error), color = 'blue') +
  geom_line(aes(x = little_p$folds, y = little_p$test_error), color = 'green') + 
  geom_line(aes(x = large_p$folds, y = large_p$test_error), color = 'orange')


#Looking at the obtained plot, it seems that out of all the chosen split values
#the split value of 0.7(70% of the dataset as training set) with a k = 11 and folds = 10,
#provides us with the lowest test error rate. However, the lowest test error rate is 
#provided at around the split-value of 85% (0.85), with folds ~ 9 which minimizes it
#the most.