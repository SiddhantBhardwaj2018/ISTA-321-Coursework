### Siddhant Bhardwaj

# Today you're going to be comparing a ridge, lasso, and regular regression model.  The goal is to determine the MSE on all three models and figure out which one fits best. I also want you to think about which one fits your goal of reducing the number of features. 

# You'll be working with a dataset where you're trying to predict an individual's credit rating from a bunch of other features.

# First, load in your packages tidyverse, caret, ISLR, glmnet.  You might need to install missing package

library(tidyverse)
library(caret)
library(ISLR)
library(glmnet)

# The data is in the ISLR package, so we can call it and then assign it to an object like this

data('Credit')
credit <- Credit
set.seed(888)

# First get rid of the uppercase column names
colnames(credit) <- tolower(colnames(credit))

# QUESTION - As always, explore your data.  Make a histogram of you target as well

# ANSWER

glimpse(credit)
summary(credit)
hist(credit$rating)

# QUESTION - You don't need to scale and center this dataset. 
#We can also skip imputing as there are no missing values.  
#We do need to one hot encode...  do that below!

# ANSWER - 

dummies_1 <- dummyVars( ~ gender, data = credit, fullRank = TRUE)
dummies_2 <- dummyVars( ~ ethnicity, data = credit, fullRank = TRUE)
dummies_3 <- dummyVars( ~ married, data = credit, fullRank = TRUE)
dummies_4 <- dummyVars( ~ student, data = credit, fullRank = TRUE)
dummies_pred_1 <- predict(dummies_1, newdata = credit)
dummies_pred_2 <- predict(dummies_2,newdata = credit)
dummies_pred_3 <- predict(dummies_3,newdata = credit)
dummies_pred_4 <- predict(dummies_4,newdata = credit)
credit <- cbind(credit, dummies_pred_1) # bind our two data frames
credit <- cbind(credit,dummies_pred_2)
credit <- cbind(credit,dummies_pred_3)
credit <- cbind(credit,dummies_pred_4)
dropped <-  c('married','ethnicity','student','gender')
credit <- credit[,!(names(credit) %in% dropped) ]
glimpse(credit)


# QUESTION - Make you splitting index.  Let's use an 80/20 train/test split

# ANSWER
split_index <- createDataPartition(credit$rating, p = 0.8, list = F)

# QUESTION - Make your training and test features and targets. 
#Remember that the features need to be a matrix... check the Rpubs!  
#You can also define your range of lambda values here. 
#Use the same range as in the rpubs.

# ANSWER

features <- model.matrix(rating~., data = credit)[,-1] # convert features to matrix
target <- credit$rating # get target
features_train_m <- features[split_index,]
features_test_m <- features[-split_index,]
target_train_m <- target[split_index]
target_test_m <- target[-split_index]
lambda_vals <- 10^seq(-2, 10, length = 1000)
lambda_vals[c(1,2,3, 998,999,1000)]

# QUESTION - Find your optimal lambda values through cross validation. 
#Remember the cv.glmnet function. Print your optimal lambda as well.

# ANSWER -

ridge_cv <- cv.glmnet(features_train_m, target_train_m, alpha = 0, lambda = lambda_vals, grouped = FALSE)
bestlam <- ridge_cv$lambda.min
bestlam


# QUESTION - Use this lambda to fit your ridge regression model and 
#then generate predictions from your test features.  
#Be sure to store the data as a vector of predictions so we can calculate 
#error rate.

# ANSWER - 

ridge_mod <- glmnet(features_train_m, target_train_m, alpha = 0)
ridge_preds <- predict(ridge_mod, s = bestlam, newx = features_test_m)


# QUESTION - Calculate your MSE for your ridge model

# ANSWER

mean((ridge_preds - target_test_m)^2)

# QUESTION - Get the model coefficients for this model. 
#Remember you got to use predict() like in the RPubs document.

# ANSWER

ridge_coef <- predict(ridge_mod, type = "coefficients", s = 0)
ridge_coef

# QUESTION - Now fit your Lasso model!  
#You need to find the best lambda again, fit the model using that, 
#measure error, and get the coefficients.  

# ANSWER -

lambda_vals <- 10^seq(-2, 10, length = 1000)
lambda_vals[c(1,2,3, 998,999,1000)]
lasso_cv <- cv.glmnet(features_train_m, target_train_m, alpha = 1, lambda = lambda_vals, grouped = FALSE)
bestlam <- lasso_cv$lambda.min
bestlam 
lasso_mod <- glmnet(features_train_m, target_train_m, alpha = 1)

lasso_preds <- predict(lasso_mod, s = bestlam, newx = features_test_m)
mean((lasso_preds - target_test_m)^2)
lasso_coef <- predict(lasso_mod, type = "coefficients", s = 0)[1:11]
lasso_coef


# QUESTION - Which model fit the best and why? 
#In your lasso model which coefficients were shrunk to zero?
#Which one of the two you would use for feature selection and why?

# ANSWER - 

#The lasso model fit best because its mean sqaured error is
#less than that of ridge model.

#The id and balance coefficients shrink to zero.

#I would use lasso model since it is more accurate.

# QUESTION - Let's compare this to a linear regression model.
#Take your one hot encoded data frame and split into training features 
#and targets using your existing split index.  Fit your model, and 
#calculate test MSE.  

# ANSWER

features_train <- features[split_index,]
features_test <- data.frame(features[-split_index,])
target_train <- target[split_index]
target_test <- target[-split_index]
training <- data.frame(features_train,target_train)
training <- training %>% rename(score = target_train)
reg_mod <- lm(score ~ ., data = training)
reg_pred <- predict(reg_mod, newdata = features_test)
reg_mse <- mean((target_test - reg_pred)^2)
reg_mse

# QUESTION - Compare your multiple regression summary to 
#the coefficients in your Lasso model.  Which features were shrunk to 
#zero in your Lasso?  Where those features significant or not in your 
#multiple regression model?  What the MSE that different?  If not, why 
#bother with Lasso?

#The id and balance were shrunk to zero in the Lasso model.
#They were only slightly significant in the multiple regression model.
#No, the MSE was not very different.
#For this particula problem, it would not have mattered if we defaulted to Multiple
#Regression model.
