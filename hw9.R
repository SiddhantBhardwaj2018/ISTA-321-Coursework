#Siddhant Bhardwaj
#HW9 - Wrapping Up Assignment

# load libraries 
library(tidyverse)
library(caret)
library(ISLR)
library(glmnet)

#Obtaining the beer dataset
beer <- read_csv("https://docs.google.com/spreadsheets/d/1FQvlCVdeGiMttYgBCsUXge3P_KfL8AmI12cvSXxkPuU/gviz/tq?tqx=out:csv")

#Obtaining a summary of the dataset of beers
summary(beer)

#Obtaining information about the columns of the table.
glimpse(beer)

#In this assignment, I shall be attempting to predict beer_overall_score 
#by performing multiple regression over suitable variables in the dataset.

#Exploring the beer_overall_score column.

#Obtaining head in beer_overall_score column
head(beer[,'beer_overall_score'])

#Obtaining tail in beer_overall_score column
tail(beer[,'beer_overall_score'])

#Summary of the beer_overall_score column
summary(beer[,'beer_overall_score'])
#Here, we can see that the minimum value is 0.000 and maximum value is 5.000
#with a mean value of 3.417

#Since, the data is continuous numeric data, we make a histogram of the
#'beer_overall_score' column.

hist(beer$beer_overall_score)
#This histogram shows a left-skewed distribution with a significant population
#around the 0 value of the beer_overall_score.

#Obtaining a box plot of the brewery_country with beer-Overall_score
ggplot(beer,aes(x = brewery_country , y = beer_overall_score)) + geom_boxplot()

#Obtaining a scatter plot of the number_of_reviews with beer-Overall_score
ggplot(beer,aes(x = number_of_reviews , y = beer_overall_score)) + geom_point()


#Obtaining a scatter plot of the abv(alcohol by volume) with beer-Overall_score
ggplot(beer,aes(x = abv , y = beer_overall_score)) + geom_point()

#Obtaining a scatter plot of the year with beer_overall_score
ggplot(beer,aes(x = year , y = beer_overall_score)) + geom_point()
#Obtaining a box plot of the brewery_country with beer-Overall_score
ggplot(beer,aes(x = brewery_country , y = beer_overall_score)) + geom_boxplot()

#Since brewery_city,add_date and brewery_name have too many categorical
#variables, then there is no option but to drop them as variables from my 
#attempt to use multiple regression to solve this problem.

#I shall be using abv, brewery_country, number_of_reviews,style
#for multiple regression to predict beer_overall_score.

#Preprocessing the dataset.

#Since brewery_country and brewer_city are categorical variables, one-hot-encoding
#needs to be performed.

dumm1 <- dummyVars( ~ brewery_country, data = beer, fullRank = TRUE)
dumm1_pred <- predict(dumm1, newdata = beer)
beer <- cbind(beer, dumm1_pred) # bind our two data frames

dumm2 <- dummyVars( ~ style, data = beer, fullRank = TRUE)
dumm2_pred <- predict(dumm2, newdata = beer)
beer <- cbind(beer, dumm2_pred) # bind our two data frames

#Dropping the unneeded columns
dropped <- c('brewery_country','add_date','brewery_city','brewery_name','style', 'year'  ,'beer_name')
#Reassigning the  dataframe
beer <- beer[, !(names(beer)) %in% dropped]

#Checking for missing values 

sum(is.na(beer$number_of_reviews))
sum(is.na(beer$beer_overall_score))
sum(is.na(beer$abv))
sum(is.na(beer$`brewery_countryUnited.Kingdom`))
sum(is.na(beer$`brewery_countryUnited.States`))

#Since there are missing values in the dataframe, therefore, I will be dropping
#them since there are too many of them to impute into them and make a fair prediction.

beer <- beer[complete.cases(beer),]

hist(beer$number_of_reviews)
hist(beer$abv)

#We need to rescale number_of_reviews
beer$number_of_reviews <- rnorm(mean = sum(beer$number_of_reviews)/length(beer$number_of_reviews),
sd = sd(beer$number_of_reviews),n = length(beer$number_of_reviews))

#Renaming Columns to avoid formatting errors encountered in  running prediction

beer <- beer %>% rename(brewery_UK = `brewery_countryUnited Kingdom`)
beer <- beer %>% rename(brewery_US = `brewery_countryUnited States`)
beer <- beer %>% rename(styleAmerican_Adjunct_Lager = `styleAmerican Adjunct Lager`)
beer <- beer %>% rename(styleAmerican_Amber_Red_Ale = `styleAmerican Amber / Red Ale`)
beer <- beer %>% rename(styleAmerican_Barleywine = `styleAmerican Barleywine`)
beer <- beer %>% rename(styleAmerican_Black_Ale = `styleAmerican Black Ale`)
beer <- beer %>% rename(styleAmerican_Blonde_Ale = `styleAmerican Blonde Ale`)
beer <- beer %>% rename(styleAmerican_Double_Imperial_IPA = `styleAmerican Double / Imperial IPA`)
beer <- beer %>% rename(styleAmerican_Double_Imperial_Stout = `styleAmerican Double / Imperial Stout`)
beer <- beer %>% rename(styleAmerican_IPA = `styleAmerican IPA`)
beer <- beer %>% rename(styleAmerican_Pale_Ale_APA = `styleAmerican Pale Ale (APA)`)
beer <- beer %>% rename(styleAmerican_Pale_Lager = `styleAmerican Pale Lager`)
beer <- beer %>% rename(styleAmerican_Pale_Wheat_Ale = `styleAmerican Pale Wheat Ale`)
beer <- beer %>% rename(`styleAmerican_Porter` = `styleAmerican Porter`)
beer <- beer %>% rename(styleAmerican_Stout = `styleAmerican Stout`)
beer <- beer %>% rename(styleAmerican_Strong_Ale = `styleAmerican Strong Ale`)
beer <- beer %>% rename(styleAmerican_Wild_Ale = `styleAmerican Wild Ale`)
beer <- beer %>% rename(styleBelgian_Dark_Ale = `styleBelgian Dark Ale`)
beer <- beer %>% rename(styleBelgian_IPA = `styleBelgian IPA`)
beer <- beer %>% rename(styleBelgian_Strong_Dark_Ale = `styleBelgian Strong Dark Ale`)
beer <- beer %>% rename(styleBelgian_Strong_Pale_Ale = `styleBelgian Strong Pale Ale`)
beer <- beer %>% rename(styleBerliner_Weissbier = `styleBerliner Weissbier`)
beer <- beer %>% rename(styleBière_de_Garde = `styleBière de Garde`)
beer <- beer %>% rename(styleCalifornia_Commo_Steam_Beer = `styleCalifornia Common / Steam Beer`)
beer <- beer %>% rename(styleEnglish_Pale_Ale = `styleEnglish Pale Ale`)
beer <- beer %>% rename(styleEnglish_Porter = `styleEnglish Porter`)
beer <- beer %>% rename(styleEuro_Dark_Lager = `styleEuro Dark Lager`)
beer <- beer %>% rename(styleEuro_Pale_Lager = `styleEuro Pale Lager`)
beer <- beer %>% rename(styleGerman_Pilsener = `styleGerman Pilsener`)
beer <- beer %>% rename(styleIrish_Dry_Stout = `styleIrish Dry Stout`)
beer <- beer %>% rename(styleIrish_Red_Ale = `styleIrish Red Ale`)
beer <- beer %>% rename(styleKellerbier_Zwickelbier = `styleKellerbier / Zwickelbier`)
beer <- beer %>% rename(styleLambic_Fruit = `styleLambic - Fruit`)
beer <- beer %>% rename(styleLight_Lager = `styleLight Lager`)
beer <- beer %>% rename(styleMaibock_Helles_Bock = `styleMaibock / Helles Bock`)
beer <- beer %>% rename(styleMunich_Dunkel_Lager = `styleMunich Dunkel Lager`)
beer <- beer %>% rename(styleMunich_Helles_Lager = `styleMunich Helles Lager`)
beer <- beer %>% rename(styleOatmeal_Stout = `styleOatmeal Stout`)
beer <- beer %>% rename(styleQuadrupel_Quad = `styleQuadrupel (Quad)`)
beer <- beer %>% rename(styleSaison_Farmhouse_Ale = `styleSaison / Farmhouse Ale`)
beer <- beer %>% rename(styleScotch_Ale_Wee_Heavy = `styleScotch Ale / Wee Heavy`)
beer <- beer %>% rename(styleMärzen_Oktoberfest = `styleMärzen / Oktoberfest`)

#Cross Validation

split_index <- createDataPartition(beer$beer_overall_score, p = 0.8, list = FALSE, times = 10)
head(split_index, 5)

#Creating error_df
comp_df <- data.frame(matrix(ncol = 4, nrow = ncol(split_index)))
colnames(comp_df) <- c('lin_reg_mse', 'ridge_mse', 'lasso_mse' ,'fold')

#Applying multiple regression, lasso and ridge models in cross-validation


for(i in 1:ncol(split_index)) {
  
  features_train <- beer[split_index[,i], !(colnames(beer) %in% c('beer_overall_score'))]
  features_test <- beer[-split_index[,i], !(colnames(beer) %in% c('beer_overall_score'))]
  target_train <- beer[split_index[,i], c('beer_overall_score')]
  target_test <- beer[-split_index[,i], c('beer_overall_score')]
  training <- data.frame(features_train, target_train)
  training <- training %>% rename(score = target_train)
  reg_mod <- lm(score ~ ., data = training)
  reg_pred <- predict(reg_mod, newdata = features_test)  
  # calculate MSE and add to DF
  reg_mse <- mean((target_test - reg_pred)^2)
  comp_df[i, 'lin_reg_mse'] <- reg_mse
  comp_df[i, 'fold'] <- i
  # generate x and y (this doesn't have to be here but will leave for clarity)
  features <- model.matrix(beer_overall_score ~ ., beer)[,-1]
  target <- beer$beer_overall_score
  lambda <- 10^seq(10, -2, length = 100)
  # split x and y 
  features_train_m <- features[split_index[,i],]
  features_test_m <- features[-split_index[,i],]
  target_train_m <- target[split_index[,i]]
  target_test_m <- target[-split_index[,i]]
  # fit ridge model
  ridge_mod <- glmnet(features_train_m, target_train_m, alpha = 0)
  cv_out <- cv.glmnet(features_train_m, target_train_m, alpha = 0, lambda = lambda, grouped = FALSE)
  bestlam <- cv_out$lambda.min
  ridge_pred <- predict(ridge_mod, s = bestlam, newx = features_test_m)
  # calculate error and add
  ridge_mse <- mean((target_test_m - ridge_pred)^2)
  comp_df[i, 'ridge_mse'] <- ridge_mse
  # same for lasso
  lasso_mod <- glmnet(features_train_m, target_train_m, alpha = 1)
  cv_out <- cv.glmnet(features_train_m, target_train_m, alpha = 1, lambda = lambda, grouped = FALSE)
  bestlam <- cv_out$lambda.min
  lasso_pred <- predict(lasso_mod, s = bestlam, newx = features_test_m)
  lasso_mse <- mean((lasso_pred - target_test_m)^2)
  comp_df[i, 'lasso_mse'] <- lasso_mse
  
}

#Graphing our results
data_long <- gather(data = comp_df, key = type, value = error, c('lin_reg_mse', 'ridge_mse', 'lasso_mse' ), factor_key=TRUE)
data_long[c(1,2,3,4,5,6,7,8,9,10),]

ggplot(data_long,
       aes(x = fold, y = error, color = type)) +
  geom_line() +
  labs(x = 'Fold Number', y = 'Mean Squared Error') +
  theme_classic()

#Reasons for choosing the said models.

#I haved the multiple regression model, the lasso model and the ridge regression 
#model for prediction purposes. I have used the simple multiple regression model 
#as a benchmark model against which I would compare the other models of lasso and
#ridge regression.I used lasso regression since it would constitute an example of 
#regularization since the preprocessed dataset has a large number of variables and it would
#useful to see which of them are actually useful to the prediction process. The
#same thinking has guided me when choosing the ridge regression model.

#Description of output

#Multiple Regression model
summary(reg_mod)

#Based on the multiple regression model, the following features are important - 
#number_of_reviews,abv, styleAmerican_Adjunct_Lager, styleAmerican_Pale_Wheat_Ale,
#styleBelgian_Strong_Dark_Ale,styleBelgian_Strong_Pale_Ale,styleCalifornia_Commo_Steam_Beer,
#styleDunkelweizen,styleEnglish_Pale_Ale,styleEnglish_Porter,styleIrish_Red_Ale,
#styleKellerbier_Zwickelbier,styleLight_Lager,styleMärzen_Oktoberfest and styleTripel

#Since the p-value of the model is less than 2.2e-16, hence the null hypothesis can
#be dismissed.

#However, the R2 is only 13.19%, which is not a good explanatory power.

#Lasso Regression Model
lasso_coef <- predict(lasso_mod, type = "coefficients", s = 0)[1:51,]
lasso_coef

#Utilizing the Lasso Regression Model, the regularization process shows that the
#intercept matters the most and other factors are not as important to the prediction
#process.

#Ridge Regression Model
ridge_coef <- predict(ridge_mod, type = "coefficients", s = 200)[1:51,]
ridge_coef

#Utilizing, the Ridge Regression Model, the regularization process showed that 
#abv, brewery_US and number_of_reviews were important.

#Looking at the Graph, we can see that the MSE for ridge_regression model is the lowest
#while it is highest for lasso_model. The linear regression model tracks the ridge_regssion
#model closely which suggests that it would make more sense to simply use it in place of
#other regularization methods.