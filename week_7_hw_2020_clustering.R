# Siddhant Bhardwaj

# This week's assignment will focus on using k-means clustering to find out how many groups of wines exist based on a dataset of 178 wines that vary in 13 different features.  Each of these features relate to some aspect of the wine's chemical composition.  Given there are lots of different wine varieties, the question is if we can detect varietal differences based only on these chemical properties.

# Packages and data
library(tidyverse)
library(caret)
library(dbscan)
library(ggmap)
library(factoextra)
wine <- read_csv("https://docs.google.com/spreadsheets/d/1QA96h2A_i35FBKCrlmm8_QPgdcUYSZlTDeNFSUl8sEc/gviz/tq?tqx=out:csv")


# QUESTION - Scale your data

# ANSWER

wine_scaled <- data.frame(scale(wine))


# QUESTION -  fit a k-means model with k= 5 and plot the resulting 
#clusters with alcohol on the x and flavanoids on the y.  
#Be sure to color your points by cluster and make sure cluster isn't a 
#continuous variable. Also, be sure to make a copy of your original data 
#frame to assign your clusters back to as we're going to use the scaled data 
#again.

# ANSWER - 

wine_new <- data.frame(wine)
wine_kmeans_scaled <- kmeans(wine_scaled, centers = 5, nstart = 20)
wine_kmeans_scaled_clusters <- factor(wine_kmeans_scaled$cluster) # extract from km_scaled object
wine_new$cluster <- wine_kmeans_scaled_clusters # add back to original data frame

ggplot(wine_new,
       aes(x = alcohol, y = flavanoids, color = cluster)) +
  geom_point() 

# QUESTION - What do you think of the results? 
#Do the clusters make sense?  Too many or too few?

# ANSWER - 

#The clusters seem to be too close to each other. As we can see from the plot,
#the clusters 4 and 5 seem to be too meshed in together and the clusters 2 and 3
#also have points in their zones. Thus, it seems like there are too many clusters. 


# QUESTION - Calculate the total within-cluster sum of squares from your above model.

# ANSWER - 

print(wine_kmeans_scaled$tot.withinss)


# QUESTION - We're now going to use the elbow method to figure out 
#our optimal k.  First make your empty data frame with two columns and 
#10 rows.  One column should be named for the within column variation and 
#the other for the number of k

# ANSWER

new_df <- data.frame(matrix(ncol = 2, nrow = 10))
colnames(new_df) <- c('column_variation','num_of_k')

# QUESTION - Now use a for loop to fit a model for k values ranging from 1 to 10.  You should fit the model for each value in i, calculate the within-column variation, and then add that value and the k value into your empty data frame

# ANSWER

for(i in 1:10) { 
  km <- kmeans(wine_scaled, centers = i) # to run k through the range
  wi_ss <- km$tot.withinss # get total within-cluster sum of squares
  new_df[i, 'num_of_k'] <- i
  new_df[i, 'column_variation']  <- wi_ss
}



# QUESTION - Now make your elbow plot

# ANSWER

ggplot(new_df,
       aes(x = num_of_k, y = column_variation)) +
  geom_line() +
  labs(x = 'number of clusters', y = 'total within-cluster sum of squares') +
  theme_classic()


# QUESTION - Based on this, what value k should you use?  Go and refit 
#the single model with this value, assign clusters back to the data, and 
#replot, coloring by cluster

# ANSWER

#I would use the value of k = 3, since after this point, 
#the total-within sum of squares does not change substantially.

wine_new_again <- data.frame(wine)
wine_kmeans_scaled_1 <- kmeans(wine_scaled, centers = 3, nstart = 20)
wine_kmeans_scaled_clusters_1 <- factor(wine_kmeans_scaled_1$cluster) # extract from km_scaled object
wine_new_again$cluster <- wine_kmeans_scaled_clusters_1 # add back to original data frame

ggplot(wine_new_again,
       aes(x = alcohol, y = flavanoids, color = cluster)) +
  geom_point() 

# QUESTION - How good of a job did this do clustering?  Give an example of
#what you could use this model for.

# ANSWER - 

#Since, the plot shows well-defined clusters,therefore it seems like 
#it has done a good job of clustering.
#Based on this information, we could create newer and more optimized ways 
#to deal with flavanoids based on alcohol content which would allow us
#to make better wines.
