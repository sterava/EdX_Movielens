#title: "HarvardX PH125.9x - Movielens: Movie recommendation system"
#author: "Stefania_Ravazzini"
#date: "2021/11/14"

# Install and load useful packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(forcats)
library(stringr)

# Data preparation
## Download data in R and build a unique dataset
## The zip file that we're about to download contains 10000054 ratings applied to 10677 movies 
## by 69878 unique users; the source is the online movie recommender service MovieLens. 
## The zip file contains two separate datasets, that we will merge in a single one.
## Users were selected at random for inclusion. All users selected had rated at least 20 movies. 
## Each user is represented by an id, and no other information is provided. 

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

## if using R 3.6 or earlier:
## movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
##                                           title = as.character(title),
##                                           genres = as.character(genres))
## if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

## Explore the two datasets

head(ratings) # --> ratings dataset is composed of the four features userId, movieId, rating and timestamp
head(movies)  # --> movies dataset has the three features movieId, title, genres

## Create a unique dataset, by using the key field movieId which is in common for the two datasets

movielens <- left_join(ratings, movies, by = "movieId")

# Data exploration and Data visualization
## Explore the unique dataset

ncol_movielens <- ncol(movielens)
nrow_movielens <- nrow(movielens)

ncol_movielens  # --> 6 features
nrow_movielens  # --> 10000054 observations

head(movielens)
str(movielens)

## Let's have a look at all the features of movieLens dataset, one by one
## users

ml_users <- unique(movielens$userId) 
length(ml_users) # --> 69878 unique users

ml_users_detail <- movielens %>%
  mutate(userId = as.factor(userId)) %>%
  group_by(userId) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

ml_users_detail%>%
  top_n(10) # --> top 10 users made at least 3400 reviews 

ml_users_detail %>%
  mutate(userId = fct_reorder(userId, count)) %>%
  ggplot(aes(x= userId, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "User Id", y = "Count of reviews") +
  ggtitle("Count of reviews by User Id")

ml_users_detail %>%
  summarize(min_review = min(count), 
            max_review = max(count),
            mean_review = mean(count),
            sd_review = sd(count))  # --> on average, a single user gave 143 reviews.

## movieId

ml_movieid <- unique(movielens$movieId) 
length(ml_movieid) # --> 10677 unique movies

ml_movieid_detail <- movielens %>%
  mutate(movieId = as.factor(movieId)) %>%
  group_by(movieId, title) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

head(ml_movieid_detail, 10) # --> top 10 movies are really famous titles and they have 
                            #     at least 27000 reviews each 

ml_movieid_detail %>%
  mutate(movieId = fct_reorder(movieId, count)) %>%
  ggplot(aes(x= movieId, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Movie Id", y = "Count of reviews") +
  ggtitle("Count of reviews by Movie Id") 

ml_movieid_detail_1 <- ml_movieid_detail %>%
  select(movieId, count) %>%
  ungroup()

ml_movieid_detail_1 %>%
  summarize(min_review = min(count), 
            max_review = max(count),
            mean_review = mean(count),
            sd_review = sd(count))  # --> on average, a single movie receives 937 reviews.

## genres

ml_genres <- unique(movielens$genres) 
length(ml_genres) # --> 797 unique movie genres

ml_genres_detail <- movielens %>%
  mutate(genres = as.factor(genres)) %>%
  group_by(genres) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

head(ml_genres_detail, 10) # --> top 10 genres are more or less split between drama, comedy and action

ml_genres_detail_1 <- ml_genres_detail %>%
  mutate(genres = as.character(genres))
        
ml_genres_detail_1 <- ml_genres_detail_1 %>%         
  mutate(genres = case_when(count < 80000 ~ "Other", 
                           TRUE ~ genres)) # --> we group the genres with less than 80K reviews into a single 
                                           # category, that we call "Other". This is made for data viz purpose

ml_genres_detail_1 <- ml_genres_detail_1 %>%
  group_by(genres) %>%
  summarize(count=sum(count))

ml_genres_detail_1 %>%
  mutate(genres = fct_reorder(genres, count)) %>%
  ggplot(aes(x= genres, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Genres", y = "Count of reviews") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  ggtitle("Count of reviews by Genre") 
  
  ml_genres_detail_1 %>%
    mutate(genres = fct_reorder(genres, count)) %>%
    filter(genres != "Other") %>%
    ggplot(aes(x= genres, y = count)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(x = "Genres", y = "Count of reviews") +
    theme(axis.text.x=element_text(angle=90,hjust=1)) + 
    ggtitle("Count of reviews by Genre - without 'Other' category") 

ml_genres_detail %>%
    summarize(min_review = min(count), 
              max_review = max(count),
              mean_review = mean(count),
              sd_review = sd(count))  # --> on average, a single genre receives 12547 reviews.

##rating

ml_rating <- unique(movielens$rating) 
length(ml_rating) # --> 10 unique ratings

ml_rating_detail <- movielens %>%
  mutate(rating = as.factor(rating)) %>%
  group_by(rating) %>%
  summarize(count=n())

ml_rating_detail  # --> ratings range from 0.5 to 5; ratings 3 and 4 are the most frequent ones

ml_rating_detail %>%
  ggplot(aes(x= rating, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label=paste0(round(count/nrow_movielens, 2)*100,"%")), position =position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(x = "Rating", y = "Count of reviews") +
  ggtitle("Count of reviews by Rating")


# Data cleaning and improvement
## Let's see if some missing value is detected

sum(is.na(movielens)) # --> no missing value

## We build a new column, the "movieYear" column, that we can create from a string-split on title column

movielens <- movielens %>%
  mutate(movieYear = str_sub(title, -5, -2))

head(movielens)
str(movielens)

sum(is.na(movielens)) # --> the new column doesn't bring in any NAs

## Let's explore the new column
## movieYear

ml_movieyear <- unique(movielens$movieYear) 
length(ml_movieyear) # --> we have movies released in 94 different years

ml_movieyear_detail <- movielens %>%
  mutate(movieYear = as.factor(movieYear)) %>%
  group_by(movieYear) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

ml_movieyear_detail%>%
  top_n(10) # --> top 10 years range from the 90s to 2002 and they have at least 300000 reviews each 

ml_movieyear_detail_1 <- ml_movieyear_detail %>%
  filter(count > 100000) # --> we filter to retain only movie years with relevant count of reviews.
                         # This is made for data viz purpose

ml_movieyear_detail_1 %>%
  mutate(movieYear = fct_reorder(movieYear, count)) %>%
  ggplot(aes(x= movieYear, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Movie Year", y = "Count of reviews") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  ggtitle("Count of reviews by Movie Year - focus on relevant review count")

ml_movieyear_detail %>%
  summarize(min_review = min(count), 
            max_review = max(count),
            mean_review = mean(count),
            sd_review = sd(count))  # --> on average, on a single movie release-year, we find 100000 reviews.

## Split between training set and test set
## Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

## We make sure that userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

## We then add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

## We perform a data consistency test. We check that the number of rows of training and test set is consistent
## with split of the source database

nrow_edx <- edx %>%
  summarize(n())

nrow_movielens * 0.9 - nrow_edx  # --> -6.4 check is satisfying

nrow_validation <- validation %>%
  summarize(n())

nrow_movielens * 0.1 - nrow_validation  # --> 6.4 check is satisfying


# Machine learning models
## We build up machine learning models that will predict the rating that a specific user would give to a
## specific movie. In other words, rating is our dependent variable. 
## The other features will be used as the independent variables.
## edx will be our training set, that we use for training the algorithm. 
## validation will be our test set, that we use for testing the "goodness of fit" of our predictions.

## First, we declare a RMSE function, which is equal to the square root of the average through all the 
## observations of the difference squared between actual ratings and predicted ratings.
## This RMSE function will help us evaluate the average error of our predictions.

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## We start from what we saw in HarvardX PH125.8x course and then we add some improvements.

## First step of the model. We define the following:
## Prediction for rating of user u of movie i = Yu,i
## Average rating = mu_hat
## Random error of prediction Yu,i = Eu,i
## Model 1.0: Yu,i = mu_hat + Eu,i

mu_hat <- mean(edx$rating) 

model1_rmse <- RMSE(validation$rating, mu_hat)

model1_rmse # --> RMSE of Model 1.0: 1.061202

## Second step of the model. We define the following:
## Movie effect = bi
## Model 2.0: Yu,i = mu_hat + bi + Eu,i

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat)) 

model2_predictions <- mu_hat + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model2_rmse <- RMSE(validation$rating, model2_predictions)

model2_rmse # --> RMSE of Model 2.0: 0.9439087

## Third step of the model. We define the following:
## User effect = bu
## Model 3.0: Yu,i = mu_hat + bi + bu + Eu,i

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

model3_predictions <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

model3_rmse <- RMSE(validation$rating, model3_predictions)

model3_rmse # --> RMSE of Model 3.0: 0.8653488

## Fourth step of the model. We define the following:
## Genre effect = bg
## Model 4.0: Yu,i = mu_hat + bi + bu + bg + Eu,i

genre_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))

model4_predictions <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)

model4_rmse <- RMSE(validation$rating, model4_predictions)

model4_rmse # --> RMSE of Model 4.0: 0.8649469

## Fifth step of the model. We define the following:
## Release year effect = by
## Model 5.0: Yu,i = mu_hat + bi + bu + bg + by + Eu,i

year_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(movieYear) %>%
  summarize(b_y = mean(rating - mu_hat - b_i - b_u - b_g))

model5_predictions <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(year_avgs, by = 'movieYear') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y) %>%
  pull(pred)

model5_rmse <- RMSE(validation$rating, model5_predictions)

model5_rmse # --> RMSE of Model 5.0: 0.8647606

## Sixth step of the model. We define the following:
## Penalty term lambda = l
## The penalty term reduces the importance of rare observations and can be applied 
## to all the effects bi, bu, bg, by
## Number of movies = n_i
## Number of users = n_u
## Number of genres = n_g
## Number of release years = n_y
## bi(l) =  1 / (lambda + n_i) * sum(on all n_i ratings)(Yu,i - mu_hat)
## bu(l) =  1 / (lambda + n_u) * sum(on all n_u ratings)(Yu,i - mu_hat - bi(l))
## bg(l) =  1 / (lambda + n_g) * sum(on all n_g ratings)(Yu,i - mu_hat - bi(l) - bu(l))
## by(l) =  1 / (lambda + n_y) * sum(on all n_y ratings)(Yu,i - mu_hat - bi(l) - bu(l) - bg(l))
## We perform a tuning parameter session, in order to find the penalty term l that minimizes the RMSE. 

l <- seq(0, 10, 0.25)

## The following code chunk will run in around 10 minutes

rmses <- sapply(l, function(lambda){
  
  mu_hat <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_hat - b_i)/(n()+lambda))
  
  b_g <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu_hat - b_i - b_u)/(n()+lambda))
  
  b_y <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(movieYear) %>%
    summarize(b_y = sum(rating - mu_hat - b_i - b_u - b_g)/(n()+lambda))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "movieYear") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_y) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

l_best <- l[which.min(rmses)]

l_best # --> l = 5 is the penalty term that minimizes the RMSE

lambdas <- data.frame(l, rmses)

ggplot(data = lambdas, aes(x = l, y = rmses)) +
  geom_point() +
  xlab("Lambda value")+
  ylab("RMSE") +
  ggtitle("RMSE values depending on Penalty term lambda")

## Model 6.0: Yu,i = mu_hat + bi(l) + bu(l) + bg(l) + by(l) + Eu,i 
## Having the penalty term l = l_best = 5
## This is our final model

b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l_best))
  
b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu_hat - b_i)/(n()+l_best))
  
b_g <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu_hat - b_i - b_u)/(n()+l_best))
  
b_y <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(movieYear) %>%
    summarize(b_y = sum(rating - mu_hat - b_i - b_u - b_g)/(n()+l_best))
  
model6_predictions <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "movieYear") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_y) %>%
    pull(pred)
  
model6_rmse <- RMSE(validation$rating, model6_predictions)

model6_rmse # --> RMSE of Model 6.0: 0.8642929

## Model results
## We embed the RMSEs of the various models into a single table

models <- c("Model 1.0 - Yu,i = mu_hat + Eu,i",
            "Model 2.0 - Yu,i = mu_hat + bi + Eu,i",
            "Model 3.0 - Yu,i = mu_hat + bi + bu + Eu,i",
            "Model 4.0 - Yu,i = mu_hat + bi + bu + bg + Eu,i",
            "Model 5.0 - Yu,i = mu_hat + bi + bu + bg + by + Eu,i",
            "Model 6.0 - Yu,i = mu_hat + bi(l) + bu(l) + bg(l) + by(l) + Eu,i")

rmse_models <- c(model1_rmse,
                 model2_rmse,
                 model3_rmse,
                 model4_rmse,
                 model5_rmse,
                 model6_rmse)

results <- data.frame(models = models, RMSE = rmse_models) 

results

