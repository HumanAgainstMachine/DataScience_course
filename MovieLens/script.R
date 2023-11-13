# This script generates the predictions for movie ratings and calculates 
# the RMSE according to the best finding described in the report.
# ----------------------------------------------------------------------


# Load packages and install if not exist
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")

# Load edx and validation sets
load("./data/edx&validation_sets.RData")

RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))
}

create_train_and_test_sets <- function(seed=1993){
  ## Create train and test sets from edx set and assign
  ## them to global variables train_set & test_set
  
  set.seed(seed, sample.kind = "Rounding")
  
  test_index <- 
    createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
  train_set <<- edx[-test_index]
  test_set <<- edx[test_index]
  
  test_set <<- test_set %>%
    semi_join(train_set, by = 'movieId') %>%
    semi_join(train_set, by = 'userId')
}


# cross-validation
create_train_and_test_sets(1861)

mu_train <- mean(train_set$rating)
lambdas <- seq(4.5, 5.5, 0.1)

# tuning of lambda
rmses <- sapply(lambdas, function(l){
  
  bi_df <- train_set %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu_train)/(n()+l))
  
  bu_df <- train_set %>% 
    left_join(bi_df, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu_train)/(n()+l))
  
  predictions <- test_set %>%
    left_join(bi_df, by = 'movieId') %>%
    left_join(bu_df, by = 'userId') %>%
    mutate(pred = mu_train + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predictions, test_set$rating))
})


best_lambda <- lambdas[which.min(rmses)]

mu_hat <- mean(edx$rating)

# penalized least squares estimate of movie effect b_is
penalized_bi_df <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_hat)/(n()+best_lambda))

# penalized least squares estimate of user effect b_us
penalized_bu_df <- edx %>%
  left_join(penalized_bi_df, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu_hat - b_i)/(n()+best_lambda))

# NOT penalized least square estimates of genres-combinations effect
bz_df <- edx %>%
  left_join(penalized_bi_df, by = "movieId") %>%
  left_join(penalized_bu_df, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_z = mean(rating - mu_hat - b_i - b_u))

# predictions
predictions <- validation %>%
  left_join(penalized_bi_df, by='movieId') %>%
  left_join(penalized_bu_df, by='userId') %>%
  left_join(bz_df, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_z) %>%
  pull(pred)

# assessment
rmse <- RMSE(predictions, validation$rating)

print(rmse)