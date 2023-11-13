# This script trains the models, generates the predictions
# and calculates the RMSE according to the companion report.
# ----------------------------------------------------------

# Load library and if not exist load packages
if(!require(tidyverse)) install.packages("tidyverse")
library(readxl) # part of tidyverse, still need to load explicitly
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(randomForest)) install.packages("randomForest")
if(!require(Rborist)) install.packages("Rborist")


# Load dataset from file located at relative path from project folder
# and correct original name typo from PE to EP (Electrical Power)
df <- read_xlsx("./data/Folds5x2_pp.xlsx",
                col_names=c("AT", "V", "AP", "RH", "EP"), 
                skip=1)

# Error function
RMSE <- function(actual_outcomes, predictions){
  sqrt(mean((actual_outcomes - predictions)^2))
}

# Split dataset into train and test sets
set.seed(1972)
test_index <- createDataPartition(y = df$EP, times = 1, p = 0.2, list = FALSE)
df_train <- df[-test_index,]
df_test <- df[test_index,]

# Removing RH from features
df_train_no_RH <- df_train %>% select(AT, V, AP, EP)


# linear regression
fit_lm <- train(EP ~ ., data = df_train, method = 'lm')

# knn
fit_knn <- train(EP ~ ., data = df_train, method = 'knn',
             tuneGrid = data.frame(k = seq(5, 15, 2)))

# knn on 3 features
fit_knn_no_RH <- train(EP ~ ., data = df_train_no_RH, method = 'knn',
                 tuneGrid = data.frame(k = seq(5, 15, 2)))

# Random Forest method rf on 3 features
fit_rf_no_RH <- train(EP ~ ., data = df_train_no_RH, method = 'rf')

# Random Forest method Rborist on 3 features
fit_rborist_no_RH <- train(EP ~ ., data = df_train_no_RH, method = 'Rborist')



# Print rmse for all fitting 
for (model in c('fit_lm', 'fit_knn', 'fit_knn_no_RH', 'fit_rf_no_RH', 'fit_rborist_no_RH')) {
  predictions  <- predict(get(model), df_test)
  a <- switch(model, 'fit_lm'= '- linear regression on 4 features', 
                     'fit_knn'= '- Knn on 4 features', 
                     'fit_knn_no_RH'= '- Knn on 3 features', 
                     'fit_rf_no_RH'= '- rf Random Forest on 3 features', 
                     'fit_rborist_no_RH'= '- Rborist Random Forest on 3 features')
  
  cat('rmse =', round(RMSE(df_test$EP,  predictions),2), a,  '\n')
}
