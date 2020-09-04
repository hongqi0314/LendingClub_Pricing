## Feature importance ranking using xgboost

#### 0. Load required packages and data preparation ----
### 0.1 Load packages ----
library(xgboost)
library(rBayesianOptimization)

### 0.2 Change data format to meet xgboost requirement ----
lc_xgb_data <- lc_data_wDummies %>%
  dplyr::select(-id) %>% 
  as.matrix()

xgb_colnames <- colnames(lc_xgb_data)

### 0.3 Stratified split data into train and test ----
set.seed(314817)
in_train <- 
  caret::createDataPartition(lc_data_wDummies[['loan_status']], 
                             p = 0.8, 
                             list = FALSE)
lc_xgb_train <- lc_xgb_data[in_train, ]
lc_xgb_test <- lc_xgb_data[-in_train, ]
lc_xgb_train_sample <- 
  lc_xgb_train[sample(1:nrow(lc_xgb_train), size = 50000, replace = FALSE), ]

### 0.4 xgb.cv using default hyper-parameters ----
lc_param <- list(
    'objective' = 'binary:logistic'
  , 'eta' = 0.01
  , 'max_depth' = 3
  , 'min_child_weight' = 1
  , 'colsample_bytree' = 1
  , 'subsample' = 1
  , 'gamma' = 0
  , 'seed' = 314817
)

lc_xgb_cv <- xgb.cv(
    params = lc_param 
  , data = lc_xgb_train_sample[, xgb_colnames != "loan_status"]
  , label = lc_xgb_train_sample[, "loan_status"]
  , nfold = 5
  , nrounds = 5000
  , early_stopping_rounds = 150
  , verbose = TRUE
  , stratified = TRUE
  , prediction = TRUE
  , metrics = 'auc'
)
