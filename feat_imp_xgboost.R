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
lc_xgb_test_sample <- 
  lc_xgb_test[sample(1:nrow(lc_xgb_test), size = 15000, replace = FALSE), ]

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

#### 1. Manual hyper-parameter tuning ----
### 1.1 "max_depth" ----
scores_max_depth <- c()

for (max_depth in c(3, 4, 5, 6)) {
  
  lc_param <- list(
      'objective' = 'binary:logistic'
    , 'eta' = 0.01
    , 'max_depth' = max_depth
    , 'min_child_weight' = 1
    , 'colsample_bytree' = 1
    , 'subsample' = 1
    , 'gamma' = 0
  )
  
  cv_results <- xgb.cv(
      params = lc_param 
    , data = lc_xgb_train_sample[, xgb_colnames != "loan_status"]
    , label = lc_xgb_train_sample[, "loan_status"]
    , nfold = 5
    , nrounds = 2500
    , early_stopping_rounds = 500
    , verbose = TRUE
    , stratified = TRUE
    , prediction = TRUE
    , metrics = 'auc'
    , seed = 12345
  )
  
  best_iteration <- cv_results$best_iteration
  best_score <- max(cv_results$evaluation_log[['test_auc_mean']])
  print(glue::glue("'max_depth': {max_depth} -- 
                    'best_iteration': {best_iteration} --
                    'best_score': {best_score}"))
  scores_max_depth <- 
    c(scores_max_depth, 
      best_score, 
      lc_param[['eta']], 
      lc_param[['max_depth']],
      lc_param[['min_child_weight']], 
      lc_param[['colsample_bytree']],
      lc_param[['subsample']], 
      lc_param[['gamma']], 
      best_iteration)
  
}

scores_max_depth_display <- 
  as.data.frame( matrix(scores_max_depth, ncol = 8, byrow = TRUE) )
names(scores_max_depth_display) <- 
  c('score', 'eta', 'max_depth', 'min_child_weight', 'colsample_bytree', 
    'subsample', 'gamma', 'best_iteration')
best_max_depth <- scores_max_depth_display %>%
  dplyr::top_n(1, score) %>%
  dplyr::pull(max_depth)

print(glue::glue("best 'max_depth' is {best_max_depth}."))

### 1.2 "min_child_weight" ----
scores_min_child_weight <- c()

for (min_child_weight in c(1, 10, 50, 100)) {
  
  lc_param <- list(
      'objective' = 'binary:logistic'
    , 'eta' = 0.01
    , 'max_depth' = best_max_depth
    , 'min_child_weight' = min_child_weight
    , 'colsample_bytree' = 1
    , 'subsample' = 1
    , 'gamma' = 0
  )
  
  cv_results <- xgb.cv(
      params = lc_param 
    , data = lc_xgb_train_sample[, xgb_colnames != "loan_status"]
    , label = lc_xgb_train_sample[, "loan_status"]
    , nfold = 5
    , nrounds = 2500
    , early_stopping_rounds = 500
    , verbose = TRUE
    , stratified = TRUE
    , prediction = TRUE
    , metrics = 'auc'
    , seed = 12345
  )
  
  best_iteration <- cv_results$best_iteration
  best_score <- max(cv_results$evaluation_log[['test_auc_mean']])
  print(glue("'min_child_weight': {min_child_weight} -- 
              'best_iteration': {best_iteration} --
              'best_score': {best_score}"))
  scores_min_child_weight <- 
    c(scores_min_child_weight, best_score, lc_param[['eta']], lc_param[['max_depth']],
      lc_param[['min_child_weight']], lc_param[['colsample_bytree']],
      lc_param[['subsample']], lc_param[['gamma']], best_iteration)
  
}

scores_min_child_weight_display <- 
  as.data.frame( matrix(scores_min_child_weight, ncol = 8, byrow = TRUE) )
names(scores_min_child_weight_display) <- 
  c('score', 'eta', 'max_depth', 'min_child_weight', 'colsample_bytree', 
    'subsample', 'gamma', 'best_iteration')
best_min_child_weight <- scores_min_child_weight_display %>%
  top_n(1, score) %>%
  pull(min_child_weight)

print(glue("best 'min_child_weight' is {best_min_child_weight}."))

### 1.3 "colsample_bytree" ----
scores_colsample_bytree <- c()

for (colsample_bytree in c(0.1, 0.3, 0.5, 0.7, 0.9)) {
  
  lc_param <- list(
      'objective' = 'binary:logistic'
    , 'eta' = 0.01
    , 'max_depth' = best_max_depth
    , 'min_child_weight' = best_min_child_weight
    , 'colsample_bytree' = colsample_bytree
    , 'subsample' = 1
    , 'gamma' = 0
  )
  
  cv_results <- xgb.cv(
      params = lc_param 
    , data = lc_xgb_train_sample[, xgb_colnames != "loan_status"]
    , label = lc_xgb_train_sample[, "loan_status"]
    , nfold = 5
    , nrounds = 2500
    , early_stopping_rounds = 500
    , verbose = TRUE
    , stratified = TRUE
    , prediction = TRUE
    , metrics = 'auc'
    , seed = 12345
  )
  
  best_iteration <- cv_results$best_iteration
  best_score <- max(cv_results$evaluation_log[['test_auc_mean']])
  print(glue("'colsample_bytree': {colsample_bytree} -- 
              'best_iteration': {best_iteration} --
              'best_score': {best_score}"))
  scores_colsample_bytree <- 
    c(scores_colsample_bytree, best_score, lc_param[['eta']], 
      lc_param[['max_depth']], lc_param[['min_child_weight']], 
      lc_param[['colsample_bytree']], lc_param[['subsample']], 
      lc_param[['gamma']], best_iteration)
  
}

scores_colsample_bytree_display <- 
  as.data.frame( matrix(scores_colsample_bytree, ncol = 8, byrow = TRUE) )
names(scores_colsample_bytree_display) <- 
  c('score', 'eta', 'max_depth', 'min_child_weight', 
    'colsample_bytree', 'subsample', 'gamma', 
    'best_iteration')
best_colsample_bytree <- scores_colsample_bytree_display %>%
  top_n(1, score) %>%
  pull(colsample_bytree)

print(glue("best 'colsample_bytree' is {best_colsample_bytree}."))


#### 3. Bayesian optimization ----
xgbDM_train <- 
  xgb.DMatrix(label = lc_xgb_train_sample[, 'loan_status'], 
              data = lc_xgb_train_sample[, xgb_colnames != 'loan_status'])
xgbDM_test <- 
  xgb.DMatrix(label = lc_xgb_test_sample[, 'loan_status'], 
              data = lc_xgb_test_sample[, xgb_colnames != 'loan_status'])
watchlist <- list(train = xgbDM_train, eval = xgbDM_test)

cv_folds <- 
  KFold(lc_xgb_train_sample[, 'loan_status'], 
        nfolds = 5, 
        stratified = TRUE, 
        seed = 314817)

xgb_cv_bayes <- function(max_depth, 
                         min_child_weight,
                         colsample_bytree,
                         subsample,
                         gamma) {
  lc_params = list(objective = "binary:logistic",
                   eta = 0.01,
                   max_depth = max_depth,
                   min_child_weight = min_child_weight,
                   colsample_bytree = colsample_bytree,
                   subsample = subsample,
                   gamma = gamma,
                   lambda = 1, 
                   alpha = 0)
  cv <- xgb.cv(params = lc_params,
               data = xgbDM_train, 
               seed = 12345,
               metrics = 'auc',
               nround = 1000,
               early_stopping_rounds = 100,
               folds = cv_folds, 
               prediction = TRUE, 
               showsd = TRUE,
               maximize = TRUE, 
               verbose = TRUE)
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
       Pred = cv$pred)
}

lc_bayes_opt <- 
  BayesianOptimization(xgb_cv_bayes,
                       bounds = list(max_depth = c(3L, 10L),
                                     min_child_weight = c(0L, 100L),    
                                     colsample_bytree = c(0.1, 0.7),
                                     subsample = c(0.7, 1),
                                     gamma = c(0, 2)),
                       init_grid_dt = NULL, 
                       init_points = 10, 
                       n_iter = 20, 
                       acq = "ucb", 
                       kappa = 2.576, 
                       eps = 0.0, 
                       verbose = TRUE)

saveRDS(lc_bayes_opt, here::here('RDS', 'lc_bayes_opt.RDS'))
