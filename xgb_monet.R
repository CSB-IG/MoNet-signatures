library(caret) # Machine Learning Library
library(dplyr) # Data Wrangling
library(ggplo2) # Data visualization
library(xgboost) # XGBoost algorithm and utils
library(Ckmeans.1d.dp) # Feature Importance

set.seed(42)
# M.t and df.umap.mclust are objects from umap_gmm.R file
train <- cbind(M.t, df.umap.mclust$classification) %>% 
    as.data.frame()
colnames(train)[393737] <- "Classes"

## Train matrix
train_Dmatrix <- train %>%
    select(-c(393737)) %>%
    as.matrix() %>%
    xgb.DMatrix()

targets <- train$Classes

# Cross-validation
xgb_tr_control <- trainControl(
    method = "cv",
    number = 5,
    allowParallel = T,
    verboseIter = F,
    returnData = F
)

# Building parameters set
xgb_grid <- expand.grid(
  list(
    nrounds = seq(100, 200),
    max_depth = c(6, 15, 20), 
    colsample_bytree = 1, 
    eta = 0.02,
    gamma = 0,
    min_child_weight = 1,  
    subsample = 1)
)

### XGBoost training ###
xgb_model <- caret::train(
    train_Dmatrix,
    targets,
    trControl = xgb_tr_control,
    tuneGrid = xgb_grid,
    method = "xgbTree",
    nthread = 4
)

#xgb_model$bestTune

### Feature importance calculated by XGBoost ###
xgb_imp <- xgb.importance(
    feature_names = colnames(train %>% select(-c(393737))),
    model = xgb_model$finalModel
)

## Write the supplementary file
xgb_imp %>% write.csv("feature_importance.csv", row.names = F)

#### Visualize the feature importance ###
xgb.ggplot.importance(
    xgb_imp, 
    rel_to_first = F,
    top_n = 6
)