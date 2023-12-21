library(rpart)
data(kyphosis)
head(kyphosis)

set.seed(123457)
train.prop <- 0.90

strats <- kyphosis$Kyphosis
rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, 
                                     function(x) sample(x, length(x)*train.prop)))))
kyphosis.train <- kyphosis[idx, ]
kyphosis.test <- kyphosis[-idx, ]

table(kyphosis$Kyphosis)
summary(kyphosis$Kyphosis)/nrow(kyphosis)

table(kyphosis.train$Kyphosis)
summary(kyphosis.train$Kyphosis)/nrow(kyphosis.train)

table(kyphosis.test$Kyphosis)
summary(kyphosis.test$Kyphosis)/nrow(kyphosis.test)

model.full.logit <- glm(Kyphosis ~ . ,data = kyphosis.train, 
                        family = binomial(link = "logit"))
summary(model.full.logit)

model.null.logit <- glm(Kyphosis ~ 1 ,data = kyphosis.train, 
                        family = binomial(link = "logit"))
summary(model.null.logit)

with(model.full.logit, cbind(deviance = deviance, df = df.residual,
                             p = pchisq(deviance, df.residual, lower.tail=FALSE)))

library(pROC)

pred.full <- predict(model.full.logit, newdata = kyphosis.train, type="response")
pred.null <- predict(model.null.logit, newdata = kyphosis.train, type="response")

roc.full <- roc(kyphosis.train$Kyphosis, pred.full, levels=c('absent','present'))
roc.full

roc.null <- roc(kyphosis.train$Kyphosis, pred.null, levels=c('absent','present'))
roc.null


pred.full <- predict(model.full.logit, newdata = kyphosis.test, type="response")
pred.null <- predict(model.null.logit, newdata = kyphosis.test, type="response")

roc.full <- roc(kyphosis.test$Kyphosis, pred.full, levels=c('absent','present'))
roc.full

roc.null <- roc(kyphosis.test$Kyphosis, pred.null, levels=c('absent','present'))
roc.null


age_coef <- coef(model.full.logit)["Age"]
age_coef

beta_age <- 0.009606
se_age <- 0.007057

# Calculate the Wald test statistic
wald_test_statistic <- beta_age / se_age

# Calculate the p-value for the test statistic
# Note: This should be a two-tailed test since we're testing for inequality in the hypothesis
p_value <- 2 * (1 - pnorm(abs(wald_test_statistic)))

# Output the Wald test statistic and p-value
list(wald_test_statistic = wald_test_statistic, p_value = p_value)


model.reduced.logit <- glm(Kyphosis ~ Age, data=kyphosis.train, family = binomial(link = "logit"))
summary(model.reduced.logit)

full_model_predictions <- predict(model.full.logit, newdata = kyphosis.test, type = "response")
reduced_model_predictions <- predict(model.reduced.logit, newdata = kyphosis.test, type = "response")

# Convert predictions to binary outcomes based on a 0.5 cutoff
full_model_pred_class <- ifelse(full_model_predictions > 0.5, "present", "absent")
reduced_model_pred_class <- ifelse(reduced_model_predictions > 0.5, "present", "absent")

# Calculate the accuracy for both models
full_model_accuracy <- mean(full_model_pred_class == kyphosis.test$Kyphosis)
reduced_model_accuracy <- mean(reduced_model_pred_class == kyphosis.test$Kyphosis)

full_model_accuracy
reduced_model_accuracy

roc_full <- roc(kyphosis.test$Kyphosis, full_model_predictions)
auc_full <- auc(roc_full)
auc_full
# ROC curve for the reduced model
roc_reduced <- roc(kyphosis.test$Kyphosis, reduced_model_predictions)
auc_reduced <- auc(roc_reduced)
auc_reduced

library(rpart)
tree_model <- rpart(Kyphosis ~ ., data = kyphosis.train,control = rpart.control(minsplit = 1, cp = 0.0001))

printcp(tree_model) 

(cp= tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"])
(xerr = tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "xerror"])

library(rpart.plot)
rpart.plot(tree_model, extra = "auto")

library(caret)
sensitivity(conf_matrix_base)
specificity(conf_matrix_base)
(mis.rate <- conf_matrix_base[1, 2] + 
    conf_matrix_base[2, 1])/sum(conf_matrix_base) 

prune_tree_model <- prune(tree_model, cp =
                            tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"])
rpart.plot(prune_tree_model, extra = "auto")

pred <- predict(prune_tree_model, newdata = kyphosis.test, type = "class")
(conf_matrix_base <- table(kyphosis.test$Kyphosis, pred)) #confusion matrix

library(ranger)

fit.rf.ranger <- ranger(Kyphosis ~ ., data = kyphosis.train, 
                        importance = 'impurity', mtry = 3)
print(fit.rf.ranger)

pred <- predict(fit.rf.ranger, data = kyphosis.test)
test_df <- data.frame(actual = kyphosis.test$Kyphosis, pred = NA)
test_df$pred <- pred$predictions
(conf_matrix_rf <- table(test_df$actual, test_df$pred)) 

library(randomForestSRC)

# Measure time for the ranger package

system.time({
  rf_ranger <- ranger(Kyphosis ~ ., data = kyphosis.train, probability = TRUE)
})

# Measure time for the randomForestSRC package
system.time({
  rf_randomForestSRC <- rfsrc(Kyphosis ~ ., data = kyphosis.train)
})

library(xgboost)
library(Matrix)

kyphosis.train$Kyphosis <- ifelse(kyphosis.train$Kyphosis=='present',1,0)
kyphosis.test$Kyphosis <- ifelse(kyphosis.test$Kyphosis=='present',1,0)


# Transform the predictor matrix using dummy (or indictor or one-hot) encoding 
matrix_predictors.train <- 
  as.matrix(sparse.model.matrix(Kyphosis ~., data = kyphosis.train))[, -1]
matrix_predictors.test <- 
  as.matrix(sparse.model.matrix(Kyphosis ~., data = kyphosis.test))[, -1]


# Train dataset
pred.train.gbm <- data.matrix(matrix_predictors.train) # predictors only

#convert factor to numeric
kyphosis.train.gbm <- as.numeric(as.character(kyphosis.train$Kyphosis)) 

dtrain <- xgb.DMatrix(data = pred.train.gbm, label = kyphosis.train.gbm)

# Test dataset
pred.test.gbm <- data.matrix(matrix_predictors.test) # predictors only

#convert factor to numeric
kyphosis.test.gbm <- as.numeric(as.character(kyphosis.test$Kyphosis))
dtest <- xgb.DMatrix(data = pred.test.gbm, label = kyphosis.test.gbm)

watchlist <- list(train = dtrain, test = dtest)
param <- list(max_depth = 2, eta = 1, nthread = 2,
              objective = "binary:logistic", eval_metric = "auc")

pred.y.train <- predict(model.xgb, pred.train.gbm)
prediction.train <- as.numeric(pred.y.train > 0.5)
# Measure prediction accuracy on train data
(tab<-table(kyphosis.train.gbm, prediction.train))

# Convert training and test data to DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(kyphosis.train[, -1]), label = kyphosis.train$Kyphosis == "1")
dtest <- xgb.DMatrix(data = as.matrix(kyphosis.test[, -1]), label = kyphosis.test$Kyphosis == "1")

# Set parameters for XGBoost
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc"
)

# Train the XGBoost model
xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)

# Predict on the test data
xgb_predictions_prob <- predict(xgb_model, dtest)
xgb_predictions_class <- ifelse(xgb_predictions_prob > 0.5, "present", "absent")

# Validate the performance
xgb_accuracy <- mean(xgb_predictions_class == kyphosis.test$Kyphosis)
xgb_confusion_matrix <- table(Predicted = xgb_predictions_class, Actual = kyphosis.test$Kyphosis)

# Output the accuracy and confusion matrix
list(accuracy = xgb_accuracy, confusion_matrix = xgb_confusion_matrix)
