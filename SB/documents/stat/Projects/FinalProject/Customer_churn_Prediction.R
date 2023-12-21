library(ggplot2)
library(pROC)
library(Metrics)
library(rpart)
library(caret)
library(rpart.plot)
library(ranger)
library(vip)
library(xgboost)
library(Matrix)

#Reading the Data
bank_data = read.csv('data/Bank Customer Churn Prediction.csv')
str(bank_data)

#Data Cleaning
bank_data = subset(bank_data,select = -customer_id)
head(bank_data,1)

#Null check
null_count <- colSums(is.na(bank_data))
print(null_count)

par(mfrow = c(2, 2))

hist(bank_data$balance, main = "Histogram of Balance", xlab = "Balance", ylab = "Frequency")
hist(bank_data$credit_score, main = "Histogram of Credit Score", xlab = "Credit Score", ylab = "Frequency")
hist(bank_data$estimated_salary, main = "Histogram of Estimated Salary", xlab = "Estimated Salary", ylab = "Frequency")
hist(bank_data$age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency")


# Frequency tables
country_frequency <- table(bank_data$country)
gender_frequency <- table(bank_data$gender)


# Bar plots
# For Country
ggplot(bank_data, aes(x=country)) + 
  geom_bar(fill="steelblue") + 
  labs(title="Bar Plot of Country", x="Country", y="Count")

# For Gender
ggplot(bank_data, aes(x=gender)) + 
  geom_bar(fill="pink") + 
  labs(title="Bar Plot of Gender", x="Gender", y="Count")

dim(bank_data)

table(bank_data$churn)

table(bank_data$churn)/nrow(bank_data)

# Data pre-Processing
col_class <- sapply(1:ncol(bank_data), function(x) class(bank_data[,x]))
col_id <- which(col_class == "character")
for(i in 1:length(col_id)){
  bank_data[,col_id[i]] <- as.factor(bank_data[,col_id[i]])
}

#Train-Test Split

set.seed(123457)
train.prop <- 0.80
strats <- bank_data$churn
rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, 
                                     function(x) sample(x, length(x)*train.prop)))))
bank_data.train <- bank_data[idx, ]
bank_data.test <- bank_data[-idx, ]

#Checking Proportions
table(bank_data.train$churn)/nrow(bank_data.train)
table(bank_data.test$churn)/nrow(bank_data.test)
table(bank_data$churn)/nrow(bank_data)

#Binary Logit Modelling

# Full Model
full.logit <- glm(churn ~ . ,data = bank_data.train, 
                  family = binomial(link = "logit"))
summary(full.logit)
BIC(full.logit)

# Null Model

null.logit <- glm(churn~1, data = bank_data.train, 
                  family = binomial(link = "logit"))
summary(null.logit)
BIC(null.logit)

#Variable Selection
both.logit <- step(null.logit, list(lower = formula(null.logit),
                                    upper = formula(full.logit)),
                   direction = "both", trace = 0, data = bank.data.train)
formula(both.logit)
summary(both.logit)
BIC(both.logit)

#Performance on test Data
pred.both <- predict(both.logit, newdata = bank_data.test, type = "response")
pred.full <- predict(full.logit, newdata = bank_data.test, type = "response")

(table.both <- table(ifelse(pred.both > 0.5,1,0), bank_data.test$churn))
(table.full <- table(ifelse(pred.full > 0.5,1,0), bank_data.test$churn))

(accuracy.both <- round((sum(diag(table.both))/sum(table.both))*100, 3)) 
(accuracy.full <- round((sum(diag(table.full))/sum(table.full))*100, 3))

par(mfrow = c(1,2))
roc.both <- roc(bank_data.test$churn ~ pred.both, plot = TRUE, 
                legacy.axes = TRUE, print.auc = TRUE)

roc.full <- roc(bank_data.test$churn ~ pred.full, plot = TRUE, 
                legacy.axes = TRUE, print.auc = TRUE)


print("For Both Model:")
accuracy.both <- round((sum(diag(table.both))/sum(table.both))*100, 3)

precision_val <- table.both[2, 2] / (table.both[2, 2] + conf_matrix_base[2, 1])
recall_val <-table.both[2, 2] / (table.both[2, 2] + table.both[1, 2])
F1_both <- 2 * (precision_val * recall_val) / (precision_val + recall_val)


paste("Accuracy: ",accuracy.both)
paste("precision: ",precision_val)
paste("Recall:",recall_val)
paste("F1: ",F1_both)

print("For Full Model:")
accuracy.full <- round((sum(diag(table.full))/sum(table.full))*100, 3)
precision_val_full  <- table.full[2, 2] / (table.full[2, 2] + table.full[2, 1])
recall_val_full <-table.full[2, 2] / (table.full[2, 2] + table.full[1, 2])
F1_full <- 2 * (precision_val_full * recall_val_full) / (precision_val_full + recall_val_full)

paste("Accuracy: ",accuracy.full)
paste("precision: ",precision_val_full)
paste("Recall: ",recall_val_full)
paste("F1: ",F1_full)


#Probit Model

both.probit <- glm(churn ~ age + active_member + country + gender + balance + credit_score + products_number ,data = bank_data.train , 
                   family = binomial(link = "probit"))
summary(both.probit)

#Probit model Performance

pred.pboth <- predict(both.probit, newdata = bank_data.test, type = "response")
(table.pboth <- table(ifelse(pred.pboth > 0.5,1,0), bank_data.test$churn))
accuracy.pboth <- round((sum(diag(table.pboth))/sum(table.pboth))*100, 3)

print("For Probit both Model:")

paste("Accuracy: ",accuracy.pboth)

precision_val_full  <- table.pboth[2, 2] / (table.pboth[2, 2] + table.pboth[2, 1])
recall_val_full <-table.pboth[2, 2] / (table.pboth[2, 2] + table.pboth[1, 2])
F1_full <- 2 * (precision_val_full * recall_val_full) / (precision_val_full + recall_val_full)

paste("precision: ",precision_val_pboth)
paste("Recall: ",recall_val_pboth)
paste("F1: ",F1_pboth)

#Decision trees

# Fitting full binary tree
fit.allp <- rpart(churn ~., method = "class", data = bank_data.train,
                  control = rpart.control(minsplit = 1, cp = 0.001))
printcp(fit.allp) 

(cp= fit.allp$cptable[which.min(fit.allp$cptable[, "xerror"]), "CP"])
(xerr = fit.allp$cptable[which.min(fit.allp$cptable[, "xerror"]), "xerror"])

plotcp(fit.allp)

data.frame(fit.allp$variable.importance)

test_df <- data.frame(actual = bank_data.test$churn, pred = NA)
test_df$pred <- predict(fit.allp, newdata = bank_data.test, type = "class")
(conf_matrix_base <- table(test_df$pred, test_df$actual)) #confusion matrix

precision_dtfull <- precision <- conf_matrix_base[2, 2] / (conf_matrix_base[2, 2] + conf_matrix_base[2, 1])
recall_dtfull <-conf_matrix_base[2, 2] / (conf_matrix_base[2, 2] + conf_matrix_base[1, 2])
accuracy_dtfull <- round((sum(diag(conf_matrix_base))/sum(conf_matrix_base))*100, 3)
F1_dtfull <- 2 * (precision_dtfull * recall_dtfull) / (precision_dtfull + recall_dtfull)

print("For Full Decision Tree Model:")


paste("Accuracy: ",accuracy_dtfull)
paste("precision: ",precision_dtfull)
paste("Recall: ",recall_dtfull)
paste("F1: ",F1_dtfull)

# Pruning the Decision Tree
pfit.allp <- prune(fit.allp, cp =
                     fit.allp$cptable[which.min(fit.allp$cptable[, "xerror"]), "CP"])
printcp(pfit.allp)

rpart.plot(pfit.allp, extra = "auto")

#Performance of the pruned tree
test_df <- data.frame(actual = bank_data.test$churn, pred = NA)
test_df$pred <- predict(pfit.allp, newdata = bank_data.test, type = "class")
(conf_matrix_base_prune <- table(test_df$pred, test_df$actual)) #confusion matrix

precision_dtprune <- precision <- conf_matrix_base_prune[2, 2] / (conf_matrix_base_prune[2, 2] + conf_matrix_base_prune[2, 1])
recall_dtprune <-conf_matrix_base_prune[2, 2] / (conf_matrix_base_prune[2, 2] + conf_matrix_base_prune[1, 2])
accuracy_dtprune <- round((sum(diag(conf_matrix_base_prune))/sum(conf_matrix_base_prune))*100, 3)
F1_dtprune <- 2 * (precision_dtprune * recall_dtprune) / (precision_dtprune + recall_dtprune)

print("For Pruned Decision Tree Model:")


paste("Accuracy: ",accuracy_dtprune)
paste("precision: ",precision_dtprune)
paste("Recall: ",recall_dtprune)
paste("F1: ",F1_dtprune)

#Ensemble Techniques
#Random Forest

fit.rf.ranger <- ranger(factor(churn) ~ ., data = bank_data.train, 
                        importance = 'impurity', mtry = 3)
print(fit.rf.ranger)
v1 <- vi(fit.rf.ranger)
vip(v1)
fit.rf.ranger.reduced <- ranger(factor(churn) ~ age+estimated_salary+balance+credit_score+products_number+tenure+active_member+country, data = bank_data.train, 
                                importance = 'impurity', mtry = 3)
print(fit.rf.ranger.reduced)

pred <- predict(fit.rf.ranger.reduced, data = bank_data.test)
test_df_rf <- data.frame(actual = bank_data.test$churn, pred = NA)
test_df_rf$pred <- pred$predictions
(conf_matrix_rf <- table(test_df_rf$pred, test_df_rf$actual)) #confusion matrix

precision_dtprune <- precision <- conf_matrix_rf[2, 2] / (conf_matrix_rf[2, 2] + conf_matrix_rf[2, 1])
recall_dtprune <-conf_matrix_rf[2, 2] / (conf_matrix_rf[2, 2] + conf_matrix_rf[1, 2])
accuracy_dtprune <- round((sum(diag(conf_matrix_rf))/sum(conf_matrix_rf))*100, 3)
F1_dtprune <- 2 * (precision_dtprune * recall_dtprune) / (precision_dtprune + recall_dtprune)

print("For Random Forest Model:")


paste("Accuracy: ",accuracy_dtprune)
paste("precision: ",precision_dtprune)
paste("Recall: ",recall_dtprune)
paste("F1: ",F1_dtprune)


#Gradient Boosting


# Transform the predictor matrix using dummy (or indicator or one-hot) encoding 
matrix_predictors.train <- 
  as.matrix(sparse.model.matrix(churn ~., data = bank_data.train))[, -1]
matrix_predictors.test <- 
  as.matrix(sparse.model.matrix(churn ~., data = bank_data.test))[, -1]

# Train dataset
pred.train.gbm <- data.matrix(matrix_predictors.train) # predictors only
#convert factor to numeric
bank.data.train.gbm <- as.numeric(as.character(bank_data.train$churn)) 
dtrain <- xgb.DMatrix(data = pred.train.gbm, label = bank.data.train.gbm)
# Test dataset
pred.test.gbm <- data.matrix(matrix_predictors.test) # predictors only
#convert factor to numeric
bank.data.test.gbm <- as.numeric(as.character(bank_data.test$churn))
dtest <- xgb.DMatrix(data = pred.test.gbm, label = bank.data.test.gbm)

watchlist <- list(train = dtrain, test = dtest)
param <- list(max_depth = 2, eta = 1, nthread = 2,
              objective = "binary:logistic", eval_metric = "auc")

model.xgb <- xgb.train(param, dtrain, nrounds = 10, watchlist)
summary(model.xgb)

pred.y = predict(model.xgb, pred.test.gbm)
prediction <- as.numeric(pred.y > 0.5)
# Measure prediction accuracy on test data
conf_matrix_xgb<-table(bank.data.test.gbm,prediction)

precision_dtprune <- precision <- conf_matrix_xgb[2, 2] / (conf_matrix_xgb[2, 2] + conf_matrix_xgb[1, 2])
recall_dtprune <-conf_matrix_xgb[2, 2] / (conf_matrix_xgb[2, 2] + conf_matrix_xgb[2, 1])
accuracy_dtprune <- round((sum(diag(conf_matrix_xgb))/sum(conf_matrix_xgb))*100, 3)
F1_dtprune <- 2 * (precision_dtprune * recall_dtprune) / (precision_dtprune + recall_dtprune)

print("For XGB Model:")


paste("Accuracy: ",accuracy_dtprune)
paste("precision: ",precision_dtprune)
paste("Recall: ",recall_dtprune)
paste("F1: ",F1_dtprune)
