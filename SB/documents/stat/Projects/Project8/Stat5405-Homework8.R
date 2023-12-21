caesarian=read.csv('data/caesarian.csv')
head(caesarian)

caesarian$delivery_number <- as.factor(caesarian$delivery_number)
caesarian$delivery_time <- as.factor(caesarian$delivery_time)
caesarian$blood_pressure <- as.factor(caesarian$blood_pressure)
caesarian$heart_problem <- as.factor(caesarian$heart_problem)
caesarian$Caesarian <- as.factor(caesarian$Caesarian)


set.seed(123457)
train.prop <- 0.80
strats <- caesarian$Caesarian
rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, 
                                     function(x) sample(x, length(x)*train.prop)))))
caesarian.train <- caesarian[idx, ]
caesarian.test <- caesarian[-idx, ]


summary(caesarian.train$Caesarian)/nrow(caesarian.train)
summary(caesarian.test$Caesarian)/nrow(caesarian.test)
summary(caesarian$Caesarian)/nrow(caesarian)

model_age.logit <- glm(Caesarian ~ age ,data = caesarian.train, 
                       family = binomial(link = "logit"))
summary(model_age.logit)

final_model <- step(
  model_age.logit,
  scope = list(
    lower = ~ age,
    upper =  ~ age + delivery_number + delivery_time + blood_pressure + heart_problem
  ),
  direction = "forward"
)
#formula(final_model)
summary(final_model)


full.logit <- glm(Caesarian ~ . ,data = caesarian.train, 
                  family = binomial(link = "logit"))
summary(full.logit)



with(full.logit, cbind(deviance = deviance, df = df.residual,
                       p = pchisq(deviance, df.residual, lower.tail=FALSE)))


fitted_values<-ifelse(fitted(full.logit)>0.65,1,0)
confusion_matrix <- table(fitted_values, caesarian.train$Caesarian)
print(confusion_matrix)


TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[2, 1]
FN <- confusion_matrix[1, 2]

Total <- sum(confusion_matrix)

accuracy <- (TP + TN) / Total

sensitivity <- TP / (TP + FN)

specificity <- TN / (TN + FP)


cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")



predicted_values<-ifelse(predict(full.logit,newdata = caesarian.test, type = 'response')>0.65,1,0)
confusion_matrix_test <- table(predicted_values, caesarian.test$Caesarian)
print(confusion_matrix_test)



TP <- confusion_matrix_test[2, 2]
TN <- confusion_matrix_test[1, 1]
FP <- confusion_matrix_test[2, 1]
FN <- confusion_matrix_test[1, 2]

Total <- sum(confusion_matrix_test)

accuracy <- (TP + TN) / Total

sensitivity <- TP / (TP + FN)

specificity <- TN / (TN + FP)


cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")


leveefailure=read.csv("data/leveefailure.csv")
leveefailure <- subset(leveefailure, select = -X)

head(leveefailure)


leveefailure$Landcover <- as.factor(leveefailure$Landcover)
leveefailure$Meander <- as.factor(leveefailure$Meander)
leveefailure$Borrowpit <- as.factor(leveefailure$Borrowpit)
leveefailure$Sediments <- as.factor(leveefailure$Sediments)
leveefailure$Failure <- as.factor(leveefailure$Failure)
leveefailure$Revetment <- as.factor(leveefailure$Revetment)


set.seed(123457)
train.prop <- 0.80
strats <- leveefailure$Failure
rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, 
                                     function(x) sample(x, length(x)*train.prop)))))
leveefailure.train <- leveefailure[idx, ]
leveefailure.test <- leveefailure[-idx, ]



summary(leveefailure.train$Failure)/nrow(leveefailure.train)


levee.full.logit <- glm(Failure ~ .  ,data = leveefailure.train, 
                        family = binomial(link = "logit"))
summary(levee.full.logit)



levee.null.logit <- glm(Failure~1, data = leveefailure.train, 
                        family = binomial(link = "logit"))
summary(levee.null.logit)



both.logit <-
  step(
    levee.null.logit,
    list(
      lower = formula(levee.null.logit),
      upper = formula(levee.full.logit)
    ),
    direction = "both",
    trace = 0,
    data = leveefailure.train
  )
formula(both.logit)

levee.both.logit = glm(Failure~Sinuosity + ChannelWidth + Floodwaywidth + Meander , data=leveefailure.train,family = binomial(link = "logit"))
summary(levee.both.logit)


pred.both <- predict(levee.both.logit, newdata = leveefailure.test, type="response")
pred.full <- predict(levee.full.logit, newdata = leveefailure.test, type="response")


library(pROC)
roc.both <- roc(leveefailure.test$Failure, pred.both, levels=c(1,0))
roc.both

backwards <- step(levee.full.logit, trace = 0)  #suppress details of each iteration
formula(backwards)

roc.back <- roc(leveefailure.test$Failure, pred.back, levels=c(1,0))
roc.back



























