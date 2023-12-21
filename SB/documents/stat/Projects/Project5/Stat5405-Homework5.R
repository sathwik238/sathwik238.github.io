house_data=read.csv('data/kc_house_data.csv')
head(house_data)

plot(house_data$sqft_living,house_data$price, col='red')

plot(house_data$sqft_living, log(house_data$price))
plot(log(house_data$sqft_living), log(house_data$price))

cor(log10(house_data$price),house_data$sqft_living)

living_room_area<-house_data$sqft_living
log_price<- log10(house_data$price)
price<-house_data$price

SLM <-lm(log_price~living_room_area,data=house_data)
predicted <- fitted(SLM)
summary(SLM)

SLM_nolog <-lm(price~living_room_area,data=house_data)
summary(SLM_nolog)

# Residuals vs. Fitted Values Plot
plot(SLM, which = 1)

# Normal Q-Q Plot
plot(SLM, which = 2)

SLM_residuals<-residuals(SLM)
X<-house_data$sqft_living
y<-log10(house_data$price)
result_data <- data.frame(X, y, predicted, SLM_residuals)


studentized_residuals <- rstudent(SLM)

threshold <- 3

#Removing outliers
outlier_indices <- which(abs(studentized_residuals) > threshold)

cleaned_data <- result_data[-outlier_indices, ]

#refitting the model after removing the outliers
SLM_no_outliers <- lm(y ~ X, data = cleaned_data)

summary(SLM_no_outliers)

# Residuals vs. Fitted Values Plot
plot(SLM_no_outliers, which = 1)

# Residuals Normality check 
plot(SLM_no_outliers, which = 2)


summary(SLM_no_outliers)$coefficients

# Test the significance of the intercept (beta0)
summary(SLM_no_outliers)$coefficients["(Intercept)", "Pr(>|t|)"]
summary(SLM_no_outliers)$coefficients["X", "Pr(>|t|)"]


confint(SLM_no_outliers)


# Create a new data frame with the desired area value
new_data <- data.frame(X = 1500)

# Obtain predictions and 95% prediction intervals
predictions <- predict(SLM_no_outliers, newdata = new_data, interval = "prediction", level = 0.95)

# Print the results
print(predictions)


exp_predictions <- exp(predictions)
print(exp_predictions)

library(carData)
library(ggplot2)
library(car)

# Load the Duncan dataset
data("Duncan")
duncan_data=Duncan
head(duncan_data,5)


par(mfrow = c(1, 2))

plot(duncan_data$education, duncan_data$prestige, xlab = "Education", ylab = "Prestige", main = "Scatterplot of Prestige vs. Education")

abline(lm(prestige ~ education, data = duncan_data), col = "red")


plot(duncan_data$income, duncan_data$prestige, xlab = "Income", ylab = "Prestige", main = "Scatterplot of Prestige vs. Income")

abline(lm(prestige ~ income, data = duncan_data), col = "blue")

par(mfrow = c(1, 2))
boxplot(duncan_data$income~factor(duncan_data$type))
boxplot(duncan_data$prestige~factor(duncan_data$type))

model <- lm(prestige ~ education + income + factor(type), data = Duncan)

summary(model)

model_interaction <- lm(prestige ~ education * income * factor(type), data = Duncan)

summary(model_interaction)

res<-residuals(model)
plot(model,which=1)
plot(model,which=2)

confined_model=lm(prestige~education+income,data=duncan_data)
confined_residuals<-residuals(confined_model)
summary(confined_model)

confined_model_interaction=lm(prestige~education*income,data=duncan_data)
summary(confined_model_interaction)

plot(confined_model,which=1)
plot(confined_model,which=2)

y=residuals(lm(prestige~income,data=duncan_data))
x=residuals(lm(education~income,data=duncan_data))
plot(x,y)

avPlots(confined_model,terms = c('income'))

par(mfrow = c(1, 2))
crPlots(confined_model, terms = "education")
crPlots(confined_model, terms = "income")

summary(confined_model)$r.squared

