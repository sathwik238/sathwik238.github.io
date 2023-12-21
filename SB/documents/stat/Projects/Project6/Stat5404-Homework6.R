deforest_data=read.csv('data/deforest.csv')
head(deforest_data)

library(ggplot2)



model_sqrt_fcover <- lm(sqrt(fcover) ~ area + tpop + apop + gdp1 + gdp2 + gdp3 + distpvc +distport + hwaylen + explen + plain + elev + prec + slope + temp + temp0 + policy, data=deforest_data)
summary(model_sqrt_fcover)

# Using log(fcover) as response
model_fcover_log <- lm(log(fcover) ~ area + tpop + apop + gdp1 + gdp2 + gdp3 + distpvc + distport + hwaylen + explen + plain + elev + prec + slope + temp + temp0 + policy, data=deforest_data)
summary(model_fcover_log)


hist(log(deforest_data$fcover), breaks=30, col="yellow", main="Distribution of Log fcover", xlab="Log(Fcover)", ylab="Frequency")

hist(sqrt(deforest_data$fcover), breaks=30, col="red", main="Distribution of SQRT fcover", xlab="sqrt(Fcover)", ylab="Frequency")

hist(deforest_data$fcover,breaks=30)

library(MASS)

# Determine optimal lambda

lambda_range <- seq(-2, 2, by = 0.1)

lambda_optimal <- boxcox(deforest_data$fcover ~ 1, lambda = lambda_range, plotit = TRUE)$lambda

hist(sqrt(deforest_data$fcover),breaks=30)

library('RobStatTM')
data('waste')
head(waste)

model=lm(SolidWaste~.,data=waste)
residuals<-residuals(model)
summary(model)

car::qqPlot(residuals, main = NA, pch = 19, col = 2, cex = 0.7)
shapiro.test(residuals)

plot(model)

studentized_residuals <- rstudent(model)
potential_outliers <- which(abs(studentized_residuals) > 2)
outlier_data <- waste[potential_outliers, ]
print(outlier_data)

par(mfrow=c(1,1))

n <- nrow(waste)
p <- ncol(waste)-1
(hilev <- which(influence(model)$hat > max(2*(p+1)/n,0.5)))

plot(rstandard(model)^2, influence(model)$hat, pch =19, cex=0.5, col="blue",
     xlab = "squared residual", ylab = "leverage")
inf0 <- which(influence(model)$hat > 0.5)
text(rstandard(model)[hilev]^2, influence(model)$hat[hilev], 
     labels = inf0, cex = 0.9, font =2, pos =1)

hat_vals <- hatvalues(model)

threshold <- 2 * length(coef(model)) / nrow(waste)


high_leverage_points <- which(hat_vals > threshold)


print(waste[high_leverage_points, ])

cooks_d <- cooks.distance(model)
influential_cooks <- which(cooks_d > 4/40)
print(waste[influential_cooks, ])


library(car)
dffits_vals <- dffits(model)

# Using the rule of thumb
threshold_dffits <- 2*sqrt(length(coef(model))/nrow(waste))
influential_dffits <- which(abs(dffits_vals) > threshold_dffits)
print(waste[influential_dffits, ])


vif_values <- vif(model)
print(vif_values)


library(glmnet)


x <- as.matrix(waste[, -ncol(waste)])
y <- waste$SolidWaste 

# Fit Ridge Regression model
ridge_model <- glmnet(x, y, alpha=0) 

# Review the coefficients (for a particular lambda, say the one with minimum mean squared error)
cv.ridge <- cv.glmnet(x, y, alpha=0)
best_lambda=cv.ridge$lambda.min

final_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coefficients <- coef(final_ridge_model)
coefficients  