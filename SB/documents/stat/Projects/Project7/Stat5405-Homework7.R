snails = read.csv('data/snails.csv')
head(snails)

snails$Aspect[snails$Aspect == 6] = 5
snails$Aspect[snails$Aspect == 2] = 1
snails$Aspect <- as.factor(snails$Aspect)
snails$Soil <- as.factor(snails$Soil)
snails$CC <- as.factor(snails$CC)
snails$LC <- as.factor(snails$LC)

set.seed(123457)
train.prop <- 0.80
trnset <-
  sort(sample(1:nrow(snails), ceiling(nrow(snails) * train.prop)))
# create the training and test sets
train.set <- snails[trnset, ]
test.set  <- snails[-trnset, ]

n1=dim(train.set)[1]
n2=dim(test.set)[1]
n1
n2

gaenig.pf <-glm(
  Gaenig ~ Elevation + Slope + Aspect + Soil + CC + CO + LC + PA.sp + PA.other,
  family = 'poisson',
  data = train.set
)
summary(gaenig.pf)

disp.est <- gaenig.pf$deviance/gaenig.pf$df.residual
disp.est

with(gaenig.pf, cbind(deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

deviance_residuals <- resid(gaenig.pf, type = "deviance")
plot(deviance_residuals, main = "Deviance Residuals", ylab = "Deviance Residual", xlab = "Index")
abline(h = 0, col = "red")
hist(deviance_residuals, main = "Histogram of Deviance Residuals", xlab = "Deviance Residual")
plot(predict(gaenig.pf, type = "response"), deviance_residuals, 
     main = "Deviance Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, col = "red")
#plot(residuals(gaenig.pf, type = "deviance") ~ fitted(gaenig.pf))


gaenig.pn <- glm(Carcar~1, 
                 family='poisson', data=train.set)
AIC(gaenig.pn, gaenig.pf)

BIC(gaenig.pn,gaenig.pf)

train_predicted <- predict(gaenig.pf, type = "response")
sum(abs(train.set$Gaenig-train_predicted))/n1

predicted<-predict(gaenig.pf,type='link',newda=test.set)
sum(abs(test.set$Gaenig-predicted))/n2


gaenig.qpf <-glm(
  Gaenig ~ Elevation + Slope + Aspect + Soil + CC + CO + LC + PA.sp + PA.other,
  family = quasipoisson(link = "log"),
  data = train.set
)
summary(gaenig.qpf)

disp.est <- gaenig.qpf$deviance/gaenig.qpf$df.residual
disp.est


with(gaenig.qpf, cbind(deviance = deviance, df = df.residual,
                       p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# Diagnostic plots
plot(gaenig.qpf)


gaenig.qpn <- glm(Carcar~1, 
                  family=quasipoisson, data=train.set)
AIC(gaenig.qpn, gaenig.qpf)
BIC(gaenig.qpn, gaenig.qpf)


sum(abs(train.set$Gaenig-fitted(gaenig.qpf)))/n1


predicted<-predict(gaenig.qpf,type='link',newda=test.set)
sum(abs(test.set$Gaenig-predicted))/n2


library(MASS)

gaenig.nbf <-glm.nb(
  Gaenig ~ Elevation + Slope + Aspect + Soil + CC + CO + LC + PA.sp + PA.other,
  data = train.set
)
summary(gaenig.nbf)

dispersion_nb <- gaenig.nbf$theta
dispersion_nb

with(gaenig.nbf, cbind(deviance = deviance, df = df.residual,
                       p = pchisq(deviance, df.residual, lower.tail=FALSE)))

gaenig.nbn <- glm.nb(Carcar~1, data=train.set)
AIC(gaenig.nbn, gaenig.nbf)

train_predicted <- predict(gaenig.nbf, type = "response")
sum(abs(train.set$Gaenig-train_predicted))/n1

test_predicted_nb <- predict(gaenig.nbf, newdata = test.set, type = "response")
sum(abs(test.set$Gaenig-test_predicted_nb))/n2



gaenig.nbf1 <-glm.nb(
  Gaenig ~  Soil + PA.other  ,
  data = train.set
)
summary(gaenig.nbf1)

with(gaenig.nbf1, cbind(deviance = deviance, df = df.residual,
                        p = pchisq(deviance, df.residual, lower.tail=FALSE)))

train_predicted <- predict(gaenig.nbf1, type = "response")
sum(abs(train.set$Gaenig-train_predicted))/n1

test_predicted_nb <- predict(gaenig.nbf1, newdata = test.set, type = "response")
sum(abs(test.set$Gaenig-test_predicted_nb))/n2

