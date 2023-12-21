library(ACSWR)
data(tensile)


aovmod <- aov(Tensile_Strength ~ factor(CWP), data = tensile)
summary(lm(aovmod))

pred.CWP <- expand.grid(CWP=unique(data$CWP))
lsmeans <- predict(model, newdata=pred.CWP, se=TRUE, interval="confidence")
cbind(pred.CWP, lsmeans$fit)

pairwise.t.test(tensile$Tensile_Strength, factor(tensile$CWP), 
                p.adjust.method = "bonf")

TukeyHSD(aovmod)

library(sjstats)
library(effectsize)

eta_sq <- eta_squared(model)
print(eta_sq)

boxplot(ToothGrowth$len)
shapiro.test(ToothGrowth$len)
table(ToothGrowth$supp)
table(ToothGrowth$dose)

pb <- ggplot(ToothGrowth, aes(x=factor(dose),  y=len, fill=factor(dose))) + geom_boxplot(show.legend = F)
ggplot(ToothGrowth, aes(x=factor(dose),  y=len, fill=factor(dose))) + 
  geom_boxplot(show.legend = F)

x <- 1:length(ToothGrowth$len)

# Create the scatterplot
plot(x, ToothGrowth$len)
points(x, ToothGrowth$len, pch = 16, col = "blue")

library(car)
leveneTest(ToothGrowth$len, group = factor(ToothGrowth$dose))



library(car)
leveneTest(ToothGrowth$len, group = factor(ToothGrowth$dose))


# Fit a one-factor ANOVA model
anova_dose <- aov(len ~ factor(dose), data = ToothGrowth)

summary(anova_dose)
summary(lm(anova_dose))


aovres1 <- residuals(anova_dose)
qqPlot(aovres1, main = NA, pch = 19, col = 2, cex = 0.7)
shapiro.test(aovres1)



leveneTest(ToothGrowth$len, group = factor(ToothGrowth$supp))

anova_supp <- aov(len ~ factor(supp), data = ToothGrowth)

summary(anova_supp)
summary(lm(anova_supp))


aovres2 <- residuals(anova_supp)
qqPlot(aovres2, main = NA, pch = 19, col = 2, cex = 0.7)
shapiro.test(aovres2)

# Fit a two-factor ANOVA model without interaction
anova_additive <- aov(len ~ factor(dose) + factor(supp), data = ToothGrowth)

summary(anova_additive)
summary(lm(anova_additive))



# Fit a two-factor ANOVA model with interaction
anova_interaction <- aov(len ~ factor(dose) * factor(supp), data = ToothGrowth)

summary(anova_interaction)
summary(lm(anova_interaction))



