qnorm(0.025, mean = 0, sd = 1, lower.tail = TRUE)
qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

alfa<-0.05
n1<-11
n2<-13
qt(1-alfa/2, df = n1+n2-2)


qchisq(0.95, df = 17)
qf(0.95, df1 = 4, df2 = 18)

library(vcd)

data(Arthritis)

#Arthritis$Improved <- ifelse(Arthritis$Improved %in% c("Some", "Marked"), "Improved", Arthritis$Improved)
#Arthritis$Improved[Arthritis$Improved=="1"]<-'None'

Arthritis$Improved <- ifelse(Arthritis$Improved %in% c("Some", "Marked"), "Improved", "None")

cross_tab<-table(Arthritis$Treatment,Arthritis$Improved)
addmargins(cross_tab)

# Compute the sample estimate of π₁|₁ (proportion of Improved patients in Treated group)
pi1_1 <- cross_tab[2, "Improved"] / sum(cross_tab[2, ])

# Compute the sample estimate of π₁|₂ (proportion of Improved patients in Placebo group)
pi1_2 <- cross_tab[1, "Improved"] / sum(cross_tab[1, ])

cat("Sample estimate of π₁|₁ (Treated group):", pi1_1, "\n")
cat("Sample estimate of π₁|₂ (Placebo group):", pi1_2, "\n")


alpha <- 0.05  # Significance level (for a 95% confidence interval)
z <- qnorm(1 - alpha/2)  # Critical value for a two-tailed test

pi1_1 <- cross_tab[2, "Improved"] / sum(cross_tab[2, ])
pi1_2 <- cross_tab[1, "Improved"] / sum(cross_tab[1, ])
n1<-sum(cross_tab[2, ])
n2<-sum(cross_tab[1, ])

se <- sqrt((pi1_1 * (1 - pi1_1) / n1) + (pi1_2 * (1 - pi1_2) / n2))
margin_of_error <- z * se
lower_limit <- (pi1_1 - pi1_2) - margin_of_error
upper_limit <- (pi1_1 - pi1_2) + margin_of_error

# Print the confidence interval
cat("95% Confidence Interval for (pi1|1 - pi1|2): [", lower_limit, ",", upper_limit, "]\n")


# Perform two-sided hypothesis test
test_result <- prop.test(x = c(pi1_1 * n1, pi1_2 * n2), 
                         n = c(n1, n2), 
                         alternative = "two.sided")

test_result

library(vcd)
data("Arthritis")
# Combine "Some" and "Marked" into "Improved" status
Arthritis$Improved <- ifelse(Arthritis$Improved %in% c("Some", "Marked"), "Improved", "None")

cross_tab<-table(Arthritis$Treatment,Arthritis$Improved)
cross_tab

pi1_1 <- cross_tab[2, "Improved"] / sum(cross_tab[2, ])
pi1_2 <- cross_tab[1, "Improved"] / sum(cross_tab[1, ])

w1<-pi1_1/(1-pi1_1)

w2<-pi1_2/(1-pi1_2)

cat("Odds of Improvement in Placebo Group",w2,"\n")
cat("Odds of Improvement in Treatment Group",w1)

odds_ratio=w1/w2
odds_ratio


tab_1 <- table(Arthritis$Treatment, Arthritis$Improved)
library(epitools)
oddsratio(tab_1)

library("ACSWR")

data(tensile)

boxplot(Tensile_Strength ~ CWP, data = tensile, xlab = "CWP", ylab = "Tensile Strength", col='green')

library(ggplot2)  # For ggplot2-based plotting (optional)
levels=unique(tensile$CWP)
for (i in levels){
  shapiro_test<-shapiro.test(tensile$Tensile_Strength[tensile$CWP==i])
  print(i)
  print(shapiro_test)
}

shapiro.test(tensile$Tensile_Strength)

library(car)
bartlett.test(Tensile_Strength ~ CWP, data = tensile)
leveneTest(Tensile_Strength ~ factor(CWP), data = tensile)


qqPlot(tensile$Tensile_Strength, main="", ylab="Residuals", cex=0.6,pch=19, col="red", 
       col.lines = "orange")
shapiro.test(tensile$Tensile_Strength)
# Create a univariate scatterplot for Tensile Strength - to check for independence

ggplot(tensile, aes(x = seq_along(Tensile_Strength), y = Tensile_Strength)) +
  geom_point() +
  labs(x = "Observation Index", y = "Tensile Strength") +
  ggtitle("Univariate Scatterplot of Tensile Strength") +
  theme_minimal()

# Fit a one-factor ANOVA model
model <- aov(Tensile_Strength ~ factor(CWP), data = tensile)


anova_result <- summary(lm(model))


print(anova_result)

pairwise.t.test(tensile$Tensile_Strength, tensile$CWP, 
                p.adjust.method = "none")

# Extract residuals from the ANOVA model
residuals <- residuals(model)

# Create a Normal QQPlot of residuals
car::qqPlot(residuals, main = NA, pch = 19, col = 2, cex = 0.7)

shapiro.test(residuals)


