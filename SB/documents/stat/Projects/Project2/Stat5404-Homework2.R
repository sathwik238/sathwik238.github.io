
library(PairedData)
library(car)
data(PrisonStress)
data(Loblolly)
sports_data<-PrisonStress[PrisonStress$Group=='Sport',]
control_data<-PrisonStress[PrisonStress$Group=='Control',]
shapiro.test(sports_data$PSSafter-sports_data$PSSbefore) 
qqPlot(sports_data$PSSafter-sports_data$PSSbefore, main="", ylab="PSSafter", col="red", col.lines = "orange")

result <- t.test(sports_data$PSSafter,sports_data$PSSbefore, paired = TRUE, alternative = "less")
print(result)

library(effsize)
cohen.d(sports_data$PSSafter,sports_data$PSSbefore, paired = TRUE)


summary(PrisonStress$PSSafter[PrisonStress$Group=='Sport'])
summary(PrisonStress$PSSafter[PrisonStress$Group=='Control'])


library(PairedData)
library(ggplot2)


data(PrisonStress)

exit_data <- subset(PrisonStress,(Group == "Sport" | Group == "Control"))

ggplot(exit_data, aes(x = Group, y = PSSafter)) +
  geom_boxplot() +
  labs(x = "Group", y = "Stress Levels at Exit") +
  ggtitle("Comparison of Stress Levels between Sport and Control Groups at Exit")


hist(PrisonStress$PSSafter[PrisonStress$Group == "Sport"], col = "blue", xlab = "Stress Level", ylab = "Frequency", main = "Stress Levels in Sport Group")
hist(PrisonStress$PSSafter[PrisonStress$Group == "Control"], col = "red", xlab = "Stress Level", ylab = "Frequency", main = "Stress Levels in Control Group")

# Density Plots
plot(density(PrisonStress$PSSafter[PrisonStress$Group == "Sport"]), col = "blue", main = "Density Plot: Stress Levels in Sport Group", xlab = "Stress Level", ylab = "Density")
lines(density(PrisonStress$PSSafter[PrisonStress$Group == "Control"]), col = "red")
legend("topright", legend = c("Sport", "Control"), fill = c("blue", "red"))




ggplot(exit_data, aes(x = Group, y = PSSafter)) +
  geom_boxplot() +
  labs(x = "Group", y = "Stress Levels at Exit") +
  ggtitle("Comparison of Stress Levels Variability between Sport and Control Groups at Exit")

qqPlot(sports_data$PSSafter, main="", ylab="PSSafter", col="red", col.lines = "orange")
shapiro.test(sports_data$PSSafter) 
qqPlot(control_data$PSSafter, main="", ylab="PSSafter", col="red", col.lines = "orange")
shapiro.test(control_data$PSSafter) 

var.test(sports_data$PSSafter,control_data$PSSafter)

t.test(sports_data$PSSafter,control_data$PSSafter,pooled=TRUE)



mean_sport <- mean(PrisonStress$PSSafter[PrisonStress$Group == "Sport"])
sd_sport <- sd(PrisonStress$PSSafter[PrisonStress$Group == "Sport"])


mean_control <- mean(PrisonStress$PSSafter[PrisonStress$Group == "Control"])
sd_control <- sd(PrisonStress$PSSafter[PrisonStress$Group == "Control"])

n_sport <- sum(PrisonStress$Group == "Sport")
n_control <- sum(PrisonStress$Group == "Control")
pooled_sd <- sqrt(((n_sport - 1) * sd_sport^2 + (n_control - 1) * sd_control^2) / (n_sport + n_control - 2))
cohen_d <- (mean_sport - mean_control) / pooled_sd
cohen_d

cohen.d(PrisonStress$PSSafter[PrisonStress$Group == "Sport"],PrisonStress$PSSafter[PrisonStress$Group == "Control"],pooled = FALSE)

[Loblolly$age==20 | Loblolly$age==25,]
ggplot(filtered_data, aes(x = factor(age), y = height)) +
  geom_boxplot() +
  labs(x = "Age", y = "Height") +
  ggtitle("Comparison of height between 20 and 25 year old pine trees")


qqPlot(filtered_data$height[filtered_data$age==20], main="", ylab="Height", col="red", col.lines = "orange")
shapiro.test(filtered_data$height[filtered_data$age==20]) 


qqPlot(filtered_data$height[filtered_data$age==25], main="", ylab="Height", col="red", col.lines = "orange")
shapiro.test(filtered_data$height[filtered_data$age==25])

shapiro.test(filtered_data$height[filtered_data$age==20])

var.test(filtered_data$height[filtered_data$age==25],filtered_data$height[filtered_data$age==20])



diffs <- filtered_data$height[filtered_data$age==25]-filtered_data$height[filtered_data$age==20]

boxplot(diffs)
# Histogram of differences
hist(diffs, breaks = 15, main = "Histogram of Paired Differences", xlab = "Differences (Age25 - Age20)", ylab = "Frequency")
shapiro.test(diffs)

result <- BSDA::SIGN.test(diffs, md = 0, alternative = "two.sided")
print(result)

{r}
cohen.d(filtered_data$height[filtered_data$age==25],filtered_data$height[filtered_data$age==20])

