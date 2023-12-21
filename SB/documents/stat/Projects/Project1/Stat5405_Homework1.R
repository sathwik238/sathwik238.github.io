data(trees)
boxplot(trees$Volume,varwidth=TRUE)

boxplot(trees$Girth,notch = TRUE)

library(ggplot2)
ggplot(data = trees,mapping = aes(y=Volume,x=0)) +
  geom_boxplot(col="black",fill="lightblue",width=0.1) + coord_flip() + theme_light()


library(car)
qqPlot(trees$Volume, main="", ylab="Volume", cex=0.6,pch=19, col="red", 
       col.lines = "orange")


shapiro.test(trees$Volume)

library(nortest) 
pearson.test(trees$Volume) 

scatterplotMatrix(~mtcars$mpg+mtcars$disp+mtcars$hp+mtcars$drat+mtcars$wt+mtcars$qsec, data=mtcars)

data(mtcars)

selected_columns <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]

correlation_matrix <- cor(selected_columns)

diag(correlation_matrix) <- NA

max_correlation <- max(correlation_matrix, na.rm = TRUE)

indices <- which(correlation_matrix == max_correlation, arr.ind = TRUE)

column1 <- rownames(correlation_matrix)[indices[1, 1]]
column2 <- colnames(correlation_matrix)[indices[1, 2]]

cat("Pair with maximum correlation:", column1, "and", column2, "with correlation =", max_correlation, "\n")

library(dplyr)

mumbai_data <- read.csv('Temperature_And_Precipitation_Cities_IN/Mumbai_1990_2022_Santacruz.csv')


chennai_data <- read.csv('Temperature_And_Precipitation_Cities_IN/Chennai_1990_2022_Madras.csv')

delhi_data <- read.csv('Temperature_And_Precipitation_Cities_IN/Delhi_NCR_1990_2022_Safdarjung.csv')


banglore_data <- read.csv('Temperature_And_Precipitation_Cities_IN/Bangalore_1990_2022_BangaloreCity.csv')



# Combine the datasets with a grouping variable
combined_data <- bind_rows(
  mutate(mumbai_data, Group = "Mumbai"),
  mutate(chennai_data, Group = "Chennai"),
  mutate(delhi_data, Group = "Delhi"),
  mutate(banglore_data, Group = "Banglore")
)


boxplot(combined_data$tavg ~ combined_data$Group, xlab="City", 
        ylab="Avg Temprature", border = "blue", col="lightblue",notch=TRUE)


# Sample data
scores <- c(68, 72, 75, 71, 70, 74, 69, 73, 72, 68)
time_studied<- c(6, 7, 7, 7, 7, 7, 6, 7, 7, 6)

# Perform a one-sample t-test
t.test(scores,time_studied)

# Interpret the results
# Sample data
data <- c(72.1, 68.5, 71.2, 70.8, 69.6, 73.2, 68.9, 72.5)

# Perform Shapiro-Wilk test
shapiro.test(data)


# Sample data (percentages)
categories <- c("Category A", "Category B", "Category C", "Category D")
percentages <- c(30, 25, 20, 25)

# Create a pie chart
par(mfrow=c(1,2))
pie(percentages, labels = categories, main = "Ethical Pie Chart Example")

categories2 <- c("Category A", "Category B", "Category C", "Category D","Category E", "Category F", "Category G", "Category H", "Category I", "Category J", "Category K", "Category L")
percentages2 <- c(2,3,4,5,5,5,20,25,15,5,5,5)
pie(percentages2, labels = categories2, main = "Unethical Pie Chart Example")




# Sample data (percentages)
categories <- c("Category A", "Category B", "Category C", "Category D")
percentages <- c(30, 25, 20, 25)

# Create a pie chart
par(mfrow=c(1,2))
pie(percentages, labels = categories, main = "Chart with Label")
percentages2 <- c(2,3,4,5,5,5,20,25,15,5,5,5)
pie(percentages2, main = "Chart without Label")

# Sample data
x <- rnorm(100)  # Random data for the plot

# Load the ggplot2 library
library(ggplot2)

# Create a ggplot object
plot <- ggplot(data.frame(x = x), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.2, fill = "blue", alpha = 0.5) +
  geom_point(aes(y = 0), col = "red", pch = 16) +
  geom_line(stat = "density", aes(color = "Density"), size = 1) +
  geom_curve(aes(x = -2.5, y = 0.1, xend = 2.5, yend = 0.1, color = "Curve"), curvature = 0.3, size = 1) +
  geom_density(aes(color = "Density"), fill = NA, size = 1) +
  scale_color_manual(values = c("Density" = "green", "Curve" = "purple")) +
  labs(x = "X", y = "Density/Frequency", title = "Combined Visuals")

# Print the plot
print(plot)


