cars <- read.csv("C:\\Users\\Desktop\\Data base concepts\\Data visualization\\Week 8\\mtcars.csv")
str(cars)

# Load ggplot2 package
library(ggplot2)

# Calculate correlation matrix
cor_matrix <- cor(cars)

# Find the most correlated pair of variables
max_cor <- which(cor_matrix == max(cor_matrix[lower.tri(cor_matrix)]), arr.ind = TRUE)
var1 <- colnames(cars)[max_cor[1]]
var2 <- colnames(cars)[max_cor[2]]

# Perform correlation test
cor_test <- cor.test(cars[[var1]], cars[[var2]])

# Print correlation coefficient and p-value
cat("The most correlated pair of variables is", var1, "and", var2, "\n")
cat("The correlation coefficient is", round(cor_test$estimate, 3), "\n")
cat("The p-value is", round(cor_test$p.value, 3), "\n")

# Plot scatter plot with linear regression line
ggplot(cars, aes(x = .data[[var1]], y = .data[[var2]])) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = var1, y = var2,
       title = "Correlation between disp and wt") + # Add title here
  theme_minimal()
