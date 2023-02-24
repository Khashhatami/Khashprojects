library(ggplot2)
library(gridExtra)

data(mtcars)
cars <- mtcars
# Histogram
hist_cyl <- ggplot(cars, aes(x = cyl)) + 
  geom_histogram(aes(y = ..density..), fill = "blue", color = "black", alpha = 0.5) + 
  geom_density(color = "red") +
  labs(title = "Distribution of Cylinders in cars",
       x = "Cylinders", y = "Density")

# Boxplot
boxplot_mpg <- ggplot(cars, aes(x = factor(cyl), y = mpg)) + 
  geom_boxplot(fill = "orange", color = "black", alpha = 0.5) + 
  labs(title = "Distribution of MPG by Cylinders in cars",
       x = "Cylinders", y = "Miles per Gallon")

# Density Plot
density_cyl_mpg <- ggplot(cars, aes(x = mpg, fill = factor(cyl))) + 
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of MPG by Cylinders in cars",
       x = "Miles per Gallon", y = "Density")

# Combine visualizations with grid layout
grid.arrange(hist_cyl, boxplot_mpg, density_cyl_mpg, ncol = 3)