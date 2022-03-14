

# Import packages
library(ggplot2)
library(lattice)
library(hexbin)

# Import data
# Credit: https://vincentarelbundock.github.io/Rdatasets/datasets.html
#         "Cigarette Consumption Data"
cig <- read.csv("C:\\Users\\wecan\\OneDrive\\Documents\\CigarettesB.csv")

# Investigate data
str(cig)
head(cig)

# Basic R plots
plot(packs ~ price, data= cig)
plot(packs ~ income, data= cig)


# lattice plot
densityplot(~price|packs,data=cig)

# ggplot2 plots
ggplot(cig, aes(x=income)) +
  geom_histogram() +
  geom_density(stat="density", alpha=I(0.2), fill="blue") +
  xlab("income") +  ylab("packs")

