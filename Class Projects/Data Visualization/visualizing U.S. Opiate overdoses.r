library(dplyr)
library(ggplot2)

# create a data frame with state names and death rates
overdoses <- read.csv("C:\\Users\\wecan\\Desktop\\Data base concepts\\Data visualization\\week 6\\overdoses.csv")
overdoses$Deaths <- gsub(",", "", overdoses$Deaths)
overdoses$Deaths <- as.numeric(overdoses$Deaths)
overdoses$Population <- gsub(",", "", overdoses$Population)
overdoses$Population <- as.numeric(overdoses$Population)
state.name = overdoses$Abbrev
death_rate = overdoses$Deaths/overdoses$Population* 100000
data <- data.frame(state.name , death_rate)

# sort data frame by decreasing death rate
data <- data %>% arrange(desc(death_rate))

# create bar chart with state names on x-axis and death rates on y-axis
ggplot(data, aes(x = state.name, y = death_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("State") + ylab("Death rate per 100,000 population") +
  ggtitle("US State Death Rates") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
