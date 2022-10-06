library(ggplot2)
Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Bernie")
ABC <- c(4, 62, 51, 21, 2, 14, 15)
CBS <- c(12, 75, 43, 19, 1, 21, 19) 
election <- data.frame(Name, ABC, CBS, stringsAsFactors=F)
election

election <- election[order(ABC,CBS),]
election

library(ggplot2)
ggplot(election, aes(x=Name, y=ABC, fill=Name)) +
  geom_bar(stat="identity", position="dodge")
ggplot(election, aes(x=Name, y=CBS, fill=Name)) +
  geom_bar(stat="identity", position="dodge")
