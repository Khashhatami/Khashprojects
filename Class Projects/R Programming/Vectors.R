
  

# converting the the data into clear vectors 
freq <- c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2)
bloodp <- c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176)
first <- c("bad","bad","bad","bad","good","good","good","good",NA,"bad")
second <- c("low","low","high","high","low","low","high","high","high","high")
finaldecision <- c("low","high","low","high","low","high","low","high","high","high")

#Converting Good and bad assessment of doctors into categorical data (for historgram and plotboxing)

good_bad <- function(x) {
  if(x == "bad" || is.na(x)) { x <- 1 }
  else { x <- 2 }
}

# doing the same as above for blood pressure. Here we are trying to convert the normative terms such as "high" and "low"
# to descriptive numbers such as 1 and 2 to make it possible to R to graph and visualize 

low_high <- function(x) {
  if(x == "low" || is.na(x)) { x <- 1 }
  else { x <- 2 }
}

# Here we are designating the doctors to their evaluation of pataince performance 
first.num <- sapply(first, good_bad)
second.num <- sapply(second, low_high)
final.num <- sapply(finaldecision, low_high)

# Now that we have categorical vectors for doctors opinions, we can put all of the data into a single data frame

medical <- data.frame(freq, bloodp, first.num, second.num, final.num)
names(medical) <- c("freq", "bp", "first", "second", "final")

# Plot boxplots
boxplot(medical$bp~medical$first, xlab="1 = Bad Assessment (Internal)  2 = Good Assessment (Internal)", ylab="Blood Pressure")
boxplot(medical$bp~medical$second, xlab="1 = Low Assessment (External)   2 = High Assessment (External)", ylab="Blood Pressure")
boxplot(medical$bp~medical$final, xlab="1 = Not Immediate (Final)  2 = Immediate (Final)", ylab="Blood Pressure")

# Plot histograms
hist(medical$freq, main="Frequency")
hist(medical$bp, main="Blood Pressure")
hist(medical$first, main="Internal Assessment", xlab="1 - Bad    2 - Good")
hist(medical$second, main="External Assessment", xlab="1 - Bad    2 - Good")
hist(medical$final, main="Final Assessment", xlab="1 - Not Immediate    2 - Immediate")
