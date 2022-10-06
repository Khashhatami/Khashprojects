# Load packages
library(plyr)


# 1. Read file
students <- read.csv("C:\\Users\\wecan\\OneDrive\\Documents\\GitHub\\Khashprojects\\Student_i_Averages.csv", header = T)
#--Find grade averages based on gender, add as new column
students_avg <- ddply(students,"Sex",transform,Grade.Average=mean(Grade))

#--Add numerical representation of gender for finding average, add as new column
students_avg <- data.frame(students_avg, Gender.ID = as.numeric(students$Sex))

# --Find average of gender values
paste("Average of gender value is: ",mean(students_avg$Gender.ID))
# [1] "Average of gender value is:  1.2"

#--Write resulting output to a file
write.table(students_avg, "Student_Averages.txt", sep=",")

# 2. Filter for only names containing "i"
students_i <- subset(students_avg, grepl("i", students_avg$Name, ignore.case= T))

#--Write those names to a file
write.table(students_i, "Student_i_Averages.txt")

# 3. Write the filtered data set and convert it to a CSV file
#    hint (file.choose() and subset())
chosen_file <- file.choose()
write.csv(read.table(chosen_file, header=T, sep=" "), "Student_i_Averages.csv")
