#Download any type of data (from the web or use datasets package) or create your own set. 
my_air <- airquality 

#try Generic Functions
head(my_air)
list(my_air)

#  output:
#> head(my_air)
#Ozone Solar.R Wind Temp Month Day
#1    41     190  7.4   67     5   1
#2    36     118  8.0   72     5   2
#3    12     149 12.6   74     5   3
#4    18     313 11.5   62     5   4
#5    NA      NA 14.3   56     5   5
#6    28      NA 14.9   66     5   6
#> list(my_air)
#[[1]]
#Ozone Solar.R Wind Temp Month Day
#1      41     190  7.4   67     5   1
#2      36     118  8.0   72     5   2
#3      12     149 12.6   74     5   3
#4      18     313 11.5   62     5   4
#5      NA      NA 14.3   56     5   5
#6      28      NA 14.9   66     5   6
#7      23     299  8.6   65     5   7... 

#In third and last step, explore if S3 and S4 can be assigned to your data set.
isS4(my_air)

#output:
# > isS4(my_air)
#[1] FALSE


#Creating an S3 and S4
my_s3 <- list(name = "Earth", kind = "planet", Radius = 3958.8)

class(my_s3) <- "Member of Solar system"
mode(my_s3)
attributes(my_s3)
mode(my_s3$name)
my_s3

#output:
#> mode(my_s3)
[#1] "list"
#> attributes(my_s3)
#$names
#[1] "name"   "kind"   "Radius"

#$class
#[1] "Member of Solar system"

#> mode(my_s3$name)
#[1] "character"
#> my_s3
#$name
#[1] "Earth"

#$kind
#[1] "planet"

#$Radius
#[1] 3958.8

#attr(,"class")
#[1] "Member of Solar system"

#output:
#> mode(my_s3)
#[1] "list"
#> attributes(my_s3)
#$names
#[1] "name"   "kind"   "Radius"

#$class
#[1] "Member of Solar system"

#> mode(my_s3$name)
#[1] "character"
#> my_s3
#$name
#[1] "Earth"

#$kind
#[1] "planet"

#$Radius
#[1] 3958.8

#attr(,"class")
#[1] "Member of Solar system"

setClass("Employee", 
         representation(
           name = "character",
           job_title = "character",
           pay = "numeric"
         ))

my_s4 <- new("Employee", name = "Kathrine", job_title = "developer", pay = 50000)
my_s4
mode(my_s4)
mode(slot(my_s4, "name"))
mode(my_s4@name)

#ouput:
#> my_s4
#An object of class "Employee"
#Slot "name":
#  [1] "Kathrine"

#Slot "job_title":
#  [1] "developer"

#Slot "pay":
#  [1] 50000

#> mode(my_s4)
#[1] "S4"
#> mode(slot(my_s4, "name"))
#[1] "character"
#> mode(my_s4@name)
#[1] "character"
