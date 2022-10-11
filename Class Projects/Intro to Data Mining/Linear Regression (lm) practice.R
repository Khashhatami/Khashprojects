#############################################################################
#Modeling in R using lm() pakage 

library(ggplot2)

oilChanges <-c(3,5,2,3,1,4,6,4,3,2,0,10,7,8)

repairs <-c(300, 300, 500, 400, 700, 420, 100, 290,475, 620, 600, 0, 200, 50)

miles <-c(20100, 23200, 19200, 22100, 18400, 23400, + 17900, 19900, 20100, 24100, 18200, 19600, 20800, 19700)

oil <- data.frame (oilChanges, repairs, miles)

View (oil)

#Some exploratory inspection (repairs is the depedent variable while the oilchange is independent):
plot(oil$oilChanges, oil$repairs)

#It seems like more often oil has been changed, the less repair is required

#Now we do the same with miles (independent) and repaires (dependent)
plot(oil$miles, oil$repairs)

#There does not seem to be a pattern between miles and repaires. Therefore, we use oil change and repair
#in our modeling 

#This line means repair is a function of oil change. The second argument tells R where to look for
#for the data
model1 <- lm(formula=repairs ~ oilChanges, data=oil)
model1

#The output:
#Call:
#lm(formula = repairs ~ oilChanges, data = oil)

#Coefficients:
# (Intercept)   oilChanges  
#652.19       -71.99

#To have a more specific report on our model, we can use summary() function:
summary(model1)

#important outputs:
#Adjusted R-squared:  0.854
#p-value: 1.436e-06

#This means that there is 85 percent chance that oil change predicts repairs
#also p-value shows that this result is statistically significant ( 0.000001436 < 0.05)

#Residuals:
#Min       1Q   Median       3Q      Max 
#-136.208  -48.195   -0.211   54.782  119.803

# I still do not know to read risduals but the book has this to say about them:
#"We see five summary points about residuals. One way to
#explore how well the model fits the data is to see if the residuals are
#symmetrically distributed across these five summary points and for the
#median to be close to zero." (page 216)

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  652.191     40.537  16.089 1.74e-09 ***
#  oilChanges   -71.994      8.202  -8.778 1.44e-06 ***

#if the formula for the modleing is Y= mX+B, The first line about B and the second line is about 
#m. Intercept predict the how the vlaue of Y is affected when X=0. The solpe predicts how the value
#X affect the value of Y. 
#There is also a column for errors as well as t-value and Pr(>|t|) which can tell if these results
#are scientifically significant

#for instance the The slope term in our model is saying that for every additional oil change,
#the expected repairs decrease by about $72 (oilChanges   -71.994).

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#the signifiant code tells us how many astrisks is sufficeint for satistical significance. 

#   Pr(>|t|)    
#   1.74e-09 ***
#   1.44e-06 ***
# As we can see both Coefficients have three astrisks which they are both statistically significant


#Concluusion: 
#Because both the R-squared value and the
#slope coefficient on oilChanges are significant, we can “reject the null
#hypothesis” that oil changes and repairs are not connected.

#abline() draws a line for us 
plot(oil$oilChanges, oil$repairs)
abline(model1)
#Alternatively we could have used this line of code to get the same plot
plot(model1)

#This model shows us :
#it predicts very low (almost zero [0]) repairs if we
#do nine or more oil changes, but about $680 if we do no oil changes.

#Drawing multilple linear regression:
# we can use "+" to add more predictors (indepednet variables ) to increase the accuracy of our
#predictions
m <- lm(formula=repairs ~ oilChanges + miles, data=oil)

summary(m)

#The ouput:
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 343.26567  231.42285   1.483    0.166    
#oilChanges  -71.98591    7.93052  -9.077 1.93e-06 ***
#  miles         0.01508    0.01114   1.354    0.203    

#see how many asterisks are given to each Coefficients

# let's take into consideration how the price of repairs and ol channges collectively affected by oil changes
#we can add a new column to our data frame:
oil$oilChangeCost <- oil$oilChanges * 350
oil$totalCost <- oil$oilChangeCost + oil$repairs

#let us model that: 
m <- lm(formula=totalCost ~ oilChanges, data=oil)
plot(oil$oilChanges, oil$totalCost)
abline(m)

#Notice how much more accurate our model has become. the abline is cutting through the points
#with more presicion

#the model shows eventhough more oil changes reduce the repairs, but given that oil changes are really high,
# it is not worth to change your oil at all!

# let's see if this reflected on our p-value and R-squared
summary(m)

#the output:  p-value: 2.763e-13 / Adjusted R-squared:  0.9888
# the probability has increased to 99% percent !!!!


#With predict() function we can see under different scenrios what results we can get
#we can set  a value for our indepedent variable, and then R calculates what the dependent varialbe is going to look like:

# let's test  to see what price we have to pay if we dont change our oil at all
test = data.frame(oilChanges=0)
predict (m, test, type="response")
#652.191 


# how much we would have to pay if we change our oil 5 times
test = data.frame (oilChanges=5)
predict (m, test, type="response")
#2042.219 

# how much we would have to pay if we change our oil 10 times
test = data.frame (oilChanges=10)
predict (m, test, type="response")
#3432.247 

#let us conclude with some nice graphing:
ggplot(oil, aes(x = oilChanges, y = totalCost))+ geom_point() + stat_smooth(method="lm", col="red")


