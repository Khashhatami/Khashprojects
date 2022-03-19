
# in this chapter, we will use the suppervised learning algorithms for our data
#mining purposes 
install.packages("kernlab")
library(kernlab)

#Loading the data:
data(spam)
str(spam)
dim(spam)

#Now let's take a cloer look to spam$type to see what kind of emials
#have been marke as spam by human experts
#the goal of this exercise is to develop an supervised algorithm that uses 
#these inputs to determine auotmatically if an email is spam or not 

#table() function enables us to see the factor spam and see what categories 
#these emails can fall into 
#We use the table function because type is a factor rather than a numeric
#variable.
table(spam$type)

#When training machine learning algorithm
#we need a clean data set. Then we need to divide the data set
#into two pieces. The first piece is to train the data and the 
#second piece is to test the algorithm and see whether it actually
#works or not. We cannot test the algorithm on the data set that has
#not been cleaned cause we will not know whether or not it has worked correctly. 

# There fore we need to create twp sets : 1: training set 2. Test set
# It is important to randomize
#your selection of cases for the training and test sets in order to ensure that
#there is no systematic bias in the selection of cases.

# In human language the function below, is going to 
#choose random numbers from 1 to 4601 (the number of rows in spam)
#based on these randomized row numbers we can train our algorithm without 
#sytematic bias in case the order of rows were sorted based a specific criterion 
randIndex<- sample(1:dim(spam)[1])

summary(randIndex)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1    1151    2301    2301    3451    4601 
length(randIndex)
#[1] 4601

# now we want to allocate two thirds of our data set for training and 
#one third for testing the algorithm 

#in human language the function below grabs the number of rows and divide them
# in three then multiply them by two to give us the 2/3 of our data set
#Note that the floor() function
#chops off any decimal part of the calculation. We want to get rid of any
#decimal because an index variable needs to be an integer.
cutPoint2_3 <- floor(2*dim(spam)[1]/3)
cutPoint2_3
#[1] 3067 // this is our cut point for 2/3 of the data set

# now we isolate the traning set
trainData <- spam[randIndex[1:cutPoint2_3],]

#Then we create the test set. Notice that we used +1 after cutPoiint2_3 because 
#we do not want the last row from the traning set to be included in the test set.
#Also notice that we put cutPoint2_3+1 in paranthesis so R does not get confused with
#its math
testData <- spam[randIndex[(cutPoint2_3+1):dim(spam)[1]],]

#*************************************************************************************#
#*************************************************************************************#
#*************************************************************************************#


# time to train our support vector model

#type~. expression means that we want to have the type variable (i.e., whether the
#message is spam or nonspam) as the outcome variable that our model
#predicts. The tilde character (~) in an R expression simply separates the
#left-hand side of the expression from the right-hand side. Finally, the dot
#character (.) is a shorthand that tells R to use all of the other variables in
#the dataframe to try to predict type.

# kernel=“rbfdot”. the kernel is the customizable
#part of the SVM algorithm that lets us project the low-dimensional
#problem into higher-dimensional space.

# So type is the dependent output which we expect the algorithm to generate. 
#remember how we generated regresion models using y~x . here the dot after the 
# the tilda simply says use all the columns as the indepedent variable.

#cross= and prob.model= specify our how much the patterns in our data is
#specialized. If the model is too well trained on a particular data set.
#it cannot function on other data sets.

#C argument set the standard to margin of error 


svmOutput <- ksvm(type~., data=trainData,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)

svmOutput
# Cross validation error is higher than the training error because our model 
#inevitabely peforms worse on new data sets compared to the original data set 
#that it has been trained with. In other word, if we used the same data set that 
#we trained the algorithm with, we will have 2 percent chance of error while if we
#try this on a new data set, we will have 7 percent chance of error 

#Training error : 0.029997 
#Cross validation error : 0.073691

#Support vectors are data points that are closer to the hyperplane 
#and influence the position and orientation of the hyperplane. 
#Using these support vectors, we maximize the margin of the classifier. 
#Deleting the support vectors will change the position of the hyperplane.
#These are the points that help us build our SVM.

#Number of Support Vectors : 938 

#kpar="automatic" has determined our our sigma 

#Hyperparameter : sigma =  0.0301860226676077

#*************************************************************************************#
#*************************************************************************************#
#*************************************************************************************#

#Now it is time to see our support vectors 
#the list in which our support vectors are located is a nested list; so we use
# [[]] to access it
hist(alpha(svmOutput)[[1]])

#The maximum value of the support vector is equal to the cost parameter
#that we discussed earlier (remember c= 5)

#If we increase the cost parameter we can get fewer of these problem points
#but only at the cost of increasing our cross-validation error
#notice the inverse relationship between cost parameter and cross validation error.
#Increasing the cost parameter makes the cross validation error greater given that
#our model now is less able to generalize from the original data set.
#It can perfectly mimic the results in the original data set that has been trained with
#but it cannot find results with equal accuracy in future data sets which it will
#be tested on.

#increasing the cost parameter. Notice how this changes the cross validation and training errors:
svmOutput <- ksvm(type~., data=trainData,kernel="rbfdot",kpar="automatic",C=50,cross=3,prob.model=TRUE)
svmOutput

#Number of Support Vectors : 825 
#Objective Function Value : -7364.469 
#Training error : 0.011412 
#Cross validation error : 0.079881 

#pgraphing our support vectors
hist(alpha(svmOutput)[[1]], main= "Support Vector Histogram with C=50",xlab="Support Vector Values")

#Now there are only about 100 cases where the support vector is at the
#maxed-out value (in this case 50, because we set C = 50 in the ksvm()command).

#this is a nested list within a nested list . We are looking at the support vectors
#that were close zero meaning that these cases of spam emails were so easily distinguisable 
#from non-spam cases that the their features did not help much with our training algorithm 
#to find an optimized position for the hyperlane. The reason the number of these spam cases
#near he zero increased was that we increased our support vectors from the c =5 to c=50
alphaindex(svmOutput)[[1]][alpha(svmOutput)[[1]] < 0.05]
#[1]   64  315  415  431  540  627  962 1182 1382 1517 2012 2026 2809 3016

#Now let's take a closer look for the first case that our algorithm had an easy time
#picking it out as an spam. 
trainData[64,]
# type nonspam
#we can see what signs that this case had that made it east to identify (lower capital cases)
#specific mail address etc.)

#Now we can take an look a case that our algorithm had a hard time figuring out whether it was
#spam or not (higher number of support vectors), notice we choose cases that had highest number
#support vectors possible (50)
alphaindex(svmOutput)[[1]][alpha(svmOutput)[[1]] == 50]
#[1]   22   28   31   43   75   80  152  192  247  265  292  300  355  368  384  407  424

trainData[22,]
# we can see that the large number of capital letters in this case had made it hard for
#our cllasifier to know whether it was a spam or not (it was a spam)

#*************************************************************************************#
#*************************************************************************************#
#*************************************************************************************#

#now time to test our algorithm with the test data:
#The output from the predict() command is a two-dimensional numeric list. there are
#two lists of vote values side by side. Each list is 1,534 elements long,
#corresponding to the 1,534 cases in our testData object. The left-hand list
#has one (1) for a nonspam vote and zero (0) for a spam vote. Because this
#is a two-class problem, the other list has just the opposite. We can use
#either one because they are mirror images of each other.
svmPred<- predict(svmOutput,testData,type="votes")

#now we want to compare if the results generated by the algorithm matches the 
#results with original human vote 

# we made a data frame that has the columns of "type" from the test data
# and the results from the svmPred (in the first row and it contains ones
#for nonspam predictions and zeros for spam predictions.)
compTable <- data.frame(testData[,58],svmPred[1,])

#table uses the cross-classifying factors to build a contingency
#table of the counts at each combination of factor levels as matrix table
table(compTable)

#            svmPred.1...
#testData...58.   0   1
#       nonspam  36 882
#       spam    547  69

#So in this analysis we can see that there are 36 cases which were non-spams 
#but were classified as spam and 69 cases that were spams but were classified as 
#none-spams

#remember that the cross vlidation error was 0.079881. Now we can test that error rate
# with respect to testData using our contigency table:
(36+69)/1534
#[1] 0.0684485
#As you can see our test did slightly better than we thought by margin of 1 percent