#Stat 260 Final Project 
#Marisa, Chandler, Sophie, Viki
#December 10, 2019

#General Question: Which factors contribute to youth vape use? 

#open data set and import libraries 
library(party)
install.packages("tree")
library(tree)
install.packages("varhandle")
library(varhandle)
install.packages("rpart.plot")
library(rpart.plot)

d<- read.csv(file="NewYorkTobacco/NYYTS_2000_2018_PublicUse.csv", header = TRUE)

#Data Cleaning:

#Create new data frame with only variables we are observing - have limited variables greatly 
dnew<- data.frame(d$year, d$sex, d$X_grade, d$X_nyc, d$X_schlev, d$X_race2, d$moneyall2, d$livesmkr2, d$X_evape)
names(dnew)<-  c("year", "sex", "grade", "nyc", 'education', "race","money", "liveWithSmoker", "hasVaped")

#Fill in missing data with '' to create a new missing category - 999 is also used to indicate missing 
#The variables for our model will be categorical
dnew$sex[is.na(dnew$sex) == TRUE]<- ''
dnew$grade[is.na(dnew$grade) == TRUE]<- ''
dnew$nyc[is.na(dnew$nyc) == TRUE]<- ''
dnew$education[is.na(dnew$education) == TRUE]<- ''
dnew$race[is.na(dnew$race) == TRUE]<- ''
dnew$money[is.na(dnew$money) == TRUE]<- ''
dnew$liveWithSmoker[is.na(dnew$liveWithSmoker) == TRUE]<- ''
dnew$hasVaped[is.na(dnew$hasVaped) == TRUE]<- 999


#Actual values are written next to numbers
#Need to convert string values to ints for model (except for missing) 

dnew$sex[dnew$sex == '1']<- 1 #'female'
dnew$sex[dnew$sex == '2']<- 2 #'male'

dnew$nyc[dnew$nyc == '1']<- 1 #'yes'
dnew$nyc[dnew$nyc == '0']<- 0 #'no'

dnew$education[dnew$education == '2']<- 2 #'high school'
dnew$education[dnew$education == '1']<- 1 #'middle school'

#rename categorial variables to be more interpretable within data set
dnew$race[dnew$race == '1']<- 1 #'white only'
dnew$race[dnew$race == '2']<- 2 #'black or african american only'
dnew$race[dnew$race == '3']<- 3 #'hispanic or latino'
dnew$race[dnew$race == '4']<- 4 #'asian only'
dnew$race[dnew$race == '5']<- 5 #'hawaiian or pacific islander only'
dnew$race[dnew$race == '6']<- 6 #'american indian or alaska native only'
dnew$race[dnew$race == '7']<- 7 #'multiracial, other, or unspecified'

dnew$money[dnew$money == '1']<-  1 #'none'
dnew$money[dnew$money == '2']<-  2 #'less than $1'
dnew$money[dnew$money == '3']<-  3 #'$1 to $5'
dnew$money[dnew$money == '4']<-  4 #'$6 to $10'
dnew$money[dnew$money == '5']<-  5 #'$11 to $20'
dnew$money[dnew$money == '6']<-  6 #'$21 to $50'
dnew$money[dnew$money == '7']<-  7 #'$51 to $100'
dnew$money[dnew$money == '8']<-  8 #'$101 to $150'
dnew$money[dnew$money == '9']<-  9 #'$151 or more'
dnew$money[dnew$money == '.']<-  ''

dnew$liveWithSmoker[dnew$liveWithSmoker == '1']<- 1 #'yes'
dnew$liveWithSmoker[dnew$liveWithSmoker == '2']<- 2 #'no'
dnew$liveWithSmoker[dnew$liveWithSmoker == '.']<- ''

dnew$hasVaped[dnew$hasVaped == '1']<- 1 #'yes'
dnew$hasVaped[dnew$hasVaped == '0']<- 0 #'no'
dnew$hasVaped[dnew$hasVaped == '.']<- 999

#Need to convert each of the variables to factors for the tree 
dnew$money = as.factor(dnew$money)
dnew$race = as.factor(dnew$race)
dnew$education = as.factor(dnew$education)
dnew$sex = as.factor(dnew$sex)
dnew$grade = as.factor(dnew$grade)
dnew$liveWithSmoker = as.factor(dnew$liveWithSmoker)
dnew$nyc = as.factor(dnew$nyc)
dnew$year = as.factor(dnew$year)
dnew$hasVaped = as.factor(dnew$hasVaped)

#set final data set to year constraint of 2014-2018 for analysis
dfinal<- dnew[dnew$year == c('2014', '2015' , '2016', '2017' , '2018'),]
summary(dfinal)

#Sample sizes in each year - one row represents one response 
nrow(dfinal[dfinal$year=='2014',]) #1693
nrow(dfinal[dfinal$year=='2016',]) #1452
nrow(dfinal[dfinal$year=='2018',]) #1637

dfinal$hasVaped

#Total Responses
nrow(dfinal)

#Randomly sample 3000 responses 
indicesTrainingSet<- sample(1:nrow(dfinal), 3000, replace=FALSE)

#Split into training and testing groups 
train<-dfinal[indicesTrainingSet,]
test<-dfinal[-indicesTrainingSet,]

#There will be 3000 training points and 1,780 testing points 
nrow(train)
nrow(test)

#Building the model (2 Potential Versions): 

#First Version: RPart 

#Build the tree with the selected variables: 
rtree<- rpart(hasVaped~sex+grade+nyc+education+race+money+liveWithSmoker+year, data=train)
rtree
rpart.plot(rtree)

#Second Version: cTree

tree = ctree(hasVaped~sex+grade+nyc+education+race+money+liveWithSmoker+year, data=train)

#Export entire tree image 
jpeg("treeplotSimple.jpg", width = 3000, height = 3000)
plot(tree, type = "simple")
dev.off()
summary(tree)

#The tree's nodes tell us which factors contribute most to whether someone will have vaped or not 

#Validation: 
#Will choose whichever tree implementation yields better accuracy 

#Predicting values for the testing group: 
predictions = predict(tree, interval = "prediction",new = test)
length(predictions)
#Compare the predicted values with the actual values for test

#Trying to visually see the differences: 

#Predictions for test
plot(predictions)
#Actual values for test
plot(test$hasVaped)

#Results 
summary(test$hasVaped)
summary(predictions)

#Calculating Overall Accuracy:

#First need to unfactor the results for calculations 
testResults = unfactor(test$hasVaped)
predictionResults = unfactor(predictions)

#One way to validate: check which values are not equal on a 1:1 level 
boolres = testResults==predictionResults
correct = length(boolres[boolres == TRUE])
incorrect = length(boolres[boolres == FALSE])
total = length(boolres)

#Take the ratio of correctly predicted values / total predicted values 
accuracy = correct / total 
accuracy * 100

#Alternative validation method: take residuals of predicted vs. actual 

difference = testResults - predictionResults

plot(difference, main = "Residuals of Predicted vs. Actual Values", xlab = "Predictions", ylab = "Differences", col = "blue")

hist(difference)

correctlyPredicted = length(difference[difference == 0])
totalPredicted = length(difference)
accuracy2 = correctlyPredicted / totalPredicted 
accuracy2 * 100 

#Model currently has 75% prediction accuracy with ctree implementation 

#Plot showing vape use increasing over time 
plot(dfinal$year,dfinal$hasVaped, type="b", col = c("blue", "red", "dark gray"), main = "State of New York Teen Vape Use Overtime", xlab = "Years", ylab = "Has Vaped")



