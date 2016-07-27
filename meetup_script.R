#########################################################################
#########################################################################
#########################################################################
#                                                                       #
#         Fort Lauderdale Machine Learning Meetup Script                #
#                                                                       #
#########################################################################
#########################################################################
#########################################################################

#Please run the meetup_datasets.R file to load dfA and dfB data

#Print datasets
dfA
dfB 

#Q - How do I visualize data?
#install.packages("ggplot2", repos="http://cran.us.r-project.org")
library(ggplot2)
#Bar
ggplot(dfB, aes(x=Gender, y=Age)) + geom_bar(stat="identity")

#Scatter Plot
ggplot(dfB, aes(x=Income, y=Age)) + geom_point()

#Histogram/Density Plot
ggplot(dfB, aes(Income)) + geom_density()

#Q - How do I get help with a function?
?geom_density

#------------------------#
#Challenge 1





#------------------------#


#Q - How do we delete a column?
dfA = dfA[-3]

#Q - How do I get a frequency count?
table(dfA$Gender_F)

#Q - How do I recode a column?
dfA$Gender_F == "female" 
cbind(dfA, dfA$Gender_F == "female")

#Q How do I test the conditional OR?
dfA$Gender_F == "female" | dfA$Gender_F == "FEMALE"

#Create index with the %in% operator
myindex = dfA$Gender_F %in% c("female", "FEMALE", "F")

#Use the index to recode values
dfA$Gender_F[myindex] = "F"


#Q - How do I delete rows based on a condition?
#Identify index
tokeep = dfA$Gender_F != "99"

#Overwrite data frame
dfA = dfA[tokeep,]

#------------------------#
#Challenge 2





#------------------------#


#The name the gender column does not match the name of the other data frame
#Q7 - How do I check the names of the columns?
names(dfA)
#Check the names of 'dfB'


#Overwrite the second name
names(dfA)[2] = "Gender"

#Change the name of the third column to match dfB. Check the names after.


#There is a negative value in the Income column. We consider this an invalid entry.

#------------------------#
#Challenge 3
#Hint: To test if a number is negative we can use the structure x < 0
#Hint: It will help to create an index first, then extract using that index




#------------------------#


#Q - How do I multiply a column by a value?
dfA$Income = dfA$Income * 10000

#Q8 - How do I recode numeric to categorical variables?
dfA$Environmentalist = factor(dfA$Environmentalist, labels=c("No", "Yes"))


#Q How do I identify NA values?
missing_age <- is.na(dfA$Age)

#Create train and test sets
test = dfA[missing_age, ]
train = dfA[!missing_age, ]

#How do I complete Linear Regressions
mdl = lm(Age ~ Income, data=train)
age_predict = predict(mdl, test)

#Q How do I round numbers?
rnd_age = round(age_predict)
dfA$Age[missing_age] = rnd_age



#Classification
#Find missing Environmentalist answers
env_missing <- is.na(dfA$Environmentalist)
to_env_predict <- dfA[ env_missing, ]

#Unsupervised K-Means Clustering on Age and Income. Choose 2 centers
k_mdl <- kmeans(dfA[, c(3,4)], centers=2)
k_mdl$cluster[env_missing]

#RandomForest Supervised Classification
#install.packages("randomForest", repos="http://cran.us.r-project.org")
library(randomForest)
rand_mdl = randomForest(Environmentalist ~ Income, data=dfA, na.action=na.omit)
env_predict <- predict(rand_mdl, to_env_predict)

#Assign predictions to data
dfA$Environmentalist[ env_missing ] = env_predict

#Merge on Gender, Age, Environmentalist
#First create a bin for Age
dfA$Age = cut(dfA$Age, breaks=c(18, 35, 65, 99), labels=c("Millenial", "Middle", "Senior"))

#Do the same for dfB
dfB$Age = cut(dfB$Age, breaks=c(18, 35, 65, 99), labels=c("Millenial", "Middle", "Senior"))

#Final Task - Merge the datasets
dfmrg <- merge(dfA[-4], dfB, by=c("Age", "Gender", "Environmentalist"))


#Fusion Task Complete!