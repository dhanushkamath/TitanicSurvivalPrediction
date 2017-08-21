# Loading the data


train <- read.csv("train.csv",header=TRUE)
test  <- read.csv("test.csv",header=TRUE)


#Add a "Survived" variable (Extra column) to test data to combine both

test.survived <- data.frame(Survived=rep("None",nrow(test)),test[ ,])

#combine both the data sets
data.combined <- rbind(train,test.survived)

str(data.combined)

#factors are like enums. they can only take discrete values. the code below converts to factor

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)


#Survival rates and distribution across classes

table(data.combined$Survived)
table(data.combined$Pclass,data.combined$Survived) #Code that i figured out,which creates a table giving info of survival of each class

#Load up ggplot2 package for visualization

library(ggplot2)

#Hypothesis - Rich folks survived at a higher rate
#factor(survived) is used to convert survived which is int to factor EXPLICITLY, aes basically means aesthetics 

train$Pclass<-as.factor(train$Pclass)
ggplot(train, aes(x=Pclass, fill=factor(Survived)))+
  geom_histogram(binwidth=0.5)+xlab("Pclass")+ylab("Total Count")+
  labs(fill="Survived") 

#Display first few names in the training data set
head(as.character(train$Name))

#Numb of unique names both train and test
length(unique(as.character(data.combined$Name)))

#check results. There are 2 duplicate names. as the o/p of previous data is 1307 while there are 1309 names
#Get duplicates, store them as a vector for further analysis 
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#next, take a look at the records in the combined data set. 
#this displays the data of those passengers whose names are present in data.combined and dup.names
#basically, it is used to display the duplicated records. 
#now, as only the names are same, we are convinced that they are not duplicates or anomalies(check other data like age,ticket)
data.combined[which(data.combined$Name %in% dup.names),]

#validate that "Master" is a good proxy for male childen
boys<-data.combined[which(str_detect(data.combined$Name,"Master.")),]
summary(boys$Age)







#whats up with the Miss and Mr thing?
library(stringr)

#Any correlation with other variables(e.g, sibsp)?

misses<-data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

#Hypothesis-Name titles correlate with age
#Analysis mrs.
mrses<-data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]
 
#Check out males to see if pattern continues
males<-data.combined[which(train$Sex=="male"),]
males[1:5,]

#(My code) Plot classes vs survived for males.
ggplot(males, aes(x=as.integer(Pclass), fill=factor(Survived)))+
  geom_histogram(binwidth=0.5)+xlab("Pclass")+ylab("Total Count")+
  labs(fill="Survived") 


#expand upon the relationship between "Survived" And Pclass by adding the new title variable
#to the dataset and then explore a potential 3-dimensional relationship

#Create a function for extracting title.
# NOTE: This somehow works only if Mrs. is given above Mr.
extractTitle<-function(name)
{
  name<-as.character(name)
  
  if(length(grep("Master.",name))>0)
  return ("Master.")
  else if(length(grep("Miss.",name))>0)
   return("Miss.")
  else if(length(grep("Mrs.",name))>0)
    return("Mrs.")
  else if(length(grep("Mr.",name))>0)
    return("Mr.")
  else return("Other")
  
}
 
titles<-NULL

for(i in 1:nrow(data.combined)) {
  titles<-c(titles,extractTitle(data.combined[i,"Name"]))
  
}
data.combined$title<-as.factor(titles)


#Visualizing title and survival for each class
#We only use the first 891 rows as others are "None"
ggplot(data.combined[1:891,],aes(x=title,fill=Survived))+
  geom_bar()+facet_wrap(~Pclass)+
  ggtitle("Pclass")+xlab("Title")+ylab("Total Count")+labs(fill="Survived")


#visualizing gender and survival for each class
ggplot(data.combined[1:891,],aes(x=Sex,fill=Survived))+
  geom_bar()+facet_wrap(~Pclass)+ggtitle("Pclass")+xlab("Sex")+ylab("Total Count")+
  labs(fill="Survived")

summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

#visualizing age,gender and class
ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
  geom_histogram(binwidth = 10)+facet_wrap(~Pclass+Sex)+ggtitle("Pclass")+xlab("age")+ylab("Total Count")+
  labs(fill="Survived")


#Validate that master is a good proxy for male childern
boys<-data.combined[which(str_detect(data.combined$title)=="Master."),]
summary(boys$Age)

#Miss, is more complex because the age varies. lets analyze
misses<-data.combined[which(str_detect(data.combined$title)=="Miss."),]
summary(misses$Age)


#visualising survival of misses with age and class
ggplot(misses[misses$Survived != "None" & !is.na(misses$Age),], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


#checking for misses who are alone, ie parch=0 and sibsp=0
misses.alone<-misses[which(misses$SibSp==0&misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<=14.5))



#move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)

#check if sibsp can be a factor?
length(unique(data.combined$SibSp))
data.combined$Sibsp<-as.factor(data.combined$SibSp)


#We believe title is predictive. Visualize survival rates by sibsp,Pclass and title
ggplot(data.combined[1:891,], aes(x = Sibsp, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.Sibsp <- c(train$SibSp, test$Sibsp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.Sibsp + temp.Parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Take a look at the ticket variable
str(data.combined$Ticket)


# Based on the huge number of levels ticket really isn't a factor variable it is a string. 
# Convert it and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)


# OK, we can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of Pclass & title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")




# Next up - the Fares Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))


# Can't make Fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# Let's check to see if Fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")

# Analysis of the cabin variable
str(data.combined$Cabin)


# Cabin really isn't a factor, make a string and the display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]


# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# Add to combined data set and plot 
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon Pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# What about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))


# What about folks with multiple Cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#Lets see if port of embarkation has something to do with survivability
str(data.combined$Embarked)

#Lets plot it.
ggplot(data.combined[1:891,],aes(x=Embarked,fill=Survived))+geom_bar()+facet_wrap(~Pclass+title)+
  ggtitle("Pclass+title")+xlab("Embarked")+ ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")





#=====================================================================================================================================#


# EXPLORATORY MODELING WITH RANDOM FORESTS


library(randomForest)



#Train a random forest with the default parameters using pclass and title
rf.train.1<-data.combined[1:891,c("Pclass","title")]
rf.label<-as.factor(train$Survived)


#Set the seed to avoid getting a different result every time
set.seed(1234)
# importance = true, keeps track of the importance of each variable, ntree sets the number of trees(default is 500)
rf.1<-randomForest(x=rf.train.1,y=rf.label, importance = TRUE, ntree=1000)
rf.1
#Prints out the result and some extra details,OOB stands for out of bag, 

#(NOTE!)in the CONFUSION MATRIX,the true labels are the vertical column and the predicted labels are the horizontal column.(IMPORTANT!)

#A Way of plotting it. The more on the right the dot for a particular variable is, the more important it is.
varImpPlot(rf.1)





#(This line of code was added because SibSp was not factor, and as a result oob error rate was different. YES THAT CAN AFFECT THE ERROR)
data.combined$SibSp<-as.factor(data.combined$Sib)



#Train a random forest with the default parameters using pclass,title and sibsp
rf.train.2<-data.combined[1:891,c("Pclass","title","SibSp")]
set.seed(1234)
rf.2<-randomForest(x=rf.train.2,y=rf.label,importance = TRUE, ntree=1000)
rf.2
varImpPlot(rf.2)



#Train a random forest using Pclass,title, parch
rf.train.3<-data.combined[1:891,c("Pclass","title","Parch")];
set.seed(1234) #This line is pretty important
rf.3<-randomForest(x=rf.train.3,y=rf.label,importance=TRUE,ntree = 1000)
rf.3
varImpPlot(rf.3)



#Train a random forest using Pclass,title,parch and sibsp
rf.train.4<-data.combined[1:891,c("Pclass","title","Sibsp","Parch")]
set.seed(1234)
rf.4<-randomForest(x=rf.train.4,y=rf.label,importance=TRUE,ntree=1000)
rf.4
varImpPlot(rf.4)


#Train a random forest using Pclass,title and family size

rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train a Random Forest using pclass, title, sibsp, & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "title", "Sibsp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)



# Train a Random Forest using pclass, title, parch, & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)


#===========================================================================================================================
# CROSS VALIDATION

#subset our test records and features

test.submit.df<-data.combined[892:1309,c("Pclass","title","family.size")]

#Make predictions
rf.5.preds<-predict(rf.5,test.submit.df)
table(rf.5.preds)



#Create a CSV file for submission
#First make it a data frame
# dont forget to put row.names=false
submit.df<-data.frame(PassengerId=rep(892:1309),Survived=rf.5.preds)
write.csv(submit.df,file="RF_SUB_20170124_1.csv",row.names=FALSE)



#==============================================================================


#CROSS VALIDATION



# Our submission scores 0.79426, but the OOB predicts that we should score 0.8159.
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates\
# doSNOW helps in utilizing all the cores of your CPU, and 
library(caret)
library(doSNOW)


# Research has shown that 10-fold CV repeated 10 times is the best place to start,
# however there are no hard and fast rules - this is where the experience of the 
# Data Scientist (i.e., the "art") comes into play. We'll start with 10-fold CV,
# repeated 10 times and see how it goes.


# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training set. This
# is known as stratified cross validation and generally provides better results.

#CreateMultiFolds just returns the indices.
#Note: We pass the rf.label, which is the one that contains survived(1s and 0s)
set.seed(2348)
cv.10.folds<-createMultiFolds(rf.label,k=10,times=10)

# We know that in our dataset, 2/3rd perished, and 1/3rd survived. 
#So the data is more skewed to the perished side. STRATIFICATION ensures that
#the data taken randomly for CV will always have this ratio. And this is automatically
#done by the functions in Caret.

 
#Check Stratification.
table(rf.label)
342/549 #The result

#Check stratification of the cv.10.folds created above.
table(rf.label[cv.10.folds[[33]]]) #Randomly taken 33
308 / 494

#These 2 check stratification code is just to check the stratification of
#Caret functions. Basically, it works and you dont need this code. This was just
#proof that it works


# 10 fold CV
#Setup caret's TrainControl object per above
ctrl.1<-trainControl(method="repeatedcv",number=10,repeats=10,index=cv.10.folds)



#Setup doSNOW package for multicore training. This speeds up everything
#as we're going to be needing a lot of trees

cl<-makeCluster(6,type="SOCK") 

#now we have to register it so that Caret knows that we'll be using parallel processing
registerDoSNOW(cl)

#Setup seed for reroducability and train
set.seed(34234)
rf.5.cv.1<-train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,ntree=1000,trControl=ctrl.1)



#Shutdown cluster
stopCluster(cl)



#The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough.

#Let's try 5-fold CV repeated 10 times.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3

#==========================================================================================================================

# EXPLORATORY MODELING - 2

# SINGLE DECISION TREES

#==============================================================================================================================

#Single decision trees give us an insight to whats actually happening as they are much
#more simpler than random forests. Random forests are much more powerful. Single trees are
#much more easier to understand.
#It helps in feature selection.
#It helps in visually understanding the importance of features.

#Install packages rpart and rpart.plot
 #install.packages("rpart")
 #install.packages("rpart.plot")

library(rpart)
library(rpart.plot)


#According to the CV done above, it is seen that a 3 fold CV repeated 10 times gives the least accuracy
#Well, in CV, its better to be a bit pessimistic as the accuracy obtained was closer to the actual score we got on 
#kaggle. Hence we use a 3 fold CV repeated 10 times.



#Create utility function (A function that we define) named as rpart.cv 

rpart.cv<-function(seed,training,labels,ctrl)
  {
  
  cl<-makeCluster(6,type="SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  
  rpart.cv<-train(x=training,y=labels,method="rpart",tuneLength=30,trControl=ctrl)
  
  
  stopCluster(cl)
  return (rpart.cv)
}

#Grab features
features<-c("Pclass","title","family.size")
rpart.train.1<-data.combined[1:891,features]




#Run CV with the above defined functions and check results
rpart.1.cv.1<-rpart.cv(94622,rpart.train.1,rf.label,ctrl.3)
rpart.1.cv.1

#Plot the decision tree with PRP (plot rpart)
prp(rpart.1.cv.1$finalModel,type=0,extra=1,under=TRUE)


# The plot bring out some interesting lines of investigation. Namely:
#      1 - Titles of "Mr." and "Other" are predicted to perish at an 
#          overall accuracy rate of 83.2 %.
#      2 - Titles of "Master.", "Miss.", & "Mrs." in 1st & 2nd class
#          are predicted to survive at an overall accuracy rate of 94.9%.
#      3 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes equal to 5, 6, 8, & 11 are predicted to perish
#          with 100% accuracy.
#      4 - Titles of "Master.", "Miss.", & "Mrs." in 3rd class with 
#          family sizes not equal to 5, 6, 8, or 11 are predicted to 
#          survive with 59.6% accuracy.



#Lets see if we can find something more in the name of a passenger as the first condition in the decision tree is not so satisfying


table(data.combined$title)

data.combined[1:25,"Name"]

library(stringr)


# '[' is the indexing operator (I think it applies index or numbers to the data). 
#For eg, the below code splits names into two parts. The indexing operator indexes the 2 parts as 
#'1' and '2' so that we can access them ( I THINK, NOT SURE)
#The use of sapply eliminates the use of a for loop ,check help. Here we are applying the indexing operator.
#str_split splits the data according to a string or a character
name.splits<-str_split(data.combined$Name,",")
name.splits[1]
last.names<-sapply(name.splits,"[",1)
last.names[1:10]


# Add last names to dataframe 
data.combined$last.name<-last.names

#Now for titles.
#The line below says "Use sapply over name.splits, use indexing operator, take the 2nd part and split it using spaces with str_split
name.splits<-str_split(sapply(name.splits,"[",2)," ")
titles<-sapply(name.splits,"[",2)
unique(titles)

#So, now we have other titles as well such as Rev,Dr,Col etc.
#Also, there is one title which is "the"
data.combined[which(titles=="the"),]

#In the previous model we had not taken into account all the possible titles. This can cause a lot of errors. 
#Eg, "Miss" and "Ms." are the same thing and in the previous model 'Ms." was in the "Other" category




#Combining certain titles (Of the same type, such as nobility, military officers)

#Explaining this line of code below : If the value in titles match with any of the values "in" (Hence the "%in%") the list 
#of Dona and The, then replace that with Lady
#The below code can be done with WHICH : which(titles=="Dona"|titles=="the)
titles[titles %in% c("Dona.","the")]<-"Lady."
titles[titles %in% c("Ms.","Mlle.")]<-"Miss."
titles[titles %in% c("Mme.")]<-"Mrs."
titles[titles %in% c("Jonkheer.","Don.")]<-"Sir."
titles[titles %in% c("Col.","Capt.","Major.")]<-"Officer"
table(titles)




#Make it a factor and combine it with data.combined

data.combined$new.title<-as.factor(titles)
table(data.combined$new.title)


#Visualizing with the new title
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Survival plot with new titles") +
  xlab("New titles") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Collapsing titles based on visual analysis to avoid overfitting
#Lady is joined with Mrs.
#All other adult male new titles are joined to Mr.
indexes<-which(data.combined$new.title=="Lady.")
data.combined$title[indexes]<-"Mrs."

indexes<-which(data.combined$new.title=="Dr." |
                 data.combined$new.title=="Rev."|
                 data.combined$new.title=="Officer"|
               data.combined$new.title=="Sir.")

data.combined$title[indexes]<-"Mr."

#At this stage, data.combined$title still had 4 entries in "Other". Rectified it individually.
 data.combined$title[370]<-"Mrs."
data.combined$title[444]<-"Miss."
data.combined$title[642]<-"Miss."
data.combined$title[980]<-"Miss."

#plotting the new modified and improved data.

ggplot(data.combined[1:891,],aes(x=title,fill=Survived))+
  geom_bar()+facet_wrap(~Pclass)+ggtitle("New and Improved Title")


#USING CV on the new improved data
featues<-c("Pclass","title","family.size")
rpart.train.2<-data.combined[1:891,featues]

#Invoking the CV utility function that was defined above
rpart.2.cv.1<-rpart.cv(94622,rpart.train.2,rf.label,ctrl.3)
rpart.2.cv.1

#There is an increase in accuracy by a little over 1%

#Now lets visualise the new decision tree
prp(rpart.2.cv.1$finalModel,type=0,extra=1,under=TRUE)

#Well, the way this decision tree predicts that everyone with title "Mr." dies, is pretty absurd.
#Improving the above mentioned.
#Analysis Mr in 1st class

indexes.first.mr<-which(data.combined$title == "Mr." & data.combined$Pclass == 1)
first.mr.df<-data.combined[indexes.first.mr,]
summary(first.mr.df)

#There is one female.
first.mr.df[first.mr.df$Sex=="female",]


#updating the title.
indexes<-which(data.combined$title=="Mr." & data.combined$Sex=="female")
data.combined$title[indexes]<-"Mrs."

#Checking for other gender problems
length(which(data.combined$Sex=="female" & (data.combined$title=="Master."|data.combined$title=="Mr.")))

#going back to analysis mr. in first class
indexes.first.mr<-which(data.combined$title=="Mr." & data.combined$Pclass==1)
first.mr.df<-data.combined[indexes.first.mr,]

summary(first.mr.df[first.mr.df$Survived==1,])
View(first.mr.df[first.mr.df$Survived==1,])

#Taking a closer look at the really expensive tickets that we just saw, using ticket id

indexes<-which(data.combined$Ticket=="PC 17755"|
                 data.combined$Ticket=="PC 17611"|
                 data.combined$Ticket=="113760")
View(data.combined[indexes,])

#It is observed that there are 4 tickets with 512 dollars, but family siize is not 4, and the cabin is entirely different from the other 2
#which indicates that the other 2 would be maids/Valet

#It is observed that all Mr. in the above shown subset survive. so our current model fails for Mr.


# DENSITY PLOT
# Its kind of like an alternative to histrogram when the data is continuous and not discrete.
# Fare is continuous, we can't treat it as a factor. So we use density plot

ggplot(first.mr.df,aes(x=Fare,fill=Survived))+geom_density(alpha=0.5)+ggtitle("Survival of 1st class Mr with Fare")


#Engineering features based on all the passengers with the same ticket ID (Modifying fares by taking average fare)
ticket.party.size<-rep(0,nrow(data.combined))
avg.fare<-rep(0.0,nrow(data.combined))
tickets<-unique(data.combined$Ticket)


for( i in 1:length(tickets))
{
  current.ticket<-tickets[i]
  party.indexes<-which(data.combined$Ticket==current.ticket)
  current.avg.fare<-data.combined[party.indexes[1],"Fare"]/length(party.indexes)
  


for(k in 1:length(party.indexes))
  {
  ticket.party.size[party.indexes[k]]<-length(party.indexes)
  avg.fare[party.indexes[k]]<-current.avg.fare
  }
}

data.combined$ticket.party.size<-ticket.party.size
data.combined$avg.fare<-avg.fare

#Take a look at the new columns in data.combined by opening it.

#Take a look at the summary of the modified first class Mr data
first.mr.df<-data.combined[indexes.first.mr,]
summary(first.mr.df)

#Visualize survivability of 1st class Mr with ticket.party.size
ggplot(first.mr.df[first.mr.df$Survived!="None",],aes(x=ticket.party.size,fill=Survived))+
geom_density(alpha=0.5)+ggtitle("Survival of 1st Mr by ticket.party.size")




#Visualize survivability of 1st class Mr with average fare
ggplot(first.mr.df[first.mr.df$Survived!="None",],aes(x=avg.fare,fill=Survived))+
  geom_density(alpha=0.5)+ggtitle("Survival rates 1st class Mr By average fare")



#Hypothesis - ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)


#One missing value, take a look
data.combined[is.na(data.combined$avg.fare),]

#A simple method to calculate the NA value. Another alternative would be to build an  ML model to computer the age
#But we only have one NA, so its not required.
#So take a look at other similar records

# NOTE : USE OF WITH STATEMENT. 
indexes<- with(data.combined, which(Pclass =="3" & title=="Mr." & family.size==1 & Ticket!="3701"))
similar.na.passengers<-data.combined[indexes,]
summary(similar.na.passengers$avg.fare)
#The above mentioned method for filling up of data is pretty IMPORTANT. NOTE.
#Eg. Check out the summary of age, this method cant be applied over there as there are 263 NAs. 
#use median. which is 7.840
data.combined[is.na(avg.fare),"avg.fare"]<-7.840


#Now we have all the data to do correlation between number of people traveling on a ticket with the avg fare

# NORMALIZATION

#some algorithms like SVMs require normalization. So normalization can be pretty important.
#Normalizing data using Caret's preProcess.
#Caret's preProcess is pretty cool. It gives a number of algorithms to preProcess your data. Open up the help and find out.
# "Center" and "Scale" is commonly used for normalization. It does the exact the same thing Andrew Ng was talking about.
# The default is Center and Scale.

preproc.data.combined<-data.combined[,c("ticket.party.size","avg.fare")]
preProc<-preProcess(preproc.data.combined,method=c("center","scale"))

#Just like the steps followed for the predictive models above, the above code creates an object for performing the tast.
#Now to perform it we have to use the Predict function
postproc.data.combined<-predict(preProc,preproc.data.combined)

#Now, to check the correlation we use the cor function. 
#The output from cor will be between -1 and 1. -1 means they are perfectly negatively correlated.
# +1 means they are perfectly positively corelated.0 means they are un-corelated
cor(postproc.data.combined$ticket.party.size,postproc.data.combined$avg.fare)
#the values obtained is 0.09. Which means they are almost perfectly uncorelated.
#Which is really good,as we now have 2 new features which could possibly help in prediction.

#Checking the same correlation, but only for first class
indexes<-which(data.combined$Pclass==1)
cor(postproc.data.combined$ticket.party.size[indexes],postproc.data.combined$avg.fare[indexes])
#The value is 0.2 which is quite a bit higher, but still its pretty low and these 2 features can be taken as potentially 2 good features for prediciton.

# SO REMEMBER: CHECKING FOR CORRELATION REQUIRES NORMALIZATION

#Checking if all this feature engineering made any difference.
features<-c("Pclass","title","family.size","ticket.party.size","avg.fare")
rpart.train.3<-data.combined[1:891,features]

#Running CV and checking results.
rpart.3.cv.1<-rpart.cv(94622,rpart.train.3,rf.label,ctrl.3)
rpart.3.cv.1
#The accuracy is now 83.15%. So there is an increase by about 2%.

#Plot
prp(rpart.3.cv.1$finalModel,type=0,extra=1,under=TRUE)

#Analysing this plot and the previous plot its obvious that this new plot can lead to lesser overfitting than the previous one (which checks if family size is 5,8,11 etc. ie, for specific values)
#it is also seen that family.size is no longer taken.

#unfortunately, the prediction of Mr is still the same.



#===================================================================================================================================================================================================================================================
 
#SUBMISSION AND FINAL ANALYSIS


#Rpart scores 80.383 percentage in Kaggle

#Making csv file.

features<-c("Pclass","title","family.size","ticket.party.size","avg.fare")
testsubmit.decisionTrees<-data.combined[892:1309,features]

#Make predictions.
#"type" in the below code specifies the kind of prediction you want, such as class for classification. check help for other types like probability prediction.
rpart.3.preds<-predict(rpart.3.cv.1$finalModel,testsubmit.decisionTrees,type="class")
table(rpart.3.preds)

#Checking the sample submission of Kaggle, the csv file should be in a particular format : "PassengerID" and "Survived"
#REMEMBER TO PUT ROW.NAMES = FALSE BELOW.
submit.df<-data.frame(PassengerId=rep(892:1309),Survived=rpart.3.preds)
write.csv(submit.df,file="RPART_SUB_20170204_01.csv",row.names=FALSE)


#TRYING RANDOM FOREST
#Random Forest scores 80.861
#random forest has weake feature selection. Decision trees have strong feature selection. It was observed that family.size was not used in the decision tree.
#As random forests have weaker feature selection,we omit family.size  
features<-c("Pclass","title","ticket.party.size","avg.fare")
rf.train.temp<-data.combined[1:891,features]

#Using the same seed,
set.seed(1234)
rf.temp<-randomForest(x=rf.train.temp,y=rf.label,ntree=1000)
rf.temp

testsubmit.randomForest<-data.combined[892:1309,features]


rf.preds<-predict(rf.temp,testsubmit.randomForest)
table(rf.preds)
#Writing CSV file

submit.RFfinal<-data.frame(PassengerId=rep(892:1309),Survived=rf.preds)
write.csv(submit.RFfinal,file="RForest_SUB_20170204_01.csv",row.names=FALSE)


#===================================================================================================================================================================================

#MUTUAL INFORMATION
#Read about mutual information. Watch some videos on YouTube.


#Further, to improve the model. Check the places where the model fails and dive deep into them.
#install.packages(infotheo)
library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$Sibsp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$cabin.multiple[1:891]) 
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))


#It is seen that title has the highest value.

# TIP : Start with visual analytics rather than mutual information.

#===========================================================================================================================================================================================

# RTSNE 
#Dimensionality reduction.
#Each variable is actually a dimension. Visualizing more than 3 dimensions is pretty difficult.
#Using facet_wrap for a lot of variables on Bar graphs and histograms is not recommended and will break down at some point.
#So we use rtsne to visualize more dimensions easily and this is done on a 2D plane.
#install.packages(Rtsne)

library(Rtsne)

#Lets analyze the part of the model, where we're pretty confident. So, all the data other than Mr.
#Below 2 lines code are used to extract all data other than Mr.(1st Line) in the training dataset(2nd Line)
most.correct<-data.combined[data.combined$title!="Mr.",]
indexes<-which(most.correct$Survived!="None")


#tsne is an algorithm that is probably used for dimensional redution(just google it)
#'Features' has been defined above (Pclass, title, avg.fare, fare.party.size)
#NOTE: Rtsne needs a seed to ensure consistent output between runs.
#set.seed(984357)
tsne.1<-Rtsne(most.correct[,features],check_duplicates = FALSE) #RTSNE throws in an error if there are duplicates. But we have records where a lot of values might be the same. Hence check_duplicates = FALSE

#tsne.1 has a variable called Y which is basically the x,y co-ordinates (Not only the Y co-ordinate, check below using dim())
dim(tsne.1$Y) 
#Answer is 527 x 2. Therefore x and y coordinate columns.
#Now plot this.

#the 1 in "indexes,1" in the code below , indicates "Take the first column" (x co-ordinate) similarly for "features,2" (y co-ordinate)
set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")

#USE CONDITIONAL MUTUAL INFORMATION FOR MORE THAN ONE VARIABLE. (SIMILAR TO FACET_WRAP)
# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,])) #'indexes' is DEFINED a little above.
# The above line means, out of the most correct data frame, get 'Survived' and then, grap the Y value (which are both the x,y co-ordinates),descretize them(as they are not descrete)
# and show me, if i have 2 descrete x,y coordinates, show me how good i will be. 




#Now analyze the part where our model has potential for fairling. i.e, 'Mr'
# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
# Taking Mr, and indexes where Survived=None
misters <- data.combined[data.combined$title == "Mr.",]
indexes <- which(misters$Survived != "None")


tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")

#from the plot It is pretty obvious that the alogrithm was predicting that Mr preish. There clusters are predominantly red.

# Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))

#
# Idea - How about creating tsne featues for all of the training data and
# using them in our model?
#

#Use TSNE for the entire dataset (TRAINING AND TEST) rather than just the "None" as before.
# NOTE : BUT, FOR THE PLOT, ONLY TRAINING DATA IS USED.
# The reason is Consistency.We want to map the entire collection of features of both training and test into the same 2 dimensional feature space.
# IMP : This is one of the reasons why data.combined was created. So that any global feature engineering is applied to both training and test.
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE) 
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))
#the result shows that its pretty powerful. 

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]







