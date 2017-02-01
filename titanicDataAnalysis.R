#################################################
#####TITANIC: MACHINE LEARNING FROM DISASTER#####
################# Paul Le Ster ##################
#################################################

#helper functions
library(stringr)
library(ggplot2)
library(randomForest)
library(caret)
library(doSNOW)
library(rpart)
library(rpart.plot)



source("titanicHelper.R")

#Read in train and test data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Combine test set and train set
test.survived <- data.frame(Survived = rep("None", nrow(test)),test[,])
data.combined <- rbind(train, test.survived)
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)



###################################
#Part 1: Understanding the data
#Goal: Test some preliminary hypotheses, identify important features
###################################

#survival rate = 342/(342+549) = 38.38%
table(data.combined$Survived)


#lets do some visualizations


#Hypothesis 1: Rich people were more likely to survive than poor people, assuming PClass is a proxy of wealth
#Result: Confirmed, but result isn't as stark as one might expect. 20% survival in 3rd class vs 60% survival in 1st class
ggplot(data.combined[1:891,], aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width = 0.5) + 
  xlab("Wealth v Survival") +
  ylab("Total Count") +
  labs(fill = "Survived")


#Hypothesis 2: Women more likely to survive than men
#Result: Confirmed, but still looks like there is room for improvement. Wonder if wealthy women had an advantage...
ggplot(data.combined[1:891,], aes(x = Sex, fill = factor(Survived))) + 
  geom_bar(width = 0.5) + 
  xlab("Gender v Survival") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Hypothesis 3: Wealthy women more likely to survive than men
#Result: Confirmed, to a high degree. It looks like if you were a female in 1st or 2nd class
ggplot(data.combined[1:891,], aes(x=Sex,fill=Survived))+
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("GenderxWealth v Survival")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")

#Hypothesis 4: Children more likely to survive than men and survive roughly as much as women
#Result: Confirmed, to a high degree. It looks like if you were a female in 1st or 2nd class


#Plotting out a histogram in this way doesn't really get me what I want. Let's try another proxy
ggplot(data.combined[1:891,], aes(x=Age,fill=Survived))+
  geom_histogram(binwidth = 1) +
  ggtitle("Age")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")

#what about your title
titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles,getTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)


#Hypothesis 4 again: Children more likely to survive than men and survive roughly as much as women
#Result: Confirmed, to a high degree. If you are a female or child in PClass 1 or 2, you are basically guaranteed survival

#Validate Assumption: Master is male child
summary(data.combined[which(data.combined$title=="Master."),])


ggplot(data.combined[1:891,], aes(x=title,fill=Survived))+
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill="Survived")



#Let's pause from intuitive hypotheses - why don't we do a quick scan of mutual information "scores"
#Result: Title has the highest mutual information, which confirms our hypothesis that we're going down the right path
#Other key variables look to be Sex, Age, Fare, PCLass - all in line with our thinking

str(data.combined)
mutinformation(as.factor(data.combined$Survived[1:891]), data.combined$Pclass[1:891])
mutinformation(as.factor(data.combined$Survived[1:891]), data.combined$Sex[1:891])
mutinformation(as.factor(data.combined$Survived[1:891]), data.combined$Age[1:891])
mutinformation(as.factor(data.combined$Survived[1:891]), data.combined$SibSp[1:891])
mutinformation(as.factor(data.combined$Survived[1:891]), data.combined$Parch[1:891])
mutinformation(as.factor(data.combined$Survived[1:891]), discretize(data.combined$Fare[1:891]))
mutinformation(as.factor(data.combined$Survived[1:891]), data.combined$title[1:891])

#Hypothesis 5- having a large family increased your chance of perishing, as harder to keep track of many
#Result: Sort of confirmed, but data isn't clear. To be investigated further later
data.combined$family.size<- as.factor(c(train$SibSp,test$SibSp)+c(train$Parch,test$Parch)+1)
ggplot(data.combined[1:891,], aes(x=family.size,fill=Survived))+
  geom_bar() +
  ggtitle("Age")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")


###################################
#Part 2: Make initial predictions using Random Forests
#Goal: Train RFs based on what we understoof about the data in part 1
###################################

rf.label <-as.factor(train$Survived)

#RF.1 - PClass, Sex
#Results: Not bad, seems indicative of our hypothesis that wealth + gender greatly affects survival
rf.train.1 <- data.combined[1:891, c("Pclass","Sex")]
set.seed(1)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

#RF.2 - PClass, title
#Results: Much improved from RF.1 - this is in line that our hypothesis male children (title master) also had a survival advantage
rf.train.2 <- data.combined[1:891, c("Pclass","title")]
set.seed(1)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)


#RF.3 - PClass, family.size
#Results: Not great, comparatively, but in line with what we expected from our initial data exploration
rf.train.3 <- data.combined[1:891, c("Pclass","family.size")]
set.seed(1)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# RF.4 - PClass, title, family.size
#Results: Very strong, 2% improvement on just PClass + Title - thought: are we overfitting?
rf.train.4 <- data.combined[1:891, c("Pclass","title", "family.size")]
set.seed(1)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)


#############################################
#Initial Submission using RF.4
test.only <- data.combined[892:1309, c("Pclass","title", "family.size")]
rf.4.predictions <- predict(rf.4, test.only)
submit <- data.frame(PassengerId = rep(892:1309), Survived = rf.4.predictions)
write.csv(submit,file = "submission_v1.csv", row.names = FALSE)
#Results:0.79426 - This is lower than our OOB estimate, which makes me wonder if we are overfitting. Let's investigate with Cross Validation
##############################################


###################################
#Part 3: Cross Validation 
###################################


#cross validation

#10 fold cross validation, repeated 10 times
#split data into 10 chunks, take one chunk and use as test set to train the remaining 9
#and iterate

#stratified CV - make sure that ratio of survived and perished is same
#across all chunks

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k=10, times = 10)

#check stratification
table(rf.label)

table(rf.label[cv.10.folds[[33]]])

#set up caret's trancontrol object per above
ctrl.1 <- trainControl(method="repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

#set up doSNOW package for multi core training
cl<- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

#set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.1)

#shut down cluster
stopCluster(cl)

#results
rf.5.cv.1


#this is still too optimistic. lower than what i got on kaggle submission
# I might still be overfitting
# repeat cross val with smaller number of folds

set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k=5, times = 10)

#set up caret's trancontrol object 
ctrl.2 <- trainControl(method="repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

#set up doSNOW package for multi core training
cl<- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

#set seed for reproducibility and train
set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.2)

#shut down cluster
stopCluster(cl)

#results
rf.5.cv.2

#let's try 3 fold

set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k=3, times = 10)

#set up caret's trancontrol object 
ctrl.3 <- trainControl(method="repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

#set up doSNOW package for multi core training
cl<- makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

#set seed for reproducibility and train
set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 64, trControl = ctrl.3)

#shut down cluster
stopCluster(cl)

#results
rf.5.cv.3

#so going forward, our explaratory modeling is going to be based on 3 fold CV repeated 10 times, 
#THis is where we will determine if this feature improves our random forest? yes or no

###################################
#Part 4: More Feature Engineering
###################################

#more exploratory modelling
#Moving forward we're going to use 3 fold cross validation repeated 10 times
#we will generate a single tree to try and understand our features better 


#Tree #1 - Pclass, title, family.size

rpart.cv <- function(seed, training, labels, ctrl){
  cl<- makeCluster(4,type = "SOCK")
  registerDoSNOW((cl))
  set.seed(seed)
  rpart.cv <- train(x=training, y = labels, method= "rpart", tuneLength = 30, trControl = ctrl)
  stopCluster(cl)
  return(rpart.cv)
}

#grab features from best RF, run single tree for further investigation
features <- c("Pclass","title","family.size")  
rpart.train.1 <-data.combined[1:891,features]
rpart.1.cv.1 <- rpart.cv(1, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#display tree
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#Takeaways from the tree
# 1) What's going on with "Other" in the title. Let's do some digging and figure out if we can refine
# 2) If you are a female or child and you are not in 3rd class, you survive 95% of the time. Let's focus our efforts elsewhere
# 3) Family size seems arbitrary when seen this way - looks like we are probably overfitting

#Action item #1 - knock out the "Others" in title
temp.title <-str_split(sapply(str_split(data.combined$Name,","),"[", 2)," ")
titles <- sapply(temp.title, "[",2)
unique(titles)
#A little online research yields that Mlle, Dona, etc are foreign names for Mr, Mrs, etc
#For simplicity, let's group all the females under Mrs


#looking for something that could refine our other results
#look at SIr, The, Dona - these could be titles that change results
#some indicate gender, which is important for us. 


#title fixing
#putting all elderly females under Mrs - data exploration does not indicate any major trends
#putting all younger ladies under Miss
#Putting all male titles under Mr - data exploration does not indicate any major trends
titles[titles %in% c("Ms.","Mlle.")] <- "Miss."
titles[titles %in% c("Mme.","Lady.","Dona.","the")] <- "Mrs."
titles[titles %in% c("Jonkheer.","Don.", "Col.","Capt.", "Major.","Rev.", "Sir.","Dr.")] <- "Mr."
#pop into data.combined and correct for any females that may have male titles
data.combined$title.2 <- as.factor(titles)
indexes <- which(data.combined$title.2 == "Mr." & data.combined$Sex == "female")
data.combined$title.2[indexes] <- "Mrs."
table(data.combined$title.2)

#visually check - yep, Mrs, Miss, Master in 1,2 class overwhelmingly survive
ggplot(data.combined[1:891,], aes(x=title.2,fill=Survived)) +
  geom_bar() + 
  facet_wrap(~Pclass) +
  ggtitle("survival rates for new titles by pclass")

#Tree #2 - Pclass, title.2, family.size
features <- c("Pclass","title.2","family.size")
rpart.train.2 <- data.combined[1:891,features]
rpart.2.cv.1 <-rpart.cv(1,rpart.train.2,rf.label,ctrl.3)
rpart.2.cv.1
prp(rpart.2.cv.1$finalModel,type= 0, extra = 1, under = TRUE)

#Takeaways from the tree
# 1) "Other" title resolved. It now appears that we have a clean cut between males and everyone else
# 2) Family size seems arbitrary when seen this way - looks like we are probably overfitting - let's focus on this now




#poking through data, we see that our family size feature is off. There does seem to be some mild correlation, but it may be worth it to put aside and try something new
#In a similar vein, lets look at party size. Data exploration suggests that parties travelling together have the same ticket number
#Further, this gives us a method for calculating Fare in an interesting way, which was previously dismissed because it was the Fare of the entire party

party.size <- rep(0,nrow(data.combined))
average.party.fare <- rep(0.0, nrow(data.combined))

tickets.unique <- unique(data.combined$Ticket)

for(i in 1:length(tickets.unique)){
  current <- tickets.unique[i]
  indexes <- which(data.combined$Ticket == current)
  current.average.fare<-data.combined[indexes[1],"Fare"]/length(indexes)
  
  for(k in 1:length(indexes)){
    party.size[indexes[k]] <- length(indexes)
    average.party.fare[indexes[k]] <- current.average.fare
  }
  
  
}

data.combined$party.size <- party.size
data.combined$average.party.fare <- average.party.fare
View(data.combined)
#hacky fix for missing value
data.combined[is.na(average.party.fare), "average.party.fare"] <- 7.840


#let's see if feature engineering was good

features <- c("Pclass", "title.2", "party.size", "average.party.fare")
rpart.train.3 <-data.combined[1:891, features]

rpart.3.cv.1 <-rpart.cv(1, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

prp(rpart.3.cv.1$finalModel, type =0, extra = 1, under = TRUE)


#RF.5 - Plug these new engineering features into our random forest

features <- c("Pclass", "title.2", "party.size", "average.party.fare")
rf.train.5 <-data.combined[1:891, features]
set.seed(1)
rf.5 <- randomForest(x=rf.train.5, y=rf.label, ntree=1000)
rf.5
 
#############################################
#Final Submission using RF.5
test.only <- data.combined[892:1309, c("Pclass","title.2", "party.size", "average.party.fare")]
rf.5.prediction <- predict(rf.5, test.only)
table(rf.5.prediction)
submit <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.prediction)
write.csv(submit,file = "submission_v2.csv", row.names = FALSE)
#Results:0.80383 - mprovement over our last result of 0.79426 + plus hypothesize that it will do better at conclusion of competition as used cross validation instead of just OOB
##############################################

#glaring area where model is messing up is the males. Just don't have good features to understand whether an adult males was likely to survive


rich.guys.i <- which(data.combined$title.2 == "Mr." & data.combined$Pclass == "1" & )
rich.guys <- data.combined[rich.guys.i, ]
summary(rich.guys)

ggplot(rich.guys[rich.guys$Survived != "None",], aes(x = average.party.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by average fare")

#Unsure how to move forward. Doesn't seem to be any clear indicators of propensity to survive with first class male adults
#Hypothesis - Could cabin placement be an issue? Whereas women and children almost definitely got seats, could it have been on a first-come-first serve basis for men, 
#meaning that the closest cabins got lifeboats first? Seems extremely unlikely but who knows.

