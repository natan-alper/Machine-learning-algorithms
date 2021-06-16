## DOWNLOAD and IMPORT the Churn.csv dataset.
## First let's manually code a way to compute Entropy

## Note that we first need to compute entropy for the entire dataset (without segmentation)
## -p1*log(p1)-p2*log(p2) -...(if more than 2 classes)
EntropyWhole= -sum(Churn$LEAVE=="LEAVE")/nrow(Churn)*log(sum(Churn$LEAVE=="LEAVE")/nrow(Churn))- sum(Churn$LEAVE=="STAY")/nrow(Churn)*log(sum(Churn$LEAVE=="STAY")/nrow(Churn))
EntropyWhole

### Now we try to segment the dataset and check the entropy of the segments (to compute total entropy score) -> This can be done in lot's of ways.
## Consider a certain column (take HOUSE)
## Let's segment the dataset by people whose house is worth more or less than 60000
SegGreater=Churn[Churn$HOUSE>600000,]
SegLess=Churn[(Churn$HOUSE<=600000),]

EntropySegGreater= -sum(SegGreater$LEAVE=="LEAVE")/nrow(SegGreater)*log(sum(SegGreater$LEAVE=="LEAVE")/nrow(SegGreater))- sum(SegGreater$LEAVE=="STAY")/nrow(SegGreater)*log(sum(SegGreater$LEAVE=="STAY")/nrow(SegGreater))
EntropySegLess= -sum(SegLess$LEAVE=="LEAVE")/nrow(SegLess)*log(sum(SegLess$LEAVE=="LEAVE")/nrow(SegLess))- sum(SegLess$LEAVE=="STAY")/nrow(SegLess)*log(sum(SegLess$LEAVE=="STAY")/nrow(SegLess))
EntropySegGreater
EntropySegLess

TotalEntropyScore= nrow(SegGreater)/nrow(Churn)*EntropySegGreater + nrow(SegLess)/nrow(Churn)*EntropySegLess
TotalEntropyScore


## We can estimate the probability that someone will leave or stay depending on whether their HOUSE is worth more than 600k by looking at the percentage in each Segment
# If someone has HOUSE>600k then we predict probabilities (using correction):
SegGreaterPROBleave=(sum(SegGreater$LEAVE=="LEAVE")+1)/(nrow(SegGreater)+2)
SegGreaterPROBleave

SegLessPROBleave=(sum(SegLess$LEAVE=="LEAVE")+1)/(nrow(SegLess)+2)
SegLessPROBleave

## we can take the above approach for any column. If the column is numeric, we have free reign on the threshold to use as the separating value.
## Note that we can also segment SegGreater or SegLess further to create a Decision tree.
# For example if we want to create a segment for people who have HOUSE>600k and Income<100000 we can create segment: SegHOUSEgreatINCOMEless= Churn[Churn$HOUSE>600000 & Churn$HOUSE<600000,]



#################################################################################################
### The most common function for creating Decision Trees is rpart() located in the rpart package!
#rpart() uses entropy to assess how to create the decision tree.

library(rpart)
tree=rpart(LEAVE~HOUSE+OVERAGE, data=Churn)
tree
plot(tree,margin=.1) # This one is ugly (look for rpart.plot for nice one)
text(tree,use.n=TRUE, all=TRUE, cex=.6) ## Note that we need to be able to figure out what the labels are telling us!

### For a pretty tree diagram, we can use the rpart.plot package and function:
#First install rpart.plot package!!
library(rpart.plot)
rpart.plot(tree) ## NOTE tree is an rpart object created earlier.
# If we want to remove scientific notation:
rpart.plot(tree,digits=-5) ## Note that we need to be able to figure out what the labels are telling us OR set them according to what we want to see using arguments of rpart.plot()



## The "complexity parameter" (cp) controls the requirement to create additional branches/splits.
## This lets us specify how much we require the entropy to decrease by in order to create additional nodes. 
## The lower the cp value is set to, the more branches will be created.
## But note that cp IS NOT "entropy reduction threshold"
tree=rpart(LEAVE~HOUSE+OVERAGE, data=Churn,control=rpart.control(cp=.001))
tree
plot(tree,margin=.1)
text(tree,use.n=TRUE, all=TRUE, cex=.6) 
## The graph is difficult to read. rpart.plot() is better for this:
rpart.plot(tree,digits=-3)


### Consider using all the available variables in Churn:
tree=rpart(LEAVE~., data=Churn) ## ,control=rpart.control(cp=.008)) 
rpart.plot(tree,digits=-3)

### With a smaller complexity parameter:
tree=rpart(LEAVE~., data=Churn,control=rpart.control(cp=.001)) 
rpart.plot(tree,digits=-3)


##Note that we can determine the "best" complexity parameter to use by looking at "xerror" column of the following table:
##This is the ratio of the cross-validation prediction error for the given tree to the cross-validation prediction error without segmentation (just using entire dataset)!
tree=rpart(LEAVE~., data=Churn,control=rpart.control(cp=.0001))
### Look at the row where xerror is minimized and set the control parameter slightly below that.
printcp(tree,digits=5) ## we observe that it's ideal to set cp=.0025 (since xerror is minimized right before this)
plotcp(tree)

## Note that we can obtain predictions using the function predict():
tree=rpart(LEAVE~., data=Churn,control=rpart.control(cp=.0025))
rpart.plot(tree,digits=-6)

### We can use our decision tree model to obtain predictions for each observation in the dataset:
predictions=predict(tree,type="class") ## setting type="class" will lead us to obtain the class predictions, in this case LEAVE or STAY.
predictions[1:10]
predict(tree,type="prob")[1:10,] ## We can instead predict the probabilities of being a certain class

sum(Churn$LEAVE!=predictions)/20000

########## MAJOR NOTE: If the response variable column consists of 0's and 1's that are treated as NUMERIC, The rpart will not give you what you want!!
###### You must make sure the response variable is either character or factor!
## EXAMPLE (OF WHAT NOT TO DO):
Churn2=Churn
Churn2$LEAVE=(Churn2$LEAVE=="STAY")*1 ## Note that LEAVE is now numeric!!
Churn2$LEAVE=as.character(Churn2$LEAVE) ## This will fix the numeric issue

tree=rpart(LEAVE~., data=Churn2,control=rpart.control(cp=.0025))
rpart.plot(tree,digits=-6)
predictions=predict(tree,type="class") ## setting type="class" will lead us to obtain the class predictions, in this case LEAVE or STAY.
### Note that the tree looks very different and that "class" predictions are no longer possible.
predictions=predict(tree)
predictions[1:10] ## Note that there is output here, but these are NOT the probabilitie
##### Make sure the response variable is "character" or "factor" (NOT NUMERIC)

########### Cross-validation (10-fold in this case) On Decision Trees:
g1=1:2000
g2=2001:4000
g3=4001:6000
g4=6001:8000
g5=8001:10000
g6=10001:12000
g7=12001:14000
g8=14001:16000
g9=16001:18000
g10=18001:20000
Groups=data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)

data=Churn

predictions=c()
for(i in 1:10){
  tree=rpart(LEAVE~., data=data[-Groups[,i],],control=rpart.control(cp=.001)) 
  predictions_per_fold=predict(tree,type="class",newdata=data[Groups[,i],]) ## 
  predictions=c(predictions,as.character(predictions_per_fold)) ## Note the need to change "predictions_per_fold" into character from factors. Otherwise, when we use c() function, we end up converting "LEAVE/STAY" to 0/1
}
sum(data$LEAVE!=predictions)/length(predictions)

## Compare with Logistic Regression:
predictions=c()
for(i in 1:10){
  LogisticModel=glm(LEAVE~., family=binomial(),data=data[-Groups[,i],]) 
  predictions_per_fold=(predict(LogisticModel, type="response",newdata=data[Groups[,i],])>.5)*1##
  predictions_per_fold[predictions_per_fold==1] = "STAY"
  predictions_per_fold[predictions_per_fold!="STAY"] = "LEAVE"
  predictions=c(predictions,as.character(predictions_per_fold)) ## Note the need to change "predictions_per_fold" into character from factors. Otherwise, when we use c() function, we end up converting "LEAVE/STAY" to 0/1
}
sum(data$LEAVE!=predictions)/length(predictions)

################################################################
################### For the COVID Dataset:######################
## Download and install the COVID19 dataset.

Age=COVID19$age
Gender=COVID19$gender
Country=COVID19$country
WuhanExposure=COVID19$visiting.Wuhan+COVID19$from.Wuhan
WuhanExposure[WuhanExposure==2]=1
Death=COVID19$death
Death[Death!=0]=1

DaysUntilHosp=as.numeric(as.Date(COVID19$hosp_visit_date, format = "%m/%d/%y")-as.Date(COVID19$symptom_onset, format = "%m/%d/%y"))
data=data.frame(Gender,Age,WuhanExposure,DaysUntilHosp,Country,Death)
#data=na.omit(data) ## NOTE WE DON'T NEED TO REMOVE MISSING VALUES SINCE THIS CAN EASILY BE IGNORED BY A DECISION TREE

##Let's turn "Death" variable into something non-numeric (for ease later):
data$Death=as.character(data$Death)

tree=rpart(Death~., data=data)
tree
rpart.plot(tree,digits=-3)
predictions=predict(tree,type="class") ## setting type="class" will lead us to obtain the class predictions, in this case LEAVE or STAY.
predictions[1:10]
sum(data$Death!=predictions)/nrow(data)


############### NOTE let's now use only the observations we used in assessing the logistic regression model.

Age=COVID19$age
Gender=COVID19$gender
Country=COVID19$country
WuhanExposure=COVID19$visiting.Wuhan+COVID19$from.Wuhan
WuhanExposure[WuhanExposure==2]=1
Death=COVID19$death
Death[Death!=0]=1
data=data.frame(Gender,Age,WuhanExposure,DaysToHospVisit,Country,Death)

### Let's now remove the missing values in order to more directly compare with logistic regression example from Lecture Mar17 COVID Code.
data=na.omit(data) ## NOTE WE DON'T NEED TO REMOVE MISSING VALUES SINCE THIS CAN EASILY BE IGNORED BY A DECISION TREE

### Let's remove the same 5 observations as we did when we tried logistic regression in Lecture Mar17 COVID Code.
data=data[data$Country!="Finland" & data$Country!="Nepal" & data$Country!="Phillipines" & data$Country!="Sri Lanka" & data$Country!="Vietnam", ]


##Let's turn "Death" variable into something non-numeric (for ease later):
data$Death=as.character(data$Death)

tree=rpart(Death~., data=data)
tree
rpart.plot(tree,digits=-3)
predictions=predict(tree,type="class") ## setting type="class" will lead us to obtain the class predictions, in this case LEAVE or STAY.
predictions[1:10]
sum(data$Death!=predictions)/nrow(data) ## NOTE THIS IS SMALLER THAN 0.0467 which we observed for the logistic regression model.
### However recall that the logistic regression misclassification rate was obtained for cross-validation.

#### If we want to try cross-validation:
predictions_cv=c()
for(i in 1:nrow(data)){
  tree=rpart(Death~., data=data[-i,])
  predictions_cv=c(predictions_cv,predict(tree,type="class",newdata=data[i,]))
}
predictions_cv ## Note that predictions are 1s and 2s. 
## This can sometimes happen. We can simply turn them into 1s and 0s:
predictions_cv=predictions_cv-1

sum(data$Death!=predictions_cv)/nrow(data)  ## Note that this is slightly worse than the cross-validation misclassification rate.










