#Download and Import Churn dataset

#### We will use the randomForest package (you will need to install it first)
library(randomForest)
##### Note that your response variable has to be a factor (or character). If you pass it a column of 0s and 1s that are treated as numbers, it will NOT create a classification model.

RF=randomForest(LEAVE~.,data=Churn[,c(1,2,3,4,5,6,7,8,9,12)]) ## lets use the first 9 columns as the predictor variables (for no particular reason other than to save time).
## The argument mtry states how many variables to use at each node. The default is sqrt(# of columns).
## The argument ntree states how many trees to create. The default is 500.

RF
##### Note that the output gives the Out-of-bag (OOB) misclassification rate (30.8%). This is the misclassification rate for the observations that were not used in building each particular tree.

### To obtain predictions we can use the predict function. Let's look at the predictions for the first 10 observations:
predict(RF)[1:10]
### We can also see the percentage of votes 
predict(RF,type="vote")[1:10,]

### Misclassification rate for the RF algorithm for the entire Churn Dataset is given by:
predictions=predict(RF)
sum(Churn$LEAVE!=predictions)/20000


############## Let's split Churn into a training and testing set:
trainobs=sample(1:20000,18000,replace=FALSE)
testobs=(1:20000)[-trainobs]

RF=randomForest(LEAVE~.,data=Churn[trainobs,c(1,2,3,4,5,6,7,8,9,12)])
predictions=predict(RF,newdata=Churn[testobs,])
misclassrate=sum(Churn$LEAVE[testobs]!=predictions)/length(testobs)
misclassrate

############# Let's compare this with the misclassification rate when using just one Decision Tree:
tree=rpart(LEAVE~., data=Churn[trainobs,c(1,2,3,4,5,6,7,8,9,12)],control=rpart.control(cp=.0025))
predictions=predict(tree,type="class",newdata=Churn[testobs,]) ## setting type="class" will lead us to obtain the class predictions, in this case LEAVE or STAY.
misclassrate=sum(Churn$LEAVE[testobs]!=predictions)/length(testobs)
misclassrate







