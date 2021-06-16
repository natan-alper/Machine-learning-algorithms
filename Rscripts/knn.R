# Import Churn dataset.

## The most commonly used function for carrying out the k-nearest neighbor algorithm is knn() available in the "class" package.
## Note that knn() only allows Euclidean distance to be used.
## The k.nearest.neighbors() function in package "FASTKNN" allows the use of any distance measure by allowing a parameter of type "dist" to be passed.

library(class)

##As should be anticipated, the primary inputs into knn are the values of the observations whose classes we know AND the values of the observations whose classes we wish to predict, as well as the value of K.

## "train" refers to the dataset of observations whose y-variable classes we know (which should not include the y-variable classes themselves--- these go into the "cl" parameter)
## "test" refers to the dataset of observations whose y-variable classes we wish to predict
## "cl" refers to the y-variable classes of the observations given in "train".

data=Churn[,c(2,3,4,5,6,7,8,12)] ##Lets take all the numeric columns of Churn.


###If we wanted to test the performance of the k-nearest neighbors model using the observations used in the model we can simply do the following:
###Note that the issue with this approach is that we allow each observation to cast a vote for itself.
Pred=knn(train=data[,-8],test=data[,-8],cl=data[,8],k=10) ##Note that we don't include the "LEAVE" column in test or train


###Alternatively, we can pretend like we don't know the class of the last 2000 observations.
xs_KNOWN_CLASS=data[1:18000,-8]
y_KNOWN_CLASS=data[1:18000,8]
xs_UNKNOWN_CLASS=data[18001:20000,-8]
Pred=knn(train=xs_KNOWN_CLASS,test=xs_UNKNOWN_CLASS,cl=y_KNOWN_CLASS,k=151) ##Note that we don't include the "LEAVE" column in test or train
##Let's compare to the true values to compute misclassification rate:
misclass=sum(data$LEAVE[18001:20000]!=Pred)/2000
misclass


## Note that it we should consider bringing all columns to the same scale:
for(i in 1:(ncol(data)-1)){ ## Note that response variable is assumed to be the last column here
  data[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
}

xs_KNOWN_CLASS=data[1:18000,-8]
y_KNOWN_CLASS=data[1:18000,8]
xs_UNKNOWN_CLASS=data[18001:20000,-8]
Pred=knn(train=xs_KNOWN_CLASS,test=xs_UNKNOWN_CLASS,cl=y_KNOWN_CLASS,k=151) ##Note that we don't include the "LEAVE" column in test or train
misclass=sum(data$LEAVE[18001:20000]!=Pred)/2000
misclass ### note the reduction in the misclassification rate!!!


##Let's see how changing the value k affects the predictions:
misclass=c()
for(k in 5*(1:31)){ ## Note that in the interest of time we will consider k=4,8,12,16, etc.
  Pred=knn(train=xs_KNOWN_CLASS,test=xs_UNKNOWN_CLASS,cl=y_KNOWN_CLASS,k=k) ##Note that we don't include the "LEAVE" column in test or train
  misclass=c(misclass,sum(data$LEAVE[18001:20000]!=Pred)/2000)
}

plot(5*(1:31), misclass,pch=16) ## We are trying to find a choice of "k" which would lead to the lowest misclassification rate.



######## As usual, a better way to assess performance is using cross-validation:

### randomly assigns observations to folds:
Groups=sample(1:20000,replace=FALSE) 
Groups_mat=matrix(Groups,ncol=10)

predictions_cv=c()
for(foldN in 1:10){
  predictions_per_fold=knn(train=data[-Groups_mat[,foldN],-8],test=data[Groups_mat[,foldN],-8],cl=data[-Groups_mat[,foldN],8],k=75) ##Let's use 75-nearest neighbors
  predictions_cv=c(predictions_cv,as.character(predictions_per_fold))
}
### Note that predictions_cv contains predictions for all 20000 observations, 
### but the predictions are jumbled according to the random assigment into folds
misclass=sum(predictions_cv!=data$LEAVE[Groups])/20000
misclass




############### Let's see how Knn works on COVID using only the observations we used in assessing the Logistic regression and Decision Tree models.
Age=COVID19$age 
#Gender=COVID19$gender ## Can't use qualitative variables
#Country=COVID19$country ## Can't use qualitative variables
#WuhanExposure=COVID19$visiting.Wuhan+COVID19$from.Wuhan
#WuhanExposure[WuhanExposure==2]=1 ## Can't use qualitative variables
DaysUntilHosp=as.numeric(as.Date(COVID19$hosp_visit_date, format = "%m/%d/%y")-as.Date(COVID19$symptom_onset, format = "%m/%d/%y"))
Death=COVID19$death
Death[Death!=0]=1
Death=as.character(data$Death)
data=data.frame(Age,DaysUntilHosp,Death)

### Let's now remove the missing values in order to more directly compare with logistic regression and decision tree examples
data=na.omit(data) 
### Let's remove the same 5 observations as we did when we tried logistic regression in Lecture Mar17 COVID Code.

for(i in 1:(ncol(data)-1)){ ## Note that response variable is assumed to be the last column here
  data[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
}

### Let's figure out how to code the cross-validation:

Groups=sample(1:430,replace=FALSE) 
Groups_mat=matrix(Groups,ncol=10)

predictions_cv=c()
for(foldN in 1:10){
  predictions_per_fold=knn(train=data[-Groups_mat[,foldN],-8],test=data[Groups_mat[,foldN],-8],cl=data[-Groups_mat[,foldN],8],k=75) ##Let's use 75-nearest neighbors
  predictions_cv=c(predictions_cv,as.character(predictions_per_fold))
}
### Note that predictions_cv contains predictions for all 20000 observations, 
### but the predictions are jumbled according to the random assigment into folds
misclass=sum(predictions_cv!=data$LEAVE[Groups])/20000
misclass














