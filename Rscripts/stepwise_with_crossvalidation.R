#Download and load the ChurnIntro dataset. Name it Churn:
Churn=ChurnIntro
##### Dataset for predicting the probability that a customer will stay with the same company. 

nums=sapply(Churn,is.numeric) ## identifies numeric columns of Churn dataset
Churn=Churn[,nums] ## keep only numeric columns.

#First check correlations:
library(lattice)
corrmat=cor(Churn)
levelplot(corrmat,scales=list(x=list(rot=90)))
# Note that there may be pairs of variables that are highly correlated.
# Let's check the correlation matrix:
corrmat
## Here we see that the correlations are just low enough for us to be able to include all the variables.

### We consider variables INCOME, OVERAGE, LEFTOVER, HOUSE, HANDSET_PRICE,OVER_15MINS_CALLS_PER_MONTH,AVERAGE_CALL_DURATION
data=Churn

#Let's do 10-fold cross validation:
### Note that we will code the cross validation procedure ourselves to understand what cross validation is doing.
### A direct function for CV can be found in the 'boot' package called cv.glm()
### cv.glm() requires an object of class glm (and not lm). We will see glm shortly.

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

## Backward Selection:

Groups=data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)

### This computes the SSEs for the 10 folds:
errors=c()
for(i in 1:10){
mod=lm(PROB_OF_STAY~.,data=data[-Groups[,i],]) ## Here we "Don't use" the observations in column i of Groups to fit model
# Note that we are using all predictor variables first.
predicted=predict(mod,newdata=data[Groups[,i],]) ## Here we predict PROB_OF_STAY for observations in column i of Groups using the fitted model
# Note the importance of the "predict()" function!
errors=c(errors,data$PROB_OF_STAY[Groups[,i]]-predicted)
}
sum(errors^2)### This computes the SSE for the 10 folds.
mean(errors^2)### This computes the MSE for the 10 folds.


### The same thing can be done with the following code (without the need for "Groups")
errors=c()
for(i in 1:10){
  mod=lm(PROB_OF_STAY~.,data=data[-((2000*(i-1)+1):(2000*i)),])
  predicted=predict(mod,newdata=data[(2000*(i-1)+1):(2000*i),])
  errors=c(errors,data$PROB_OF_STAY[(2000*(i-1)+1):(2000*i)]-predicted)
}
sum(errors^2)


#Using Backward Selection,
#we can now remove one variable at a time to see if the SSE improves:
#Note that we should start by removing the 'squared' variables.
sse=c()

for(k in 1:7){
datanew=data[,-k]

errors=c()
  for(i in 1:10){
  mod=lm(PROB_OF_STAY~.,data=datanew[-Groups[,i],])
  predicted=predict(mod,newdata=datanew[Groups[,i],])
  errors=c(errors,datanew$PROB_OF_STAY[Groups[,i]]-predicted)
}

sse=c(sse,sum(errors^2)) ## avgsse will contain the averages of the SSE across the folds for each time we remove a variable
}
sse
#Note that the SSE for the model which removes OVER_15MINS_CALLS_PER_MONTH is smallest 
#and the SSE is smaller than the previous model which uses all the predictor variables. 

data=data[,-7]## remove OVER_15MINS_CALLS_PER_MONTH

### Consider removing the other predictor variables now again:

sse=c()
for(k in 1:6){
  datanew=data[,-k]
  
  errors=c()
  for(i in 1:10){
    mod=lm(PROB_OF_STAY~.,data=datanew[-Groups[,i],])
    predicted=predict(mod,newdata=datanew[Groups[,i],])
    errors=c(errors,datanew$PROB_OF_STAY[Groups[,i]]-predicted)
  }
  
  sse=c(sse,sum(errors^2)) ## avgsse will contain the averages of the SSE across the folds for each time we remove a variable
}
sse
#Note that none of the SSEs are smaller than the one that removes only OVER_15MINS_CALLS_PER_MONTH



### Forward selection can be done in a similar way, starting from using only one variable at a time.


################# Using cv.glm() function.

######Note that we can alternatively use the cv.glm() function found in the package 'boot' (For Bootstrap)
###cv.glm() will use prediction error MSE by default, but any prediction error function can be assigned using parameter 'cost'
###We still need to program the backward of forward selection however.
###Note that the model must now be estimated using glm() and not lm()

library(boot)

### We consider variables INCOME, OVERAGE, LEFTOVER, HOUSE, HANDSET_PRICE, OVER_15MINS_CALLS_PER_MONTH, AVERAGE_CALL_DURATION
data=Churn

#We can carry out cross-validation with MSE using all the predictor variables as:
cv.glm(data,glm(PROB_OF_STAY~.,data=data),K=10)$delta 
## This gives us two measures of average prediction error (the first one being the actual average prediction error)
## Note that K specifies the number of groups to use.

#To do Backward selection using cross-validation,
#we can remove one of the 'squared' predictor variables and compute the cross-validation prediction error each time:
res=c()
for(k in 1:7){ ## data has 9 columns
  model=glm(PROB_OF_STAY~.,data=data[,-k])
  res=c(res,cv.glm(data,model,K=10)$delta[1]) ##captures the cross-validation prediction error (MSE) each time we remove a different variable.
}
res

#Note that MSE is smallest when we drop column 6, which is OVER_15MINS_CALLS_PER_MONTH
#and this is smaller than the MSE computed when using all variables.

##Note your result may be different when you run the CV because different observations are selected for the different folds.
#Your column that you should remove is in removecolumn:
removecolumn=(1:7)[res==min(res)] 

#The smallest crossvalidation prediction error after removal of variable is:
res[res==min(res)]

#We remove column "removecolumn" and carry out cross-validation again:
data=data[,-removecolumn] 
res=c()
for(k in 1:6){ ## data has 9 columns
  model=glm(PROB_OF_STAY~.,data=data[,-k])
  res=c(res,cv.glm(data,model,K=10)$delta[1]) ##captures the cross-validation prediction error (MSE) each time we remove a different variable.
}
res

removecolumn=(1:6)[res==min(res)] 

#The smallest crossvalidation MSE after removal of variable is:
res[res==min(res)]
### If this is smaller than the MSE when removing only OVER_15MINS_CALLS_PER_MONTH then we keep going!



## FORWARD selection can be carried out in a similar manner.








