
#Download and load the ChurnIntro dataset. Name it Churn:
Churn=ChurnIntro
##### Dataset for predicting the probability that a customer will stay with the same company. 

nums=sapply(Churn,is.numeric) ## identifies numeric columns of Churn dataset
Churn=Churn[,nums] ## keep only numeric columns.

## The potential predictor variables: INCOME, OVERAGE, LEFTOVER, HOUSE, HANDSET_PRICE,OVER_15MINS_CALLS_PER_MONTH,AVERAGE_CALL_DURATION
## The response variable is "PROB_OF_STAY" --- The estimated probability that a customer will not leave.

#First check correlations:
library(lattice)
corrmat=cor(Churn)
levelplot(corrmat,scales=list(x=list(rot=90)))
# Note that there may be pairs of variables that are highly correlated.
# Let's check the correlation matrix:
corrmat
## Here we see that the correlations are just low enough for us to be able to include all the variables.

############################################# Stepwise Regression

##################Using Adjusted-R^2
########## BACKWARD Selection:

### We consider variables INCOME, OVERAGE, LEFTOVER, HOUSE, HANDSET_PRICE,OVER_15MINS_CALLS_PER_MONTH,AVERAGE_CALL_DURATION
data=Churn

# We start by considering (all) the predictor variables INCOME, OVERAGE, LEFTOVER, HOUSE, HANDSET_PRICE,OVER_15MINS_CALLS_PER_MONTH,AVERAGE_CALL_DURATION

model=lm(PROB_OF_STAY~.,data=data) 
summary(model)
## Note where adjusted- R^2 is located. We can also obtain this value with
summary(model)$adj


## Now let's remove each of the variables one at a time to see which removal will result in largest adjusted-R^2
summary(lm(PROB_OF_STAY~.,data=data[,-1]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,-2]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,-3]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,-4]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,-5]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,-6]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,-7]))$adj
## Note that removing the 6th column (OVER_15MIN_CALLS_PER_MONTH) results in a higher adjusted-R^2.

## and start again removing additional variables:
summary(lm(PROB_OF_STAY~.,data=data[,c(-1,-6)]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,c(-2,-6)]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,c(-3,-6)]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,c(-4,-6)]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,c(-5,-6)]))$adj
summary(lm(PROB_OF_STAY~.,data=data[,c(-6,-6)]))$adj
### None of these are larger than 0.8402361.
### Therefore we stop and use all the variables previously considered (all except OVER_15MIN_CALLS_PER_MONT)

#We could have accomplished the same task with the following loops:
res=c()
for(k in 1:7){ ## data has 9 columns
  model=lm(PROB_OF_STAY~.,data=data[,-k])
  res=c(res,summary(model)$adj) ##captures the adjusted-R^2 for each iteration
}
res 

data=data[,-6]

res=c()
for(k in 1:6){ ## data has 9 columns
  model=lm(PROB_OF_STAY~.,data=data[,-k])
  res=c(res,summary(model)$adj) ##captures the adjusted-R^2 for each iteration
}
res 
#######################################


########## FORWARD Selection:

### We consider variables INCOME, OVERAGE, LEFTOVER, HOUSE, HANDSET_PRICE,OVER_15MINS_CALLS_PER_MONTH,AVERAGE_CALL_DURATION
data=Churn

summary(lm(data[,8]~ data[,1]))$adj
summary(lm(data[,8]~ data[,2]))$adj
summary(lm(data[,8]~ data[,3]))$adj
summary(lm(data[,8]~ data[,4]))$adj
summary(lm(data[,8]~ data[,5]))$adj
summary(lm(data[,8]~ data[,6]))$adj
summary(lm(data[,8]~ data[,7]))$adj

# Note that adjusted-R^2 is largest when we use the second column ("OVERAGE") (.4505094)
## Keep "OVERAGE" and add in variables one at a time:

summary(lm(data[,8]~ data[,2]+data[,1]))$adj
summary(lm(data[,8]~ data[,2]+data[,3]))$adj
summary(lm(data[,8]~ data[,2]+data[,4]))$adj
summary(lm(data[,8]~ data[,2]+data[,5]))$adj
summary(lm(data[,8]~ data[,2]+data[,6]))$adj
summary(lm(data[,8]~ data[,2]+data[,7]))$adj

# Note that adjusted-R^2 is largest when we use the second and forth columns ("Overage" and "LOG_HOUSE") (.7482824)
## Keep "OVERAGE" and "LOG_HOUSE" and add in variables one at a time:  

summary(lm(data[,8]~ data[,2]+data[,4]+data[,1]))$adj
summary(lm(data[,8]~ data[,2]+data[,4]+data[,3]))$adj
summary(lm(data[,8]~ data[,2]+data[,4]+data[,5]))$adj
summary(lm(data[,8]~ data[,2]+data[,4]+data[,6]))$adj
summary(lm(data[,8]~ data[,2]+data[,4]+data[,7]))$adj

# Note adjusted-R^2 is largest when we use "OVERAGE" and "LOG_HOUSE" and "LOG_INCOME"
#### WE SHOULD CONTINUE THIS PROCESS UNTIL ADJUSTED R^2 DOES NOT INCREASE WHEN ADDING IN A VARIABLE...


##################Using AIC/BIC

#NOTE: Things are a bit easier here since there is a function step() in the default 'stats' package!
data=Churn

##### By default, step() selects models based on AIC.

## For Backward Selection, this is all that's needed:
step(lm(PROB_OF_STAY~.,data=data))
# Note that it is determined that the model with all the variables except OVER_15MINS_CALLS_PER_MONTH is best!

## For Forward Selection, we can use:
step(lm(PROB_OF_STAY~1,data=data),direction="forward",scope=list(lower=lm(PROB_OF_STAY~1,data=data),upper=lm(PROB_OF_STAY~.,data=data)))
# The model that appears last is the one selected (In this case using all the variables except OVER_15MINS_CALLS_PER_MONTH)


########NOTE: If we want to use BIC, we can specify parameter k=log(n) in the step() function, where n is the number of observations.
## So in our case, to use BIC to determine the model, we specify k=log(20000) for the Churn data.
## Forward Selection:
step(lm(PROB_OF_STAY~.,data=data),k=log(396))

## Backward Selection:
step(lm(PROB_OF_STAY~1,data=data),direction="forward",scope=list(lower=lm(PROB_OF_STAY~1,data=data),upper=lm(PROB_OF_STAY~.,data=data)),k=log(396))


