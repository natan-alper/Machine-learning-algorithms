## DOWNLOAD and IMPORT the Churn.csv dataset. Note this DIFFERENT from the ChurnIntro dataset.

### This is the actual Churn dataset as described in the "Foster Provost" Textbook.
Churn=cbind(Churn,LEAVEnum=(Churn$LEAVE=="STAY")*1)


# We will consider INCOME, OVERAGE, LEFTOVER, HOUSE, SATISFACTION, CHANGE_OF_PLAN
data=Churn[,c(2,3,4,5,9,11,13)]

model=glm(LEAVEnum~., family=binomial(),data=data)
model
## note that it is not necessary to have a response variable be 0 and 1 as opposed to "LEAVE" and "STAY". 
### We could have similarly ran the model: glm(LEAVE~INCOME+OVERAGE+LEFTOVER+HOUSE+SATISFACTION+CHANGE_OF_PLAN, family=binomial(),data=Churn), 
############ (as long as LEAVE is a "FACTOR")


##Using the predict() function on model will result in the estimated log-odds:
predict(model)[1:10]
##Using the fitted() function on model will result in the estimated probabilities:
fitted(model)[1:10]
# or alternatively
model$fit[1:10]

## Let's choose the threshold if p(x)>.5 then we predict 1 for y. We can obtain predicted values then using:
Pred=(fitted(model)>.5)*1
#The MAE (which is the same as Misclassification Rate in logistic regression) for our predictions is then:
sum(Churn$LEAVEnum!=Pred)/20000 ## This gives the misclassification rate using logistic regression.



## If we want to use a "probit" transformation instead of "logit":
model=glm(LEAVEnum~., family=binomial("probit"),data=data)
Pred=(fitted(model)>.5)*1
#The MSE for our predictions is then:
sum(Churn$LEAVEnum!=Pred)/20000 ## This gives the misclassification rate using logistic regression with a probit link function

