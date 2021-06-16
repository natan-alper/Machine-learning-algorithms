# You must import ChurnIntro into Rstudio.
Churn=ChurnIntro

#let's consider how to incorporate 'whether a customer is considering changing their plan' into our model.
unique(Churn$CHANGE)## identifies the unique values in column CHANGE_OF_PLAN

##We first illustrate the procedure of creating 4 new (Indicator variables) predictor variables (columns)
##Later we will see that creating the additional columns is not necessary when using R.

noIND=(Churn$CHANGE=="no")*1
consideringIND=(Churn$CHANGE=="considering")*1
perhapsIND=(Churn$CHANGE=="perhaps")*1
never_thoughtIND=(Churn$CHANGE=="never_thought")*1

Churn=data.frame(Churn,noIND,consideringIND,perhapsIND,never_thoughtIND)

#First check correlations:
### We consider variables LOG_INCOME, OVERAGE, LEFTOVER, LOG_HOUSE, HANDSET_PRICE,OVER_15MINS_CALLS_PER_MONTH,AVERAGE_CALL_DURATION
### and the qualitative variable CHANGE_OF_PLAN.

data=Churn[,c(2,3,4,5,6,7,8,14,15,16,17,13)]

model=lm(PROB_OF_STAY~.,data=data)
model
summary(model)

#We can for example compare this with the model when we don't use CHANGE_OF_PLAN:
model=lm(PROB_OF_STAY~.,data=data[,-(5:8)])
model
summary(model)


### NOTE THAT IT IS ALSO POSSIBLE TO SPECIFY THE QUALITATIVE VARIABLE WITHIN THE lm() FUNCTION!
model=lm(PROB_OF_STAY~LOG_INCOME+OVERAGE+LEFTOVER+LOG_HOUSE+HANDSET_PRICE+OVER_15MINS_CALLS_PER_MONTH+AVERAGE_CALL_DURATION+CHANGE_OF_PLAN,data=Churn)
model
summary(model)




