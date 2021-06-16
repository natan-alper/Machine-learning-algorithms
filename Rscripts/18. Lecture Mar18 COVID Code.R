## Download and import COVID19 dataset
##### It would be of major interest to be able to predict "death".

## What kind of model should we use?
## What variables can we use?

##### Break up exposure date into before/after COVID became international
##### Use visiting.Wuhan and from.Wuhan  vs. NOT


### Let's try to use Gender, Age, "Exposure to someone from Wuhan", and "Days from Onset to Hospital visit" to predict death.

Age=COVID19$age
Gender=COVID19$gender
Country=COVID19$country
WuhanExposure=COVID19$visiting.Wuhan+COVID19$from.Wuhan
WuhanExposure[WuhanExposure==2]=1


## Note that we can't simply substract the symptom_onset_date from hosp_visit_date 
## since currently these are not Date variables, but simply strings.

### Google search-- r create date variable
### Or Google search -- r convert string to date

### This takes ut to the as.Date() function let's us convert strings to dates!
as.Date(COVID19$symptom_onset[1]) ## oops this doesnt work! (it thinks year is 0001)
### The following does work:
as.Date(COVID19$hosp_visit_date[1], format = "%m/%d/%y")

DaysUntilHosp=as.numeric(as.Date(COVID19$hosp_visit_date, format = "%m/%d/%y")-as.Date(COVID19$symptom_onset, format = "%m/%d/%y"))

## Finally Let's grab the "death" variable:
Death=COVID19$death
table(Death) ## oops looks like Death is not 0-1 variable. 
## let's convert the dates to 1's
Death[Death!=0]=1
table(Death)
Death= as.numeric(as.character(Death)) ## Let's convert Death from being a factor to simple 0-1.
table(Death)

## The dataset:
data=data.frame(Gender,Age,WuhanExposure,DaysUntilHosp,Country,Death)
View(data)
### sadly we need to throw out the missing values:
data=na.omit(data)
dim(data)
#### Estimate a Logistic Regression Model with Death as the respose variable!

model=glm(Death~., family=binomial(),data=data)
model
summary(model) ## Note how interesting this is as "Age", "Gender" and "DaysToHospVisit" all seem like SIGNIFICANT (useful) predictor variables.

##### Let's test our model using cross-validation misclassification rate!
Death_Pred=c()
for(k in 1:nrow(data)){
  
  model=glm(Death~.,family=binomial(), data=data[-k,])
  LogOdds_pred=predict(model,newdata=data[k,])
  Prob_pred=1/(1+exp(-LogOdds_pred)) ### Note we can also use Prob_pred=predict(model,type="response",newdata=data[k,])
  Death_Pred=c(Death_Pred,(Prob_pred>.5)*1)
  
}## note there is an error here because some countries only appear once in the data!

### Let's get rid of observations belonging to countries that appear only once:
table(data$Country) ## Note the countries that appear only once (we need to get rid of these)
data$Country=as.character(data$Country)
data=data[data$Country!="Finland" & data$Country!="Nepal" & data$Country!="Phillipines" & data$Country!="Sri Lanka" & data$Country!="Vietnam", ]
dim(data) ## We only lost 5 observations!
table(data$Country)

### Let's try the CV again:
Death_Pred=c()
for(k in 1:nrow(data)){
  
  model=glm(Death~.,family=binomial(), data=data[-k,])
  LogOdds_pred=predict(model,newdata=data[k,])
  Prob_pred=1/(1+exp(-LogOdds_pred)) ### Note we can also use Prob_pred=predict(model,type="response",newdata=data[k,])
  Death_Pred=c(Death_Pred,(Prob_pred>.5)*1)
  
}
Death_Pred

MissClassRate=sum(data$Death!=Death_Pred)/nrow(data)
MissClassRate ## This is cross-validation misclassification rate for Logistic Regression Model using all variables!
### Note that it is smaller than the misclassrate for a "simple" model which just predicts "Survived" for everybody:
sum(data$Death==1)/nrow(data) #misclass rate for naive model.

### We can also try using the cv.glm() function in the "boot" package.
library(boot)
cv.glm(data,glm(Death~.,family=binomial(),data=data),K=nrow(data))$delta[1] 
## note this is not the same. 
## As before, we need to be careful about getting 0-1 predictions as opposed to Log-Odds or Probabilities:

## Here is a way to adapt the cv.glm() function to 0-1 predictions based on a probability threshold of .5
cost <- function(r, pi) mean(abs(r-pi)> 0.5)
cv.glm(data,glm(Death~.,family = binomial(),data = data), cost = cost)$delta[1]
## This gives us the correct misclassification rate!
## (This comes from a quick Google search--- cv.glm logistic regression)


########## Doing Variable Selection (Backward Selection)
###### Cross-Validation error rates when removing 1 variables at a time:
cv_misclass=c()
for(k in 1:5){
  cv_misclass=c(cv_misclass,  cv.glm(data[,-k],glm(Death~.,family = binomial(),data = data[,-k]), cost = cost)$delta[1])
}
cv_misclass
## Note that removing WuhanExposure does not increase the misclass rate, so we should remove it (in interest of simplicity)

#### Continue variable selection after removing WuhanExposure:
########## FOR YOU TO TRY.







