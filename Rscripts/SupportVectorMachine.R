x1=c(.3,.29,1.67,-.6,-.65,-1.08)
x2=c(.43,1.42,.7,-.66,-1.15,-.75)


y=c("yes","yes","yes","no","no","no")
y=as.factor(y) ### note this is just another way to assign numbers to character vectors
dat=data.frame(cbind(x1,x2,y))
dat

plot(x2,x1,pch=c("2","2","2","1","1","1"))


plot(x2,x1,pch=c("+","+","+","-","-","-"))

library(e1071)

model=svm(y~.,data=dat,kernel="linear",scale=TRUE,type="C-classification",cost=1)
## kernel argument allows us to do linear or non-linear SVM.
## scale (TRUE by default) indicates whether we want the data to first be standardized-- For the input variables to have mean 0 and variance 1. Useful if the x-variables are on different scales.
## type="C-classification" is the standard svm procedure where there is a trade-off between making the margin as wide as possible while penalizing observations inside or on the wrong side of margin.
## Other possibilities for type will allow you to set the number of Support Vectors we want OR control the total error that is made, OR etc.
## Larger the cost (penalty for being inside or on the wrong side of margin), the smaller the margin

predict(model)
plot(model,data=dat,xlim=c(-1.5,1.5),ylim=c(-1.5,2))
## Support Vectors are on the edge (and inside) the margin (indicated by X).
##Actual Class is indicated by color of each observation- red vs. black.
##Class prediction is indicated by region color- light blue vs. purple

## We can get predictions for new data using predict() function.
newdata=matrix(c(0,0,0,-.1,0,-.2,0,-.3,0,-.4,0,-.5),ncol=2,byrow=TRUE)
newdata=data.frame(newdata)
colnames(newdata)=c("x1","x2")
newdata
predict(model,newdata=newdata)


## Alternative way to get predictions (in order to understand what the linear classifier looks like):
model$coefs
model$SV
weights=t(model$coefs) %*% model$SV
weights
intercept=-model$rho
intercept
## In the case of two x-variables, it can be shown (algebraically) that these weights (w1 and w2) 
## are the coefficients in the line 0=intercept+w1*x1+w2*x2 (which is our linear classifier in this case)
###### Note that the equation of the line appearing in our plot matches up with this:
################################ x1= intercept/(-w1) + w2/(-w1)

intercept+matrix(c(0,0,0,-.1,0,-.2,0,-.3,0,-.4,0,-.5),ncol=2,byrow=TRUE)%*%t(weights)
##The separation between classes occurs at zero. So predictions can be computed as:
sign(-model$rho+matrix(c(0,0,0,-.1,0,-.2,0,-.3,0,-.4,0,-.5),ncol=2,byrow=TRUE)%*%t(weights))

#########################################
## Non-linearly separable:

x1=c(1,x1)
x2=c(.6,x2)

y=as.factor(c("no","yes","yes","yes","no","no","no"))
dat=data.frame(cbind(x1,x2,y))
dat

plot(x2,x1,pch=c("-","+","+","+","-","-","-"),xlim=c(-2,2),ylim=c(-2,2))


model=svm(y~.,data=dat,kernel="linear",scale=TRUE,type="C-classification",cost=100)

model$coefs
model$SV
#### Note that the number of support vectors is large (thus many support vectors in margin) as the algorithm is struggling to separate the data.

predict(model)
plot(model,data=dat,xlim=c(-2,2),ylim=c(-2,2))

newdata=matrix(c(0,0,0,-.1,0,-.2,0,-.3,0,-.4,0,-.5),ncol=2,byrow=TRUE)
newdata=data.frame(newdata)
colnames(newdata)=c("x1","x2")
newdata

predict(model,newdata) ### Note that all the predictions are now that y is class 1
########################## The original model seemed to be better, but now this new 
########################## observation has screwed things up!
## Alternative way to get predictions:
weights=t(model$coefs) %*% model$SV
-model$rho+matrix(c(-.5,0,-.4,0,-.3,0,-.2,0,-.1,0,0,0,.1,0,.2,0),ncol=2,byrow=TRUE)%*%t(weights)
## All the predictions would be the same (predict y = 1) since these are all above zero.
##########Now let's try non-linear SVM
##We use the kernel "radial" (see description of svm function for different kernel names we can try), which creates a new variable x3 from x1 and x2 and uses x1,x2,x3 to classify y.


model=svm(y~.,data=dat,kernel="radial",type="C-classification",cost=1)
plot(model,data=dat,xlim=c(-2,2),ylim=c(-2,2))



model=svm(y~.,data=dat,kernel="radial",type="C-classification",cost=100)
plot(model,data=dat,xlim=c(-2,2),ylim=c(-2,2))
## Larger the cost, the smaller the margin, the more the algorithm tries to separate the classes.



##################################################################
## Let's consider the Churn dataset:

### Instead of using cross-validation (which would take too long to demonstrate in class with a time-consuming algorithm like SVM),
### We will split the dataset into TRAINING Data (which is used to build the model), and A TESTING Data (which is used to test the model)

# We will consider just INCOME and HOUSE
testobs=sample(1:20000,2000)
data_train=Churn[-testobs,c(2,5,12)]
data_test=Churn[testobs,c(2,5,12)]
### above we randomly assign 18000 observations to train the data and the last 2000 observations to test the data.

model=svm(LEAVE~.,data=data_train,kernel="radial",cost=1,type="C-classification")
predictions=predict(model,newdata=data_test)
missclass=sum(Churn$LEAVE[testobs]!=predictions)/2000
missclass
plot(model,data=data_train) 


# We will consider INCOME, OVERAGE, LEFTOVER, and HOUSE
data_train=Churn[-testobs,c(2,3,4,5,12)]
data_test=Churn[testobs,c(2,3,4,5,12)]

model=svm(LEAVE~.,data=data_train,kernel="radial",cost=1)
predictions=predict(model,newdata=data_test)
missclass=sum(Churn$LEAVE[testobs]!=predictions)/2000
missclass
plot(model,Churn,formula=INCOME~OVERAGE) ## note the need to specify which two variables (in 'formula') we want to use in our plot

model=svm(LEAVE~.,data=data_train,kernel="radial",cost=100)
predictions=predict(model,newdata=data_test)
missclass=sum(Churn$LEAVE[testobs]!=predictions)/2000
missclass

model=svm(LEAVE~.,data=data_train,kernel="linear",cost=1)
predictions=predict(model,newdata=data_test)
missclass=sum(Churn$LEAVE[testobs]!=predictions)/2000
missclass
####The higher the cost- the smaller the margin- the more ways there is to fit a linear separator in the space- the longer the algorithm takes.



#############################Compare with logistic regression:
model=glm(LEAVE~., family=binomial(),data=data_train)

## Let's choose the threshold if p(x)>.5 then we predict 1 for y. We can obtain predicted values then using:
Pred=(predict(model,newdata=data_test,type="response")>.5)*1 ####### 1 is for a prediction of "Stay"
LEAVEnum=(data_test$LEAVE=="STAY")
missclass=sum(Pred!=LEAVEnum)/2000## This gives the misclassification rate using logistic regression for the last 5000 observations in Churn.
missclass










