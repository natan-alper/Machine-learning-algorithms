##As an example, consider two predictor variables:
x1=c(1,4,10)
x2=c(2,3,4)
cor(data.frame(x1,x2))

#We "center" the data manually (a requirement of PCA) meaning that we make the mean of each predictor variable column "ZERO
x1cent=x1-mean(x1)
x2cent=x2-mean(x2)
cor(data.frame(x1cent,x2cent))

y=5+3*x1cent+2*x2cent+rnorm(3)
lm(y~x1cent+x2cent)
# NOTE the large difference between the true coefficients in the model for y,x1, and x2 
# and the coefficient estimates that we end up observing!


data=cbind(x1cent,x2cent)
data
##Suppose we wanted to find the principle components for this data:

pca=prcomp(data) ## BY DEFAULT THE DATA WILL BE "CENTERED" FIRST (ie. the means will be set to zero as we did in x1cent=x1-mean(x1), etc.)

summary(pca) ## The most important thing to look for here is the Proportion of Variance row, 
## which can tell us the best number of Principle Components to use.


##The new data is located in
pca$x ## This is the data for the new predictor variables. You would use some (or all) of the columns here as predictor variables in the model
## PC1 is a column containing data for the first new predictor variable (1st Principle Component.), PC2 is a column containing data for the second new predictor variable (2nd Principle Component.)
cor(pca$x) ## Note that the correlation between the principle components is 0.
## as oppose to:
cor(x1cent,x2cent) ## where the correlation is .9819805


##The coefficients in the linear combination are located in 
pca$rotation ## This tells us how the data in pca$x was generated using the original data x1,x2.

#Note that the linear combinations we obtain are:
#PC1= .9777*x1cent+.2099*x2cent  (READ pca$rotation 'top to bottom' NOT 'left to right')
#PC2= .2099*x1cent-.9777*x2cent

## Had we NOT centered the data first and did it within the prcomp() function then the equations would look like:
#PC1= .9777*(x1-mean(x1))+.2099*(x2-mean(x2))  (READ pca$rotation 'top to bottom' NOT 'left to right')
#PC2= .2099*(x1-mean(x1))-.9777*(x2-mean(x2))


#That is to obtain the values in the first column of the new data "pca$x" (the values of the first principle component): 
.9777*(-4)+.2099*(-1)   # NOTE THAT WE ARE USING THE "CENTERED" x1cent and x2cent.
.9777*(-1)+.2099*(0)
.9777*(5)+.2099*(1)




##Note that the sum of the variances of x1cent and x2cent is equal to the sum of the variances of PC1 and PC2:
var(x1cent)+var(x2cent)
var(pca$x[,1])+var(pca$x[,2])
## This will be true as long as x1 and x2 have been "centered" (their means are 0)


#The variance of the first principle component is however larger than the variance of x1 or x2!
var(pca$x[,1])
var(x1cent)
var(x1)
var(x2cent)
var(x2)

#A screeplot can be generated with:
screeplot(pca,type="lines")
#OR ALTERNATIVELY:
plot(pca$sd^2,type="l",ylab="Variance")
##Note that the first principle component contains MOST of the variation. Thus we can drop the second principle component.


##NOTE that we can generate a screeplot with "percentage of variance" on the y-axis with:
plot(pca$sd^2/sum(pca$sd^2),type="l",ylab="Percentage of Variance")

### Once we decide on the number of principle components to use (in this case 1), we would do the usual thing:
lm(y~pca$x[,1]) 

## IMPORTANT NOTE: If the predictor variables are on different scales, 
## you should carry out the operation (x-mean(x))/sd(x) on all the variables to bring them to the same scale.
## Note that this will also take care of the need of "centering the data".
## THIS IS TO INSURE THAT NO ORIGINAL PREDICTOR VARIABLE HAS TOO MUCH SWAY IN DETERMINING THE PRINCIPLE COMPONENTS.


########################################################################################

### Let's predict Customer Churn using Principle Component Analysis:
# Download and import the ChurnIntro dataset
Churn=ChurnIntro

nums=sapply(Churn,is.numeric) ## identifies numeric columns of Churn dataset
data=Churn[,nums] ## keep only numeric columns.


## Note that we are no longer worried about correlations because Principle Component Analysis will take care of this.
dim(data)## The 8th column of data is our response variable LEAVEnum.

##We can obtain principle components with:
pca=prcomp(data[,-8],scale=TRUE) ## LEAVE out the response variable ### THIS IS VERY IMPORTANT, otherwise (for each observation) you are including information from the response variable in a model to predict the response variable.
plot(pca$sd^2/sum(pca$sd^2),type="l",ylab="Percentage of Variance")

#Note that most of the variability is captured by the three/four principle component!
#We can simply use the model LEAVEnum=\alpha+\beta1*PCA1+\beta2*PCA2+\beta3*PCA3+\beta4*PCA4+\epsilon:

model=lm(data$PROB_OF_STAY~pca$x[,1:4])
model
#######################################



## Let's do n-fold cross validation to see which model does better on a simulated dataset:

x1=sample(-50:50,100,replace=TRUE)
x2=sample(1:100,100,replace=TRUE)
x3=3+2*x1+rnorm(100,sd=20)
eps=rnorm(100,sd=200)
y= 5 + 8*x1 + 6*x2 - 5*x3 + eps

data=data.frame(x1,x2,x3,y)
cor(data[,1:3])
### note that x1 and x3 are highly correlated (by design).


###### model with all original variables (probably will predict the best, but the estimated coefficients in the regression model will be off)
errors=c()
for(i in 1:100){
  mod=lm(y~.,data=data[-i,]  )
  predicted=predict(mod,newdata=data[i,])
  errors=c(errors,data$y[i]-predicted)
}
mean(errors^2) ## MSE for the model using original predictor variables
## model suggested: 
lm(y~.,data=data)
# y= \alpha + \beta1*x1 + \beta2*x2 + \beta3*x3 +\epsilon

###### model with all original variables without  using a pair of correlated variables (ignore x3)
errors=c()
for(i in 1:100){
  mod=lm(y~.,data=data[-i,-3]  )
  predicted=predict(mod,newdata=data[i,-3])
  errors=c(errors,data$y[i]-predicted)
}
mean(errors^2) ## MSE for the model using original predictor variables
## model suggested: 
lm(y~.,data=data[,-3])
# y= \alpha + \beta1*x1 + \beta2*x2 +\epsilon


##### model using principle component analysis:
pca=prcomp(data[,-4])
plot(pca$sd^2/sum(pca$sd^2),type="l",ylab="Percentage of Variance")
## Note that most of the variance is located in the first 2 principle component:
PCdata=data.frame(pca$x,y)

errors=c()
for(i in 1:100){
  mod=lm(y~.,data=PCdata[-i,c(1:2,4)])  ## note that we are grabbing the first and second columns of PCdata (PC1 and PC2) and the last column of PCdata (y)
  predicted=predict(mod,newdata=PCdata[i,c(1:2,4)])
  errors=c(errors,PCdata$y[i]-predicted)
}
mean(errors^2) ## MSE for the model using the first two principle componenets
lm(y~.,data=PCdata[,c(1:2,4)])
# y= \alpha + \beta1*PC1 + \beta2*PC2 +\epsilon

pca$rotation # can be used to figure out the value of PC1 and PC2 given an observations x1, x2, x3 values



