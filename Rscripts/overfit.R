## Consider a simple dataset of 3 observations, one predictor variable and one response variable.
x1=c(1,2,3)

y=c(4,12,30)  ## IMPORTANT: These y-values were generated using the model y= -12+ 14x_1+ \epsilon (ie. y=-12+14*x1+ rnorm(3,0,5))

## Note that we are working with individual vectors here instead of columns of the dataset data.
plot(x1,y,col="green",pch=16,ylim=c(0,40))

model=lm(y~x1)
model

## Note that we would predict the y-values for the observations in our dataset as:
-10.67+13*(1)
-10.67+13*(2)
-10.67+13*(3)
## Or simply
predict(model)
model$fit


## We can add these points to the plot as
points(x1,predict(model), pch=16, col="red")

## Note that for any x1 value, the predictions would fall on the line:
abline(model$coeff)

lines(x1,model$coeff[1]+model$coeff[2]*x1,col="green")

### NOTE THAT WE JUST NEED TO ADD ONE MORE VARIABLE TO PERFECTLY PREDICT Y-values in the dataset:
x2=x1^2
model2=lm(y~x1+x2)
model2
predict(model2)
### You should understand why this is an ISSUE:
### We know that the relationship is given by: y= -12+ 14x_1+ \epsilon
### yet we see perfect predictions when we estimate the model y= \alpha + \beta_1 x_1 + \beta_2 x^2_1
### which would make us erroneously think that y= \alpha + \beta_1 x_1 + \beta_2 x^2_1 is the best model.
### For instance, lets generate 100 observations using the true model:
x1new=sample(1:3,100,replace=TRUE)
ynew=-12+14*x1new+rnorm(100,0,5)
### And lets apply our two estimated models to see which one is best in predicting "ynew" values:
ypred1=model$coef[1]+model$coef[2]*x1new # predictions using (simple) "model" from line (9)
x2new=x1new^2
ypred2=model2$coef[1]+model2$coef[2]*x1new+model2$coef[3]*x2new # predictions using (more complicated) "model2" from line (29)

mean(abs(ynew-ypred1)) ## average absolute value of errors for predictions made using simple model
mean(abs(ynew-ypred2)) ## average absolute value of errors for predictions made using more complicated model
### Note that the average error is larger when using the more complicated model (even though we got perfect predictions within the original dataset)
### This is overfitting!


### Another way to see this:
#first make a grid using x1's and x2's:
x1s=seq(min(x1),max(x1),.1) ## grid of values from 1 to 3.
x1s
x2s=seq(min(x1),max(x1),.1)^2
x2s
## Note that x1s is simply a grid of values from the smallest value in x1 to the maximum value in x1.


points(x1s,model2$coeff[1]+model2$coeff[2]*x1s+model2$coeff[3]*x2s) 
lines(x1s,model2$coeff[1]+model2$coeff[2]*x1s+model2$coeff[3]*x2s) ## note how the curve "adapts" its shape to predict all the observations as well as possible.


### ALTERNATIVELY:

## Let's make a 3Dplot for how the predicted values of y look based on x1s and x2s  
#first make a grid using x1's and x2's:

seq(min(x1),max(x1),.1)
seq(min(x2),max(x2),.1)
x1s=rep(seq(min(x1),max(x1),.1),each=length(seq(min(x2),max(x2),.1)))
x2s=rep(seq(min(x2),max(x2),.1),length(seq(min(x1),max(x1),.1)))

library(rgl) ## Good package for 3d graphics
plot3d(x1,x2,y,col="red",size=10) ## shows us the scatterplot for the data in our dataset

## The following shows us how the model would predict y's for x1 and x2 values along the grid.
points3d(x1s,x2s,model2$coeff[1]+model2$coeff[2]*x1s+model2$coeff[3]*x2s) #Note the perfect predictions.

## There is nothing special about the fact that we made x2=x1^2
# Allow x2 to consist of any 3 numbers:
x2=c(3,4,2)
data.frame(x1,x2,y)
plot3d(x1,x2,y,col="red",size=10) ## shows us the data in our dataset


model2=lm(y~x1+x2)
model2
predict(model2)
x1s=rep(seq(min(x1),max(x1),.1),each=length(seq(min(x2),max(x2),.1)))
x2s=rep(seq(min(x2),max(x2),.1),length(seq(min(x1),max(x1),.1)))

## The following shows us how the model would predict ys for x1 and x2 values along the grid.
points3d(x1s,x2s,model2$coeff[1]+model2$coeff[2]*x1s+model2$coeff[3]*x2s) 
# Again, we have perfect predictions, even though the values in x2 have nothing to do with the relationship between x1 and y.




### OVERFITTING IN ACTION (example 2):
## Consider the relationship we discussed in RegressionIntro.R:

x_1=sample(-50:50,10) ## select 10 random integers between -50 and 50
x_2=sample(1:100,10)
x_3=sample(4:30,10)
eps=rnorm(10,mean=0,sd=4)
y= 5 + 8*x_1 + 6*x_2 - 5*x_3 + eps ## Note that this means the data was generated according to the TRUE regression model y= 5 + 8*x_1 + 6*x_2 - 5*x_3 + \epsilon
RegrData= data.frame(x_1,x_2,x_3,y)
########## Note that everyone's data will be different because we will all get different values in x_1, x_2, x_3, and in eps.

## Let's consider estimating a model using the first 5 observations:
x1=x_1[1:5]
x2=x_2[1:5]
x3=x_3[1:5]
yresp=y[1:5]

model3=lm(yresp~x1+x2+x3)
model3 ## again everyones estimated model will be different because the datasets are different.

## The following predicts the y-value for the observations used to estimate model3
predict(model3,data.frame(x1,x2,x3))
yresp
## Note that the predictions are close, but not perfect.

## The following predicts the y-value for the observations not used to estimate model3:
unuseddata=data.frame(x1=x_1[6:10],x2=x_2[6:10],x3=x_3[6:10])
predict(model3,newdata=unuseddata)
y[6:10]
## Note that our predictions get pretty close to the actual values.


## Now lets add in just one (unnecessary) variable:
x4=x1^2
model4=lm(yresp~x1+x2+x3+x4)
model4
## The following predicts the y-value for the observations used to estimate model3
predict(model4,data.frame(x1,x2,x3,x4))
yresp
## Note that the predictions are perfect! (IS THIS GOOD OR BAD??)


## The following predicts the y-value for the observations not used to estimate model3:
unuseddata=data.frame(x1=x_1[6:10],x2=x_2[6:10],x3=x_3[6:10],x4=x_1[6:10]^2)
predict(model4,newdata=unuseddata)
y[6:10]
## Note that these predictions are worse than the ones based on model3!!!
