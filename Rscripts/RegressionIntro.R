## Here we will demonstrate how to use a regression model to 1) make predictions about the price of a home given certain
## characteristics about the property, and 2) uncover relationships between characteristics of a home. In this case we will 
## try to figure out which features of a house affect the price of the house.

### Download and import the HOMES.csv dataset

##### Note the columns of the HOMES dataset. We need to decide which one to use as the response variable.
######## This will be the variable whose values we will predict, and try to find its relationship to other variables contained in the datset.

## In order to estimate a OLR model we use the lm() function.
?lm
### To estimate an OLR model with "Value" being the column of dataset "HOMES" containing information on the response variable,
### using information on how big the house is, the number of bedrooms,the number of bathrooms, the total number of rooms and age of the house.
### we use the following code:
lm(Value~Lotsize+Bed+Bath+Rooms+Age,data=HOMES)

##predictions for value of each home in dataset:
97.962+1.5441*HOMES$Lotsize+.8288*HOMES$Bed+25.6778*HOMES$Bath+11.0958*HOMES$Rooms-.6812*HOMES$Age

model=lm(Value~Lotsize+Bed+Bath+Rooms+Age,data=HOMES)
model ### Just prints the estimated coefficients
model$fit ##also predictions for value of each home in dataset:

## Suppose there is another house in the neighborhood that has Lotsize=8, Bed=4, Bath=2, Rooms=8, Age= 25.
## Note that our best prediction for the home using the OLR model above is:
97.962+1.5441*(8)+.8288*(4)+25.6778*(2)+11.0958*(8)-.6812*(25)


summary(model)
## can be used to determine confidence intervals
## and conduct hypothesis tests.




## Here we will demonstrate how response variable values are generated when there is an 
## Ordinary Linear Regression relationship between variables.

## Suppose the OLR relationship is given by y= 5 + 8x_1 + 6x_2 - 5x_3 + \epsilon
## So if an observation has an 
## x_1 value equal to -4
## x_2 value equal to 10
## x_3 value equal to -6
## The the response variable (y) value would be generated like so:

5+8*(-4)+6*(10)-5*(-6)+rnorm(1) 
## Note that rnorm(1) produces one outcome of a Normal Random Variable with mean=0 and variance=1.
## Note that variance does not necessarily need to be 1.


## Lets quickly create a dataset of 10 observations (where we "preset" the x_1, x_2, and x_3 values and then generate a y value)

x_1=sample(-50:50,10)
x_2=sample(1:100,10)
x_3=sample(4:30,10)
x_1
x_2
x_3
## There are now 10 values in each of x_1, x_2, x_3
eps=rnorm(10,mean=0,sd=4) ## creates 10 outcomes of a Normal RV. We will need this when we generate the y-values for the 10 observations.

y= 5 + 8*x_1 + 6*x_2 - 5*x_3 + eps
y

RegrData= data.frame(x_1,x_2,x_3,y)
### Note that in practice, all you see is data (such as RegrData) and 
### we DO NOT KNOW the parameter values (like 5,8,6,-5) that appeared in the regression model (we also don't know what rnorm() would have produced for each observation)
### But under the assumption that such an Ordinary Linear Regression relationship exists, 
### we can try to estimate the parameters of the model using some available data.
### The function used to ESTIMATE an Ordinary Linear Regression model is lm()

lm(RegrData$y ~ RegrData$x_1+RegrData$x_2+RegrData$x_3)
## Or equally as good:
lm(y~x_1+x_2+x_3,data=RegrData)

## Note that we can save the model for future use (and call it whatever you want):
OurEstimatedModel=lm(y~x_1+x_2+x_3,data=RegrData)



#### IF you want to create a scatter plot with "response" on the y-axis and some "predictor variable" on the x-axis,
#### such as y versus x_1 above, the code is:
plot(x_1,y)








