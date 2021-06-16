x1=rexp(10000)
x2=rweibull(10000,10)

hist(x1)
hist(x2)

y=10+4*x1^(-1/3)+3*sqrt(x2)+rnorm(10000,0,3) ## Note that y does not have a linear relationship with neither x1 and x2.
## y has a linear relationship with x1^(-1/3) and a linear relationship with x^(.5)

plot(x1,y)
data=data.frame(x1,x2,y) ## this is what the data we see would look like.

### Let's try to search for the transfomration which leads to the most linear relationship between x1 and y.

par(mfrow=c(3,4)) ## Allows us to output 12 scatterplots in 1 window.

plot(x1^2,y)
plot(x1^(1.5),y)
plot(x1^(1.2),y)
plot(x1^(.8),y)
plot(x1^(.5),y)
plot(x1^(.2),y)
plot(log(x1),y)
plot(x1^(-.2),y)
plot(x1^(-.5),y)
plot(x1^(-.8),y)
plot(x1^(-1.2),y)
plot(x1^(-1.5),y)

### Note that the plot of y versus x1^(-.2) is most linear (This is because the actual linear relationship is with respect to x^-.3333)

par(mfrow=c(1,1))
library(MASS)
boxcox(lm(y~x1))
## Note that this comes relatively close to determining the best transformation.

## Let's look at the histograms of x1 and x2 again, and note their skewness:
hist(x1)
hist(x2)
## x1 is right skewed while x2 is left skewed

### We can again search for the transformation which results in the most "Bell-shaped" Histogram!
par(mfrow=c(3,4))

hist(x1^2)
hist(x1^(1.5))
hist(x1^(1.2))
hist(x1^(.8))
hist(x1^(.5))
hist(x1^(.2))
hist(log(x1))
hist(x1^(-.2))
hist(x1^(-.5))
hist(x1^(-.8))
hist(x1^(-1.2))
hist(x1^(-1.5))

## We can also use the Box-Cox approach:
par(mfrow=c(1,1))
boxcox(lm(x1~1))

########### And the same goes for x2:
par(mfrow=c(3,4)) ## Allows us to output 12 scatterplots in 1 window.
hist(x2^2)
hist(x2^(1.5))
hist(x2^(1.2))
hist(x2^(.8))
hist(x2^(.5))
hist(x2^(.2))
hist(log(x2))
hist(x2^(-.2))
hist(x2^(-.5))
hist(x2^(-.8))
hist(x2^(-1.2))
hist(x2^(-1.5))

par(mfrow=c(1,1))
boxcox(lm(x2~1))

## Let's suppose now we wanted to estimate an OLR model using the data in x1, x2, and y.
## Suppose our goal is to generate get as linear a relationship as possible between y and our predictor variables.

### From line 34, we see that it is ideal to let pred1=x1^(-.2)
# Since we have not explored what transformation of x2 leads to a linear relationship, lets do so now:
boxcox(lm(y~x2)) 
boxcox(lm(y~x2), plotit=FALSE) 
## note that it seems like we should use pred2=x2^(-.2) is the best according to boxcox(). 
## In fact we know that the best is actually sqrt(x2) based on line 7, but we will trust in boxcox.

# Thus the model we would estimate would be:
pred1=x1^(-.2)
pred2=x2^(-.2)
lm(y~pred1+pred2)
### Note that there are two ways to write this mdodel:
#y= \alpha + \beta1*pred1 + \beta2*pred2 + \eps
# or
#y= \alpha + \beta1*x1^(-.2) + \beta2*x2^(-.2) + \eps










