x=c(3,2,5,6,1,7)
y=c(1,2,0,3,2,5)

fakedata=data.frame(x,y)
fakedata[c(-1,-2),]
#### Demonstration of Cross Validation:
## Let's do 3-fold cross validation for an OLR model.
mod1=lm(y~x, data=fakedata[c(-1,-2),])
mod1
pred1=predict(mod1,newdata=fakedata[c(1,2),])
pred1

mod2=lm(y~x, data=fakedata[c(-3,-4),])
mod2
pred2=predict(mod2,newdata=fakedata[c(3,4),])
pred2

mod3=lm(y~x, data=fakedata[c(-5,-6),])
mod3
pred3=predict(mod3,newdata=fakedata[c(5,6),])
pred3

allpredictions=c(pred1,pred2,pred3)
allpredictions

SSE=sum((fakedata$y-allpredictions)^2)
SSE

MSE=SSE/6
MSE
MSE=mean((fakedata$y-allpredictions)^2)
MSE

MAE=sum(abs(fakedata$y-allpredictions))/6
MAE
