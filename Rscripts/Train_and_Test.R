
NBAcurr=nbastats2018.2019
NBAcurr$Salary=as.numeric(as.character(NBAcurr$Salary))
NBAcurr2=na.omit(NBAcurr)
nums=sapply(NBAcurr2,is.numeric) ## identifies numeric columns of NBAcurr dataset
NBAcurr2=NBAcurr2[,nums]

### Recall from p_values.R that the full set of predictor variables that should be used are: Age, Points and Assists:
data= NBAcurr2[,c(3,4,5,8)] ## note that we must not forget the 4th column (Salary)
model=lm(Salary~.,data=data)
summary(model) 

#### Now lets use a Train/Test set to compute the three Overall Prediction Error measures:
nrow(data)## There are 434 players in the current dataset!
x=sample(1:434,350) ## randomly choose players for the Train set.
dataTrain=data[x,]
dataTest=data[-x,] 

model=lm(Salary~.,data=dataTrain)
predictions=predict(model, newdata=dataTest[,-2]) ## to come up with predictions we don't include the Salary column in the newdata argument!

### SSE:
sum((dataTest$Salary-predictions)^2)

### MSE:
sum((dataTest$Salary-predictions)^2)/length(predictions)

### MAE:
sum(abs(dataTest$Salary-predictions))/length(predictions)


###### Now lets use a Train/Test set and MAE set to check which of the three potential models (using only Age, using only Points and using only Assists is optimal)

model=lm(Salary~Age,data=dataTrain)
predictions=predict(model, newdata=dataTest[,-2]) 
sum(abs(dataTest$Salary-predictions))/length(predictions)

model=lm(Salary~Points,data=dataTrain)
predictions=predict(model, newdata=dataTest[,-2]) 
sum(abs(dataTest$Salary-predictions))/length(predictions)

model=lm(Salary~Assists,data=dataTrain)
predictions=predict(model, newdata=dataTest[,-2]) 
sum(abs(dataTest$Salary-predictions))/length(predictions)


