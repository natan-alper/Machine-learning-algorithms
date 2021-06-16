# Natan Alper - Hw 6
# 1
ci <- data.frame(cps_income)
# 2 qual vars:
# race and gender
# 3 quant vars:
# Age, education_number, hours_per_week

# 1a
library(rpart)
library(rpart.plot)
ciTree <- rpart(Income~race+gender+Age+education_number+hours_per_week, data=ci)
rpart.plot(ciTree,digits=-3)

# 1b
# Find better cp
ciTree <- rpart(Income~race+gender+Age+education_number+hours_per_week, data=ci, control=rpart.control(cp=.0001))
printcp(ciTree,digits=5)
ciTree <- rpart(Income~race+gender+Age+education_number+hours_per_week, data=ci, control=rpart.control(cp=.0012))
rpart.plot(ciTree,digits=-3)

# 1c
predict(ciTree,type="prob")[1,]

# Approximately a 45% chance that the first individual earns >50K

# 1d
predictions <- predict(ciTree,type="class")
predictions[1]

# 1e
# 32560 total entries
### Only run "randSamp" once
# randSamp <- sample((1:nrow(ci)), size = 30000)
training <- ci[randSamp, ]
testing <- ci[-randSamp, ]
trainTree <- rpart(Income~race+gender+Age+education_number+hours_per_week, data=training)
# Pred using training data tree
predictions <- predict(trainTree,type="class", newdata = testing)
# Misclassification rate of when comparing to testing data
sum(testing$Income!=predictions)/length(predictions)

# 1f
tenthOfdata <- nrow(ci)/10
g1 <- 1:tenthOfdata
g2 <- ((tenthOfdata)+1):(2*tenthOfdata)
g3 <- ((2*tenthOfdata)+1):(3*tenthOfdata)
g4 <- ((3*tenthOfdata)+1):(4*tenthOfdata)
g5 <- ((4*tenthOfdata)+1):(5*tenthOfdata)
g6 <- ((5*tenthOfdata)+1):(6*tenthOfdata)
g7 <- ((6*tenthOfdata)+1):(7*tenthOfdata)
g8 <- ((7*tenthOfdata)+1):(8*tenthOfdata)
g9 <- ((8*tenthOfdata)+1):(9*tenthOfdata)
g10 <- ((9*tenthOfdata)+1):(10*tenthOfdata)
Groups <- data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)

predictions <- c()
for(i in 1:10){
  ciTree <- rpart(Income~., data=ci[-Groups[,i],],control=rpart.control(cp=.001)) 
  predictions_per_fold <- predict(ciTree,type="class",newdata=ci[Groups[,i],]) 
  predictions <- c(predictions,as.character(predictions_per_fold))
}
sum(ci$Income!=predictions)/length(predictions)

# 2
# Select only quant columns
ciQuant <- ci[, c(1,3,5,11,12,13,15)]
# Standardize data
for(i in 1:(ncol(ciQuant)-1)){ ## Note that response variable is assumed to be the last column here
  ciQuant[,i] <- (ciQuant[,i]-mean(ciQuant[,i]))/sd(ciQuant[,i])
}
train1 <- ciQuant[randSamp, -7]
test1 <- ciQuant[-randSamp, -7]
##Pred contains predictions
Pred <- knn(train=train1,test=test1,cl=ciQuant[randSamp,7],k=20) ##Note that we don't include the INCOME column in test or train
##Let's compare to the true values to compute misclassification rate:
misclass <- sum(ciQuant[-randSamp, 7]!=Pred)/length(Pred)
misclass

# 3
misclass <- c()
for(k in 5*(1:50)){
  Pred <- knn(train=train1,test=test1,cl=ciQuant[randSamp,7],k=k) 
  misclass <- c(misclass, sum(ciQuant[-randSamp, 7]!=Pred)/length(Pred))
}

plot(5*(1:50), misclass,pch=16) ## We are trying to find a choice of "k" which would lead to the lowest misclassification rate.


