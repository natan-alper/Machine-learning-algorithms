# Natan Alper
# BICI Final Project

### 1st we will read the data in R and clean it so 
### it is ready for analysis!

# Directory path to the file- autoData
path <- "C:/Users/Natan/Documents/@YeshivaUniversity/Spring2020/Business Intelligence & Consumer Insights/Final Project/"
# Alternatively, we can import the dataset manually

# Input autoMPG data from csv file into RStudio
autoFileRaw <- read.csv(file = paste0(path,"/autoData.csv"))

# Rename columns of autoFile
names(autoFileRaw) <- c("MPG", "Cylinder_Count", "Displacement", "Horsepower", "Weight", "Acceleration", "Model_Year", "Origin", "Car_Name")

# Remove the Car_Name col (since not using) and last 2 rows (for 4-fold validation)
autoFile <- data.frame(autoFileRaw[-c(nrow(autoFileRaw), (nrow(autoFileRaw)-1)),-(ncol(autoFileRaw))])

# 3 cols that should be qualitative (cols 2, 7, 8)
autoFile$Cylinder_Count <- as.character(autoFile$Cylinder_Count)
autoFile$Model_Year <- as.character(autoFile$Model_Year)
autoFile$Origin <- as.character(autoFile$Origin)

autoFile$Origin[autoFile$Origin=="1"] <- "American"
autoFile$Origin[autoFile$Origin!="American"] <- "non-American"

# 5 cols that should be quantitative (1, 3, 4, 5, 6)
# autoFile$MPG is numeric
# autoFile$Displacment is numeric
# autoFile$Acceleration is numeric
autoFile$Horsepower <- as.numeric(autoFile$Horsepower)
autoFile$Weight <- as.numeric(autoFile$Weight)

autoQuant <- autoFile[,c(1, 3, 4, 5, 6, 8)]

### Now we will throw some models at this data and see
### what results they come up with!

# The groups we will use for 4-fold cross validation
# Only run samp code once
samp=sample((1:396), 396, replace = FALSE)
# We create 4 folds
g1=samp[1:99]
g2=samp[100:198]
g3=samp[199:297]
g4=samp[298:396]
Groups <- data.frame(g1,g2,g3,g4)


########## Logistic Regression ##########

library(boot)

# Converting character data into factors & Origin into 1s & 0s 
# 1=American, 0=non-American
autoFactor <- data.frame(autoFile$MPG,as.factor(autoFile$Cylinder_Count), autoFile$Displacement, autoFile$Horsepower, autoFile$Weight, autoFile$Acceleration, as.factor(autoFile$Model_Year), autoFile$Origin)
names(autoFactor) <- c("MPG", "Cylinder_Count", "Displacement", "Horsepower", "Weight", "Acceleration", "Model_Year", "Origin")
autoFactor$Origin <- as.character(autoFactor$Origin)
autoFactor$Origin[autoFactor$Origin=="American"] <- 1
autoFactor$Origin[autoFactor$Origin!=1] <- 0
autoFactor$Origin <- as.factor(autoFactor$Origin)

### By default, step() selects models based on AIC.
# Backward Selection:
step(glm(Origin~.,family=binomial(),data=autoFactor))

# Forward Selection:
step(glm(Origin~1,family=binomial(),data=autoFactor),direction="forward",scope=list(glm(Origin~1,family=binomial(),data=autoFactor),upper=glm(Origin~.,family=binomial(),data=autoFactor)))


### To use BIC, we specify k=log(n) in the step() func (n=# of obs)
# Forward Selection:
step(glm(Origin~.,family=binomial(),data=autoFactor),k=log(396))

# Backward Selection:
step(glm(Origin~1,family=binomial(),data=autoFactor),direction="forward",scope=list(glm(Origin~1,family=binomial(),data=autoFactor),upper=glm(Origin~.,family=binomial(),data=autoFactor)),k=log(396))

### cv-prediction error
cv.glm(autoFactor,glm(Origin~.,family=binomial(),data=autoFactor),K=4)$delta
## We get 2 measures of avg pred error (only use the 1st)

# Getting PCA for quantitative data
pca=prcomp(autoQuant[-6])
summary(pca)

# Look for the large drop in screeplot
screeplot(pca,type="lines")

# New data is located in:
pca$x

##The coefficients in the linear combination are located here:
pca$rotation
#PC1=(0.007621734*MPG)+(-0.114227040*Displacement)+(0.016867082*Horsepower)+(-0.993281314*Weight)+(0.001351122*Acceleration)
#PC2=(-0.01547889*MPG)+(0.98530912*Displacement)+(-0.12085733*Horsepower)+(-0.11552377*Weight)+(-0.03121063*Acceleration)

PCdata=data.frame(pca$x,autoFactor$Origin)

errors=c()
for(i in 1:396){
  mod=lm(autoFactor.Origin~.,data=PCdata[-i,c(1:2,6)])
  predicted=predict(mod,newdata=PCdata[i,c(1:2,6)])
  errors=c(errors,as.numeric(PCdata$autoFactor.Origin[i])-as.numeric(predicted))
}
errors
errors[errors<=0] <- 0
errors[errors!=0] <- 1
errors

misclass_PCA <- sum(autoFactor$Origin!=errors)/length(errors)
misclass_PCA

########## Decision Trees ##########

library(rpart)
library(rpart.plot)

autoTree1 <- rpart(Origin~., data=autoFile, control=rpart.control(cp=.1))
rpart.plot(autoTree1,digits=-3)

autoTree2<- rpart(Origin~., data=autoFile, control=rpart.control(cp=.0001))
rpart.plot(autoTree2,digits=-3)

# Find optimal cp based on XERROR
printcp(autoTree2,digits=5)

autoTree3 <- rpart(Origin~., data=autoFile, control=rpart.control(cp=.02))
rpart.plot(autoTree3,digits=-3)

predictions <- c()
for(i in 1:4){
  autoTree3 <- rpart(Origin~., data=autoFile[-Groups[,i],],control=rpart.control(cp=.02)) 
  predictions_per_fold <- predict(autoTree3,type="class",newdata=autoFile[Groups[,i],]) 
  predictions <- c(predictions,as.character(predictions_per_fold))
}

misclass_cross <- sum(autoFile$Origin!=predictions)/length(predictions)
misclass_cross



########## SVM Model ##########

library(e1071)

# Only use quant variables plus Origin = autoQuant

# 1st Model: kernel=linear, cost=1
preds <- c()
for(i in 1:4){
  modelSVM <- svm(Origin~.,data=autoQuant[-Groups[,i],],kernel="linear",type="C-classification",cost=1)
  predictions_per_fold <- predict(modelSVM,type="class",newdata=autoQuant[Groups[,i],]) 
  preds <- c(preds,as.character(predictions_per_fold))
}
misclass_cross1 <- sum(autoQuant$Origin!=preds)/length(preds)

# 2nd Model: kernel=linear, cost=10
preds <- c()
for(i in 1:4){
  modelSVM <- svm(Origin~.,data=autoQuant[-Groups[,i],],kernel="linear",type="C-classification",cost=10)
  predictions_per_fold <- predict(modelSVM,type="class",newdata=autoQuant[Groups[,i],]) 
  preds <- c(preds,as.character(predictions_per_fold))
}
misclass_cross2 <- sum(autoQuant$Origin!=preds)/length(preds)

# 3rd Model: kernel=radial, cost=1
preds <- c()
for(i in 1:4){
  modelSVM <- svm(Origin~.,data=autoQuant[-Groups[,i],],kernel="radial",type="C-classification",cost=1)
  predictions_per_fold <- predict(modelSVM,type="class",newdata=autoQuant[Groups[,i],]) 
  preds <- c(preds,as.character(predictions_per_fold))
}
misclass_cross3 <- sum(autoQuant$Origin!=preds)/length(preds)

# 4th Model: kernel=radial, cost=10
preds <- c()
for(i in 1:4){
  modelSVM <- svm(Origin~.,data=autoQuant[-Groups[,i],],kernel="radial",type="C-classification",cost=10)
  predictions_per_fold <- predict(modelSVM,type="class",newdata=autoQuant[Groups[,i],]) 
  preds <- c(preds,as.character(predictions_per_fold))
}
misclass_cross4 <- sum(autoQuant$Origin!=preds)/length(preds)

# Find lowest misclass rate to identify the optimal model
misclass_cross1
misclass_cross2
misclass_cross3
misclass_cross4
# Optimal Model= 2nd Model: kernel=linear, cost=10



########## K-nearest Neighbors ##########

library(class)
# Use autoQuant
# Standardize data
for(i in 1:(ncol(autoQuant)-1)){ ## Note that response variable is assumed to be the last column here
  autoQuant[,i] <- (autoQuant[,i]-mean(autoQuant[,i]))/sd(autoQuant[,i])
}

preds <- c()
misclass_knn <- c()
for(k in 10*(1:50)){
  for(i in 1:4){
    predictions_per_fold <- knn(train=autoQuant[-Groups[,i],-6],test=autoQuant[Groups[,i],-6],cl=autoQuant[-Groups[,i],6],k=k)
    preds <- c(preds,as.character(predictions_per_fold))
  }
  misclass_knn <- c(misclass_knn, sum(autoQuant[-Groups[,i],6]!=preds)/length(preds))
}

plot(10*(1:50), misclass_knn,pch=16) # finding a choice of "k" which would lead to the lowest misclass rate.
misclass_knn

########## Cluster Analysis ##########

autoQuant <- autoFile[,c(1, 3, 4, 5, 6)]

# Find distances between observations using "euclidean" and "manhattan"
Eucl.Distances= dist(autoQuant,method="euclidean")
Manh.Distances= dist(autoQuant,method="manhattan")

# 4 different clusters, adjusting distance-method and cluster-method
# Abbreviations: (E=eucl, M=manh)  (S=single, C=complete); ES, EC, MS, MC
ClustersES=hclust(Eucl.Distances,method="single")
ClustersEC=hclust(Eucl.Distances,method="complete")
ClustersMS=hclust(Manh.Distances,method="single")
ClustersMC=hclust(Manh.Distances,method="complete")

### Misclass for ClustersES
GroupsES=cutree(ClustersES,k=2)
GroupsES

# Find the # of positions in Group 1 that = non-American in autoFile
sum(autoFile$Origin[GroupsES==1]=="non-American")#149
# ... = American
sum(autoFile$Origin[GroupsES==1]=="American")#246

## Majority of Group 1="American"-set all Group 1 positions in pred to "American"
PredES=rep(NA,396)
PredES[GroupsES==1]="American"

# Find the # of positions in Group 2 that = non-American in autoFile
sum(autoFile$Origin[GroupsES==2]=="non-American")#0
# ... = American
sum(autoFile$Origin[GroupsES==2]=="American")#1

## Majority of Group 2="American"-set all Group 1 positions in pred to "American"
PredES[GroupsES==2]="American"

misclassES=sum(autoFile$Origin[1:396]!=PredES)/396


### Misclass for ClustersEC
GroupsEC=cutree(ClustersEC,k=2)
GroupsEC

# Group 1 pos = non-American
sum(autoFile$Origin[GroupsEC==1]=="non-American")#148
# Group 1 pos = American
sum(autoFile$Origin[GroupsEC==1]=="American")#159

## Maj Group 1 = American, set all of Group 1 to American
PredEC=rep(NA,396)
PredEC[GroupsEC==1]="American"

# Group 2 pos = non-American
sum(autoFile$Origin[GroupsEC==2]=="non-American")#1
# Group 2 pos = American
sum(autoFile$Origin[GroupsEC==2]=="American")#88

## Maj Group 2 = American, set all of Group 2 to American
PredEC[GroupsEC==2]="American"

misclassEC=sum(autoFile$Origin[1:396]!=PredEC)/396


### Misclass for ClustersMS
GroupsMS=cutree(ClustersMS,k=2)
GroupsMS

# Group 1 pos = non-American
sum(autoFile$Origin[GroupsMS==1]=="non-American")#149
# Group 1 pos = American
sum(autoFile$Origin[GroupsMS==1]=="American")#246

## Maj Group 1 = American, set all of Group 1 to American
PredMS=rep(NA,396)
PredMS[GroupsMS==1]="American"

# Group 2 pos = non-American
sum(autoFile$Origin[GroupsMS==2]=="non-American")#0
# Group 2 pos = American
sum(autoFile$Origin[GroupsMS==2]=="American")#1

## Maj Group 2 = American, set all of Group 2 to American
PredMS[GroupsMS==2]="American"

misclassMS=sum(autoFile$Origin[1:396]!=PredMS)/396


### Misclass for ClustersMC
GroupsMC=cutree(ClustersMC,k=2)
GroupsMC

# Group 1 pos = non-American
sum(autoFile$Origin[GroupsMC==1]=="non-American")#3
# Group 2 pos = American
sum(autoFile$Origin[GroupsMC==1]=="American")#132

## Maj Group 1 = American, set all of Group 1 to American
PredMC=rep(NA,396)
PredMC[GroupsMC==1]="American"

# Group 2 pos = non-American
sum(autoFile$Origin[GroupsMC==2]=="non-American")#146
# Group 2 pos = American
sum(autoFile$Origin[GroupsMC==2]=="American")#115

## Maj Group 2 = American, set all of Group 2 to American
PredMC[GroupsMC==2]="non-American"

misclassMC=sum(autoFile$Origin[1:396]!=PredMC)/396

# Find lowest misclass rate to identify the optimal model
misclassES
misclassEC
misclassMS
misclassMC
# Optimal Model= ClustersMC


########## Random Forests ##########

library(randomForest)

RF=randomForest(as.factor(Origin)~.,data=autoFile, ntree=1000, mtry=4, type="class")

# Getting predictions
predictionsRF=predict(RF)

# Getting misclass rate
misclassRF=sum(autoFile$Origin!=predictionsRF)/396
misclassRF


