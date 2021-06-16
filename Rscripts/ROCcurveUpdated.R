## Download and Import the Churn dataset.

Churn=cbind(Churn,LEAVEnum=(Churn$LEAVE=="STAY")*1)


# We will consider INCOME, OVERAGE, LEFTOVER, HOUSE, SATISFACTION, CHANGE_OF_PLAN
data=Churn[,c(2,3,4,5,9,11,13)]


model=glm(LEAVEnum~., family=binomial(),data=data)

### Let's focus on the predicting the "LEAVE" class using a logistic regression model:

misclass=c()
FPR=c()
TPR=c()
for(thresh in seq(0,1,.01)){
Pred=(fitted(model)>thresh)*1
misclass=c(misclass, sum(data$LEAVEnum!=Pred)/length(Pred))

### If we want to compute the FPR for "LEAVE" class then we need to see the percentage of "STAY" individuals that are predicted to be "LEAVE":
STAYobs=(1:20000)[data$LEAVEnum==0]
FPR=c(FPR,sum(data$LEAVEnum[STAYobs]!=Pred[STAYobs])/length(Pred[STAYobs]))

### If we want to compute the TPR for "LEAVE" class then we need to see the percentage of "LEAVE" individuals that are predicted to be "LEAVE":
LEAVEobs=(1:20000)[data$LEAVEnum==1]
TPR=c(TPR,sum(data$LEAVEnum[LEAVEobs]==Pred[LEAVEobs])/length(Pred[LEAVEobs]))
}

#ROC curve:
plot(FPR,TPR,type="l")

### There is an AUC function located in the DescTools package (which needs to be installed):
library(DescTools)
AUC(FPR,TPR) 





###############################################


model=glm(diagnosis~., family=binomial(),data=Breast_cancer_data)

misclass=c()
FPR=c()
TPR=c()
for(thresh in seq(0,1,.01)){
  Pred=(fitted(model)>thresh)*1
  misclass=c(misclass, sum(Breast_cancer_data$diagnosis!=Pred)/length(Pred))
  
  
 ### If we want to compute the FPR for "1" class then we need to see the percentage of "0" individuals that are predicted to be "1":
  ZEROobs=(1:569)[Breast_cancer_data$diagnosis==0]
  FPR=c(FPR,sum(Breast_cancer_data$diagnosis[ZEROobs]!=Pred[ZEROobs])/length(Pred[ZEROobs]))
  
  ### If we want to compute the TPR for "1" class then we need to see the percentage of "1" individuals that are predicted to be "1":
  ONEobs=(1:569)[Breast_cancer_data$diagnosis==1]
  TPR=c(TPR,sum(Breast_cancer_data$diagnosis[ONEobs]==Pred[ONEobs])/length(Pred[ONEobs]))
}

plot(FPR,TPR,type="l")

### There is an AUC function located in the DescTools package (which needs to be installed):
library(DescTools)
AUC(FPR,TPR) 


########################## SVM:

misclass=c()
FPR=c()
TPR=c()
for(Cost in seq(1,5000,100)){
  model=svm(diagnosis~.,data=Breast_cancer_data,cost=Cost,type="C-classification")
  Pred=predict(model)
  misclass=c(misclass, sum(Breast_cancer_data$diagnosis!=Pred)/length(Pred))
  
  ### If we want to compute the FPR for "1" class then we need to see the percentage of "0" individuals that are predicted to be "1":
  ZEROobs=(1:569)[Breast_cancer_data$diagnosis==0]
  FPR=c(FPR,sum(Breast_cancer_data$diagnosis[ZEROobs]!=Pred[ZEROobs])/length(Pred[ZEROobs]))
  
  ### If we want to compute the TPR for "1" class then we need to see the percentage of "1" individuals that are predicted to be "1":
  ONEobs=(1:569)[Breast_cancer_data$diagnosis==1]
  TPR=c(TPR,sum(Breast_cancer_data$diagnosis[ONEobs]==Pred[ONEobs])/length(Pred[ONEobs]))
}

lines(FPR,TPR,col="red")
AUC(FPR,TPR)



########################## knn:

misclass=c()
FPR=c()
TPR=c()
for(K in seq(2,569)){
  Pred=knn(train=Breast_cancer_data[,1:5],test=Breast_cancer_data[,1:5],cl= Breast_cancer_data[,6],k=K)
  misclass=c(misclass, sum(Breast_cancer_data$diagnosis!=Pred)/length(Pred))
  
  ### If we want to compute the FPR for "1" class then we need to see the percentage of "0" individuals that are predicted to be "1":
  ZEROobs=(1:569)[Breast_cancer_data$diagnosis==0]
  FPR=c(FPR,sum(Breast_cancer_data$diagnosis[ZEROobs]!=Pred[ZEROobs])/length(Pred[ZEROobs]))
  
  ### If we want to compute the TPR for "1" class then we need to see the percentage of "1" individuals that are predicted to be "1":
  ONEobs=(1:569)[Breast_cancer_data$diagnosis==1]
  TPR=c(TPR,sum(Breast_cancer_data$diagnosis[ONEobs]==Pred[ONEobs])/length(Pred[ONEobs]))
}

lines(FPR,TPR,col="green")
AUC(FPR,TPR)




