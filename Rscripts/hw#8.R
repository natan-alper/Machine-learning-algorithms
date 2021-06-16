# Natan Alper - Hw 8
# 2
ci <- data.frame(cps_income)
# Select only quant columns (last col is INCOME)
ciQuantOnly <- ci[, c(1,3,5,11,12,13)]

# Bring all variables to the same scale
for(k in 1:ncol(ciQuantOnly)){
  ciQuantOnly[,k]=(ciQuantOnly[,k]-mean(ciQuantOnly[,k]))/sd(ciQuantOnly[,k])
}
# Get Manhattan distances
Manh.Distances= dist(ciQuantOnly,method="manhattan")

# Cluster data
Clusters=hclust(Manh.Distances,method="average") ### default method is "complete"
Clusters$merge[1:10,] ## $merge describes which observations/clusters were merged together at which step.

# Plot data
plot(as.dendrogram(Clusters))
##Note that this isn't very informative since we have so many observations.
##Alternatively we can- 
Split=cut(as.dendrogram(Clusters),h=1) ## Divides the tree by branches below height=1 ("lower") and branches above height=1 ("upper")

plot(Split$lower[[1]]) ## We see which observations get grouped together first left-to-right
plot(Split$lower[[2]])
plot(Split$lower[[3]])

# 3a
cerQuant <- Cereal[,(seq(4,15,1))]
Cereal[,17] <- NA

Cereal$V17[Cereal$rating<=50] <- 0
Cereal$V17[Cereal$rating>50] <- 1

# Standardize
for(k in 1:ncol(cerQuant)){
  cerQuant[,k]=(cerQuant[,k]-mean(cerQuant[,k]))/sd(cerQuant[,k])
}

eucl.Distances = dist(CerQuant, method = "euclidean")
Clusters=hclust(eucl.Distances,method="single")

# Plot data
plot(as.dendrogram(Clusters))

Groups=cutree(Clusters,k=2)
Groups

# Group 1 that are >50
sum(Cereal$rating[Groups==1]>50)
# Group 1 that are <=50
sum(Cereal$rating[Groups==1]<=50)

## Therefore Group 1 should be predicted as 0 (<=50)
Pred=rep(NA,77)
Pred[Groups==1]=0

# Group 2 that are >50
sum(Cereal$rating[Groups==2]>50)
# Group 2 that are <=50
sum(Cereal$rating[Groups==2]<=50)

## Therefore Group 2 should be predicted as " >50"
Pred[Groups==2]=1

missclass=sum(Cereal$V17!=Pred)/77
missclass

# 3b
FPR=c()
TPR=c()

Pred

# If we want to compute the FPR for ">50" class then we need to see the percentage of "<=50" individuals that are predicted to be ">50":
under50obs=(1:77)[Cereal$V17==0]
FPR=sum(Cereal$V17[under50obs]!=Pred[under50obs])/length(Pred[under50obs])
FPR # All values that were predicted to be >50 were correct

# If we want to compute the TPR for ">50" class then we need to see the percentage of ">50" individuals that are predicted to be ">50":
over50obs=(1:77)[Cereal$V17==1]
TPR=sum(Cereal$V17[over50obs]==Pred[over50obs])/length(Pred[over50obs])
TPR

