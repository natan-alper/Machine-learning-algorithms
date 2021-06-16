#Download and Import Churn dataset

## One of the most frequently used functions for performing clustering is the hclust() function located in the default "stats" package.
## hclust is able to determine clusters based on "single-linkeage", "complet-linkeage", "average-linkeage", etc. (NOT k-means or k-medoids)

##VERY IMPORTANT: The primary input into hclust() is the distance between the observations from each other!!!
##that is, we can't just put Churn into hclust().
##Thus we need to first determine the distance between each observation using the dist() function.

##Let's consider columns INCOME, OVERAGE, LEFTOVER, and HOUSE of Churn and only the first 10000 observations (to not stress computer too much).
data=Churn[1:10000,c(2,3,4,5)] ##note that we disregard the LEAVE columns!!! This is clustering!

#It is CRUCIAL to bring all variables to the same scale!
for(k in 1:ncol(data)){
  data[,k]=(data[,k]-mean(data[,k]))/sd(data[,k])
}


#obtain the distances for all observations from each other:
Eucl.Distances= dist(data) ## we can choose different distances here like "Euclidean" (default), or "Manhattan", etc.
Manh.Distances= dist(data,method="manhattan")

length(Eucl.Distances) ## Note and understand the size of this "vector".
(10000*10000-10000)/2

##Note that Eucl.Distances is a vector of distances from each observation to the others (one-way only)
#### excluding distances between the observation and itself.


#Once we have the distances we can proceed with hclust:
Clusters=hclust(Eucl.Distances,method="complete") ### default method is "complete"
Clusters$merge[1:10,] ## $merge describes which observations/clusters were merged together at which step.



##To draw a dendrogram we can call:
plot(as.dendrogram(Clusters))
##Note that this isn't very informative since we have so many observations.
##Alternatively we can 


Split=cut(as.dendrogram(Clusters),h=1) ## Divides the tree by branches below height=1 ("lower") and branches above height=1 ("upper")
length(Split$lower)

plot(Split$lower[[1]]) ## We see which observations get grouped together first left-to-right
plot(Split$lower[[2]])
plot(Split$lower[[3]])
#etc.

##We can also plot the tree above the height we specified (1), but it's less informative
plot(Split$upper[[1]])
plot(Split$upper[[2]])


##### FOR PREDICTION and to SEE THE GROUPINGS:
## We can obtain the groups that each observations belong to using "cutree()" where we specify the number of groups we want.
## Note that in this case we know that we want to decide between two groups ("LEAVE" and "STAY")
Groups=cutree(Clusters,k=2)

################misclassification rate can be computed with:
sum(Churn$LEAVE[1:10000][Groups==1]=="LEAVE") #3574
sum(Churn$LEAVE[1:10000][Groups==1]=="STAY") #3999
## Therefore Group 1 should be predicted as "STAY"
Pred=rep(NA,10000)
Pred[Groups==1]="STAY"

sum(Churn$LEAVE[1:10000][Groups==2]=="LEAVE") #1340
sum(Churn$LEAVE[1:10000][Groups==2]=="STAY") #1087
## Therefore Group 2 should be predicted as "LEAVE"
Pred[Groups==2]="LEAVE"

missclass=sum(Churn$LEAVE[1:10000]!=Pred)/10000
missclass

## Note that it was just a guess that Groups==2 is equivalent to "LEAVE". 
## All we know based on hclust() is which observations belong to different groups (NOT THE LITTERAL MEANING OF THE GROUPS)
## We note however that, if missclass turned out to be >50% we would know that we made a mistake in assigning Groups==2 to "LEAVE" instead of "STAY".


### We can note how the groups split up in two dimensions using:
plot(data[,c(1,2)],col=Groups)
plot(data[,c(1,3)],col=Groups)
plot(data[,c(1,4)],col=Groups)
plot(data[,c(2,3)],col=Groups)
plot(data[,c(2,4)],col=Groups)
plot(data[,c(3,4)],col=Groups)

## We note that the dominant separation occurs along LEFTOVER
## Unfortunately this is not so true for the separation between "LEAVE" and "STAY"

color=rep(1,10000)
color[Churn$LEAVE[1:10000]=="STAY"]=2
plot(data[,c(1,4)],col=color)




#########################Just for experience we can think about an optimal number of groups for this dataset:
#### Note that there is no "simple" way to compute Average Intra-Cluster distance for objects returned by hclust()
### Thus we apply a more naive approach. 
### Since we have the dist() function, We will average out all the distances from observation to observation (as opposed to mean) within each cluster


## When using one cluster:
Gr=cutree(Clusters,k=1)
d=dist(data[Gr==1,]) ### Compute distances between objects in Group 1 (the only group here)
mean(d)

ONE.clustAve=mean(d)

## When using two clusters:

Gr=cutree(Clusters,k=2)
d1=dist(data[Gr==1,]) ### Compute distances between objects in Group 1
d2=dist(data[Gr==2,]) ### Compute distances between objects in Group 2

TWO.clustAve=mean(c(d1,d2)) # average of the distances within the clusters
TWO.clustAve

## When using three clusters:

Gr=cutree(Clusters,k=3)
d1=dist(data[Gr==1,])
d2=dist(data[Gr==2,])
d3=dist(data[Gr==3,])

THREE.clustAve=mean(c(d1,d2,d3))   # average of the distances within the clusters
THREE.clustAve

## When using four clusters:

Gr=cutree(Clusters,k=4)
d1=dist(data[Gr==1,])
d2=dist(data[Gr==2,])
d3=dist(data[Gr==3,])
d4=dist(data[Gr==4,])

FOUR.clustAve=mean(c(d1,d2,d3,d4)) # average of the distances within the clusters
FOUR.clustAve


## When using five clusters:

Gr=cutree(Clusters,k=5)
d1=dist(data[Gr==1,])
d2=dist(data[Gr==2,])
d3=dist(data[Gr==3,])
d4=dist(data[Gr==4,])
d5=dist(data[Gr==4,])

FIVE.clustAve=mean(c(d1,d2,d3,d4,d5)) # average of the distances within the clusters
FIVE.clustAve

################### THE AVERAGES TOGETHER:
ONE.clustAve
TWO.clustAve
THREE.clustAve
FOUR.clustAve
FIVE.clustAve

## To get a sense for the best grouping we can plot them:
plot(c(1,2,3,4,5),c(ONE.clustAve,TWO.clustAve,THREE.clustAve,FOUR.clustAve,FIVE.clustAve))

##### Note that the dropoff seems to be small from 3 to 4, so perhaps 3 groups are the best!


#########################################################################
######### To do k-means clustering we use the kmeans() function.
## Note that the kmeans() function requires the original dataset as the input (not d)
## Note that the distance between cluster means is computed as EUCLIDEAN DISTANCE (Cannot be changed)

Clusters=kmeans(data, 2) ## data defined above.
## The number 2 in this case specifies the number of groups we want in the end. 
## which means that 2 random "centers" are chosen to begin the algorithm--- in this case 2 random observations.
## We can alternatively write in a vector, which would then indicate what we want to be the starting point for the centers.

## We can obtain predicted groups with:
Groups=Clusters$cluster
sum(Churn$LEAVE[Groups==1]=="LEAVE") #3453
sum(Churn$LEAVE[Groups==1]=="STAY") #4633
## Therefore Group 1 should be predicted as "STAY"
Pred=rep("STAY",10000)
Pred[Groups==1]="STAY"

sum(Churn$LEAVE[Groups==2]=="LEAVE") #6399
sum(Churn$LEAVE[Groups==2]=="STAY") #5515
## Therefore Group 1 should be predicted as "LEAVE"
Pred[Groups==2]="LEAVE"

missclass=1-(sum(Churn$LEAVE[1:10000]==Pred))/10000
missclass

#### We can initialize cluster centers with a matrix. 
## For example if we want one of our cluster centers to be at INCOME=1, OVERAGE=-1, LEFTOVER=-1, HOUSE=1
## And         if we want the other cluster center to be at   INCOME=-.5, OVERAGE=.5, LEFTOVER=.5, HOUSE=.-5
## Then we can create the matrix:

InitCenters=matrix(c(1,-1,-1,1,-.5,.5,.5,-.5),nrow=2, byrow=TRUE)
InitCenters
## And use it as:

Clusters=kmeans(data, InitCenters) ## data defined above.

Groups=Clusters$cluster
sum(Churn$LEAVE[Groups==1]=="LEAVE") #3453
sum(Churn$LEAVE[Groups==1]=="STAY") #4633
## Therefore Group 1 should be predicted as "STAY"
Pred=rep("STAY",10000)
Pred[Groups==1]="STAY"

sum(Churn$LEAVE[Groups==2]=="LEAVE") #6399
sum(Churn$LEAVE[Groups==2]=="STAY") #5515
## Therefore Group 1 should be predicted as "LEAVE"
Pred[Groups==2]="LEAVE"

missclass=1-(sum(Churn$LEAVE[1:10000]==Pred))/10000
missclass

