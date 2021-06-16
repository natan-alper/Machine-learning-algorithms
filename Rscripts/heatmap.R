
x=c(1,2,3)
y=c(4,7,30)
z=x^2

data=data.frame(x,y,z)

corrmat=cor(data)
heatmap(corrmat,Rowv=NA,Colv=NA,scale="none") #the extra arguments "Rowv" and "Colv" are needed to remove some unwanted default output. Scale="none" is needed to generate a heatmap for the correlation matrix as given.

#Let's consider a big dataset:
### Dataset of NBA players statistics during 2018-2019 season. 
## We want to draw some heatmaps

######## DOWNLOAD AND IMPORT the nbastats2018-2019.csv dataset.

NBAcurr=nbastats2018.2019

nums=sapply(NBAcurr,is.numeric) ## identifies numeric columns of NBAcurr dataset
NBAcurr2=NBAcurr[,nums] ##only takes the numeric columns and puts them in a new dataset called NBAcurr2.
NBAcurr2=na.omit(NBAcurr2)## removes any row with NA's in at least one cell.

stdevs=sapply(NBAcurr2,sd)
NBAcurr2=NBAcurr2[,stdevs!=0]## Can't have any columns with a standard deviation of zero.

corrmat=cor(NBAcurr2)
corrmat
heatmap(corrmat,Rowv=NA,Colv=NA,scale="none")

## For a more user-friendly plot (with gradient legend), consider the lattice package and function levelplot()
library(lattice)
levelplot(corrmat)
### To fix x-labels, google: rotate labels in levelplot r
### first link tells you this:
levelplot(corrmat,scales=list(x=list(rot=90)))




