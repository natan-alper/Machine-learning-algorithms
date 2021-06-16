
### We will demonstrate the use of p_values below, however first we need to account for collinearity!

######## DOWNLOAD AND IMPORT the nbastats2018-2019.csv dataset.
## Get rid of non-numeric columns and rows with NAs:
#We need the following code to get "Salary" to be read as a numeric column instead of a factor:
NBAcurr=nbastats2018.2019
NBAcurr$Salary=as.numeric(as.character(NBAcurr$Salary))
NBAcurr2=na.omit(NBAcurr)
nums=sapply(NBAcurr2,is.numeric) ## identifies numeric columns of NBAcurr dataset
NBAcurr2=NBAcurr2[,nums]

## Recall from heatmap.R that the following pairs of columns should not be used together due to collinearity:

corrmat=cor(NBAcurr2)
corrmat
library(lattice)
levelplot(corrmat)
levelplot(corrmat,scales=list(x=list(rot=90)))
## Points with FGA or FTA or MP
## Height with Weight
## FTA with FGA
## PER with WS48
## PER with BPM (corr of .78)
## VORP and OWS
## VORP and DWS (corr of .78)
## VORP and WS
## BPM and WS48
colnames(NBAcurr2)
data=NBAcurr2[,c(-2,-11,-15,-16,-22,-24,-25)] ## We get rid of weight, FGA, FTA, MP, WS48, BPM, and VORP



model=lm(Salary~.,data=data)
summary(model)

### Note that from this that based on the p-value column we can get rid of:
### Height, Blocks, Steals, Rebounds, FT., FG3., FG3A, FG., PER, OWS, DWS, WS and USG

#### The remaining columns to use as predictor variables would be Age, Points, Assists, and G
colnames(NBAcurr2)
data= NBAcurr2[,c(3,4,5,8,17)] ## note that we must not forget the 4th column (Salary)
model=lm(Salary~.,data=data)
summary(model) 
## Note that getting rid of a bunch of "noisy" columns, we now observe that G has a high p_value.

data= NBAcurr2[,c(3,4,5,8)] ## note that we must not forget the 4th column (Salary)
model=lm(Salary~.,data=data)
summary(model) ## Note that getting rid of a bunch of "noisy" columns, we now observe that G has a high p_value.

## The final potential predictor variables would be Age, Points and Assists!





