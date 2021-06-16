
### Download and import the cancer_reg dataset
View(cancer_reg)
dim(cancer_reg)

cancer=cancer_reg[,-11] ## we for sure can't use the "Geography" column.
cancer=na.omit(cancer)
### Note that we lose a ton of observations because of missing values.
### Previously we would have just thrown these observations out. Let's try something smarter (Imputing Missing Data from Available Data):
### Let's create regression models that predict the value that should be imputed based on other variables in the dataset.

cancer=cancer_reg[,-11] 
## Let's check which columns have missign date:
num_missing=c()
for(k in 1:ncol(cancer)){
  num_missing=c(num_missing,sum(is.na(cancer[,k])))
}
num_missing

colnames(cancer)[num_missing>0]

### Notice that "PctSomeCol18_24" should add up to 100 with columns: "PctNoHS18_24",  "PctHS18_24", "PctBachDeg18_24"
non_missing_data=cancer[!is.na(cancer$PctSomeCol18_24),]
missing_data=cancer[is.na(cancer$PctSomeCol18_24),]
missing_data$PctSomeCol18_24=100-(missing_data$PctNoHS18_24+ missing_data$PctHS18_24+missing_data$PctBachDeg18_24)
cancer=rbind(non_missing_data,missing_data)
num_missing=c()
for(k in 1:ncol(cancer)){
  num_missing=c(num_missing,sum(is.na(cancer[,k])))
}
num_missing
## We still have missing value in "PctEmployed16_Over" and "PctPrivateCoverageAlone"


### let's impute missing values in column 19 ("PctEmployed16_Over")
data=cancer[!is.na(cancer$PctEmployed16_Over),]
mod=lm(PctEmployed16_Over~.,data=cancer[,c(-22)])
missing_data=cancer[is.na(cancer$PctEmployed16_Over),]
predictions=predict(mod,newdata=missing_data)
missing_data$PctEmployed16_Over=predictions
cancer=rbind(data,missing_data)

num_missing=c()
for(k in 1:ncol(cancer)){
  num_missing=c(num_missing,sum(is.na(cancer[,k])))
}
num_missing ## Now we only have missing data in "PctPrivateCoverageAlone"


### let's impute missing values in column 22 ("PctPrivateCoverageAlone")
data=cancer[!is.na(cancer$PctPrivateCoverageAlone),]

mod=lm(PctPrivateCoverageAlone~.,data=data)
missing_data=cancer[is.na(cancer$PctPrivateCoverageAlone),]
predictions=predict(mod,newdata=missing_data)
missing_data$PctPrivateCoverageAlone=predictions
cancer=rbind(data,missing_data)

num_missing=c()
for(k in 1:ncol(cancer)){
  num_missing=c(num_missing,sum(is.na(cancer[,k])))
}
num_missing ### Note that none of the data is missing now.


#### Let's consider correlations:
library(lattice)
corrmat=cor(cancer)
levelplot(corrmat,scales=list(x=list(rot=90)))

corrmat>.8 | corrmat< -.8

#"avgAnnCount" and "popEst2015"--are highly correlated

allcomb=expand.grid(row.names(corrmat),colnames(corrmat))
comb_mat=matrix(apply(allcomb,1,paste,collapse=","),ncol=ncol(corrmat))
comb_mat[ (corrmat>.8 | corrmat< -.8) & corrmat<.9999999]


### What should we do next?

cancer=cancer[,colnames(cancer)!="PctPrivateCoverage"]
cancer=cancer[,colnames(cancer)!="avgAnnCount"]
cancer=cancer[,colnames(cancer)!="povertyPercent"]
cancer=cancer[,colnames(cancer)!="MedianAgeMale"]
cancer=cancer[,colnames(cancer)!="PctMarriedHouseholds"]
cancer=cancer[,colnames(cancer)!="PctPublicCoverageAlone"]
cancer=cancer[,colnames(cancer)!="PctPrivateCoverageAlone"]
cancer=cancer[,colnames(cancer)!="PctWhite"]

dim(cancer)

# What next?
model= lm(TARGET_deathRate~.,data=cancer)
summary(model)

### Note that acc to p_values, we should only consider variables:
# incidenceRate, medIncome, MedianAgeFemale, PctHS25_Over, PctBachDeg25_Over, PctEmployed16_Over, PctUnemployed16_Over, 
# PctPublicCoverage, PctBlack, PctOtherRace, BirthRate

data=cancer[,c(1,2,3,7,10,11,12,13,14,15,16,17,19,20,22,23)]
model= lm(TARGET_deathRate~.,data=data)
summary(model)

data=data[,c(-5,-6,-7,-8)]
model= lm(TARGET_deathRate~.,data=data)
summary(model)

# Now we can use adjusted_R^2, AIC/BIC, OR cross_validation with MSE/SSE/MAE 
# in order to measure model performance

# Using AIC and BACKWARD Selection
step(lm(TARGET_deathRate~.,data=data))
# We conclude that we should keep all the variables
# The model we should use is:
# lm(formula = TARGET_deathRate ~ incidenceRate + medIncome + MedianAgeFemale + 
#    PctHS25_Over + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + 
#    PctPublicCoverage + PctBlack + PctOtherRace + BirthRate, 
#    data = data)

# Using AIC and FORWARD Selection
step(lm(TARGET_deathRate~1,data=data),direction="forward",scope=list(lower=lm(TARGET_deathRate~1,data=data),upper=lm(TARGET_deathRate~.,data=data)))

# Note that Forward and Backward selection agree! and we should use all the variables

## Let's do Backward selection with cross_validation:
# We start with checking a model that uses all the pred variables

library(boot)
cv.glm(data,glm(TARGET_deathRate~.,data=data), K=3047)$delta[1]
# The output of this is the MSE when using n_fold cross_validation and all the variables 

mse_vec=c()
for (k in 2:ncol(data)) {
  mse_vec=c(mse_vec, cv.glm(data[,-k],glm(TARGET_deathRate~.,data=data[,-k]), K=3047)$delta[1])

}

