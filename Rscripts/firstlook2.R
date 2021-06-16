## Consider the "titanic_data_no_gender" dataset located on Canvas and import it into R.
## Lets give it a simpler name:
titanic_data=titanic_data_no_gender


#Question: Is it possible to tell whether someone being female or male made it more likely that they would 
#          survive the Titanic disaster?

# Note there is no column for gender!
### We can tell whether someone is a male or female by looking carefully at their name!

as.character(titanic_data[1,colnames(titanic_data)=="Name"])
name=as.character(titanic_data[1,colnames(titanic_data)=="Name"])
## Notice that titles Mr/Ms/Miss/etc. is an identifying feature and is located after the first comma ",".


regexpr(",",name) ### obtains first location in the character string 'name' of ",". 
                    # We will use this to check the characters after the location of "," to obtain whether someone is a Mr/Ms/Miss, etc.

regexpr("rr",name) ## This is just an example that the value we get in the output is the location of the string "rr" in object 'name'

substr(name, start= regexpr(",",name)[1]+2, stop= regexpr(",",name)[1]+5) 
#substring returns the characters in the string 'name' which are in the location from 'start' to 'stop' arguments.
# Note that the start location is 2 spaces after comma which is where someones title starts!

##
#Now lets use a loop to do this for all the observations:
title=c() ## we will save someones 'title' like Mr, Ms, etc. in this vector.
for(i in 1:nrow(titanic_data)){
  name=as.character(titanic_data[i,colnames(titanic_data)=="Name"])
  title=c(title,substr(name, start= regexpr(",",name)[1]+2, stop= regexpr(",",name)[1]+5)) 
}

#lets look at the possible titles:
unique(title)
## Some less known abbreviations: Mme is Madame, Mlle is Mademoiselle

# A couple of weird ones are "the " and "Jonk", lets look to see what rows these are in:
(1:891)[title=="Jonk"]
(1:891)[title=="the "]
## We can also pull up the name of these people by:
titanic_data$Name[title=="Jonk"]
titanic_data$Name[title=="the "]
### Since we are sure of the gender based on these we will keep them.
### If we were not sure of the gender we could discard these two observations from the study.

# Two that are unclear is "Rev." and "Dr. " for reverend and doctor. Let's see who this is:
titanic_data$Name[title=="Rev."]
## seems like these are all male
titanic_data$Name[title=="Dr. "]
## seems like these are all male except one.

# Let's now create a gender column:
gender=rep("ANYSTRING",891)
gender[title=="Mr. "]= "male"
gender[title=="Mrs."]= "female"
gender[title=="Miss"]= "female"
gender[title=="Mast"]= "male"
gender[title=="Don."]= "male"
gender[title=="Rev."]= "male"
# skip Dr. for now.
gender[title=="Mme."]= "female"
gender[title=="Ms. "]= "female"
gender[title=="Majo"]= "male"
gender[title=="Lady"]= "female"
gender[title=="Sir."]= "male"
gender[title=="Mlle"]= "female"
gender[title=="Col."]= "male"
gender[title=="Capt"]= "male"
gender[title=="the "]= "female"
gender[title=="Jonk"]= "male"
gender[title=="Dr. "]= "male"
### Note we assigned one male to a female doctor. To fix this:
gender[titanic_data$Name=="Leader, Dr. Alice (Farnham)"]="female"

## Let's add the column gender to the titanic dataset:
titanic_data=cbind(titanic_data,gender)

#### Now we can solve our question of whether someone being female or male made it more likely that they would 
#          survive the Titanic disaster.

femaleSurvivors=titanic_data$gender[titanic_data$gender=="female" & titanic_data$Survived==1]
length(femaleSurvivors)
sum(titanic_data$gender=="female" & titanic_data$Survived==1) # another way to get number of female survivors
#233 female Survivors
###### ALTERNATIVELY: sum(titanic_data$gender=="female" & titanic_data$Survived==1)
sum(titanic_data$gender=="female")
#314 total females on board.
233/314 # proportion of surviving females.

maleSurvivors=titanic_data$gender[titanic_data$gender=="male" & titanic_data$Survived==1]
length(maleSurvivors)
#109 male survivors
sum(titanic_data$gender=="male")
#577 totale males on board.

109/577 # proportion of surviving males.



########## Note that there is a different question that can be asked (which has a more complicated solution):
# DOES someone being female or male make it more likely that they would survive A SHIPWRECK? (Relationship)
# What would we PREDICT as a percentage of males and females that would survive A SHIPWRECK? (Prediction)

# To answer this question, we need to come up with a model. In data analytics this model would come from a dataset.
####### We could use this dataset to solve this question, but perhaps there is a better dataset to use?
####### Consider the following link:  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3421183/  
####### Under "Associated Data" .xls file.










