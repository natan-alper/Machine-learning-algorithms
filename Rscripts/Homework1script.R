# Natan Alper - Homework # 1
titanic_data <- titanic_data_no_gender
nflTeams <- NFLTEAMVALUES2018

# 1 (a)
FullName <- c()

for (i in (1:nrow(titanic_data))) {
  name <- as.character(titanic_data[i,colnames(titanic_data)=="Name"]) # retrieve name of ith row
  comma_pos <- regexpr(",",name) # position of comma
  period_pos <- regexpr("\\.",name) # position of period
  total <- nchar(name) # total number of characters in string "name"
  FullName <- c(FullName, paste(substr(name, start=period_pos+2, stop=total), substr(name, start=1, stop=comma_pos-1)))
}
FullName

# 1 (b)
survivorFullName <- c()

for (i in (1:nrow(titanic_data))) {
  if (titanic_data$Survived[i]==1) {
    name <- as.character(titanic_data[i,colnames(titanic_data)=="Name"]) # retrieve name of ith row
    comma_pos <- regexpr(",",name) # position of comma
    period_pos <- regexpr("\\.",name) # position of period
    total <- nchar(name) # total number of characters in string "name"
    survivorFullName <- c(survivorFullName, paste(substr(name, start=period_pos+2, stop=total), substr(name, start=1, stop=comma_pos-1)))
  }
}
survivorFullName

# 2 (b)
lm(Current_Value~Debt_to_Value+Revenue+Operating_Income,data=nflTeams)

# 2 (d)
teamValues <- 55.678 + 1553.736*nflTeams$Debt_to_Value + 4.951*nflTeams$Revenue + 2.226*nflTeams$Operating_Income
teamValues

# 2 (e)
55.678 + 1553.736*nflTeams$Debt_to_Value[8] + 4.951*nflTeams$Revenue[8] + 2.226*nflTeams$Operating_Income[8]
2850-2833.561

# 2 (f) scatter plot
plot(teamValues, nflTeams$Revenue)

# 3 (a)
# OLR Model: y = 10 + 3*x1 + 5*x2 + eps
x1 = sample(1:50, 30)
x2 = sample(-25:25, 30)
eps = rnorm(30, mean=0, sd=2)
y = 10 + 3*x1 + 5*x2 + eps # OLR Model
y

# 3 (c)
olrData= data.frame(x1, x2, y)
lm(olrData$y ~ olrData$x1 + olrData$x2)

# 3 (d)
x_1 = sample(1:50, 30)
x_2 = sample(-25:25, 30)
epsi = rnorm(30, mean=0, sd=20)
y_1 = 10 + 3*x1 + 5*x2 + epsi
y_1

olrData_1= data.frame(x_1, x_2, y_1)
lm(olrData_1$y_1 ~ olrData_1$x_1 + olrData_1$x_2)
