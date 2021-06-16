# 1 Look in stepwise --- backwards selection
# 1(a)
x1 <- sample(1:100,40)
x2 <- sample(100:200,40)
x3 <- sample(200:300,40)
x1sq <- x1^2
x2sq <- x2^2
ep <- rnorm(40,sd=6)
y <- 5 - 6*x1 + 4*x2 + 2*x1^2 + ep
data1 <- data.frame(x1,x2,x3,y)
data2 <- data.frame(x1,x2,x3,x1sq,x2sq,y)

# Adjusted R^2
# 1st step
model <- lm(y~.,data=data2)
model
summary(model)
summary(lm(y~.,data=data2[,-1]))$adj
summary(lm(y~.,data=data2[,-2]))$adj
summary(lm(y~.,data=data2[,-3]))$adj
summary(lm(y~.,data=data2[,-4]))$adj
summary(lm(y~.,data=data2[,-5]))$adj # removing 5

summary(lm(y~.,data=data2[,c(-1,-5)]))$adj
summary(lm(y~.,data=data2[,c(-2,-5)]))$adj
summary(lm(y~.,data=data2[,c(-3,-5)]))$adj # removing 3 and 5
summary(lm(y~.,data=data2[,c(-4,-5)]))$adj

summary(lm(y~.,data=data2[,c(-1,-3,-5)]))$adj
summary(lm(y~.,data=data2[,c(-2,-3,-5)]))$adj
summary(lm(y~.,data=data2[,c(-4,-3,-5)]))$adj

# removing 3 and 5 yield best results

lm(y~.,data=data2[,c(-3,-5)])

# 1(b) AIC
step(lm(y~.,data=data2))

# 1(c)
lm(y~.,data=data2[,c(-3,-5)])
# Yes! 

# 4
var1 <- rnorm(1000,mean=0)
var2 <- rnorm(1000,mean=0)
var3 <- rnorm(1000,mean=0)
var4 <- rnorm(1000,mean=0)
var5 <- c("biology", "chemistry")
var6 <- c("mathematics", "history")

NewData <- data.frame(var1,var2,var3,var4,var5,var6)

# 4a
dataVar1 <- data.frame(var2, var3, var2^2)
model <- lm(var1~., data=dataVar1)
model

# 4b
preds <- -0.08356 - 0.04044*var2 + 0.03321*var3 + 0.02263*var2^2

# 4c
error <- var1 - preds
mean(error^2)

# 4d
mean(abs(error))

# 4e
step(lm(var4~.,data=NewData))
