# Natan Alper - Homework 4
# 1a

x1 <- c("Republican", "Republican", "Democrat", "Independent", "Democrat", "Independent", "Democrat", "Independent")
y <- c(10, 8, 5, 3, 8, 5, 10, 2)

data <- data.frame(y, x1)
data

repIND <- (data$x1=="Republican")*1
demIND <- (data$x1=="Democrat")*1

newData <- data.frame(data, repIND, demIND)
newData

model <- lm(y~repIND+demIND,data=newData)
model

# 1e
mod1 <- lm(y~repIND+demIND, data=newData[c(-1),])
mod1
pred1 <- predict(mod1,newdata=newData[c(1),])
pred1

mod2 <- lm(y~repIND+demIND, data=newData[c(-2),])
mod2
pred2 <- predict(mod2,newdata=newData[c(2),])
pred2

mod3 <- lm(y~repIND+demIND, data=newData[c(-3),])
mod3
pred3 <- predict(mod3,newdata=newData[c(3),])
pred3

mod4 <- lm(y~repIND+demIND, data=newData[c(-4),])
mod4
pred4 <- predict(mod4,newdata=newData[c(4),])
pred4

mod5 <- lm(y~repIND+demIND, data=newData[c(-5),])
mod5
pred5 <- predict(mod5,newdata=newData[c(5),])
pred5

mod6 <- lm(y~repIND+demIND, data=newData[c(-6),])
mod6
pred6 <- predict(mod6,newdata=newData[c(6),])
pred6

mod7 <- lm(y~repIND+demIND, data=newData[c(-7),])
mod7
pred7 <- predict(mod7,newdata=newData[c(7),])
pred7

mod8 <- lm(y~repIND+demIND, data=newData[c(-8),])
mod8
pred8 <- predict(mod8,newdata=newData[c(8),])
pred8

allpredictions <- c(pred1,pred2,pred3,pred4,pred5,pred6,pred7,pred8)
allpredictions

SSE <- sum((newData$y-allpredictions)^2)
SSE

# 1f
mod1f <- lm(y~repIND+demIND, data=newData[c(-1,-2,-3,-4),])
mod1f
pred1f <- predict(mod1f,newdata=newData[c(1,2,3,4),])
pred1f

mod2f <- lm(y~repIND+demIND, data=newData[c(-5,-6,-7,-8),])
mod2f
pred2f <- predict(mod2f,newdata=newData[c(5,6,7,8),])
pred2f

allpredictions <- c(pred1f,pred2f)
allpredictions

MAE <- sum(abs(newData$y-allpredictions))/8
MAE

# 3
x1 <- c(50000, 360123, 230000, 114095)
x2 <- c(300, 457, 193, 645)
y <- c(8, 16, 10.5, 10)
dataset <- data.frame(x1, x2, y)

# Log transformation
logx1 <- log(x1)
logx2 <- log(x2)
plot(logx1, y)
plot(logx2, y)

# Raise to the .5 transformation
x1sqrt <- x1^.5
x2sqrt <- x2^.5
plot(x1sqrt, y)
plot(x2sqrt, y)

# Raise to the .2 transformation
x1p2 <- x1^.2
x2p2 <- x2^.2
plot(x1p2, y)
plot(x2p2, y)

# Raise to the -.2 transformation
x1p2 <- x1^(-.2)
x2p2 <- x2^(-.2)
plot(x1p2, y)
plot(x2p2, y)

