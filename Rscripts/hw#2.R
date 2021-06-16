# Natan Alper
# HW#2

# 1 (d)

lm(Value ~ Lotsize + Bed + Bath + Rooms + Age, data = HOMES)

# 1 (e)

estValues <- 97.9262 + 1.5441*(HOMES$Lotsize) + 0.8288*(HOMES$Bed) + 25.6778*(HOMES$Bath) + 11.0958*(HOMES$Rooms) + -0.6812*(HOMES$Age)
difference <- estValues - HOMES$Value
max(difference)

which(difference==max(difference))
difference==max(difference)
(1:length(HOMES$Value))[difference==max(difference)]

# 2
predVars <- data.frame(HOMES$Lotsize, HOMES$Bed, HOMES$Bath, HOMES$Rooms, HOMES$Age)
names(predVars) <- c("Lotsize", "Bed", "Bath", "Rooms", "Age")
corPredVrs <- cor(predVars)
corPredVrs
corHomes <- cor(HOMES)
corHomes
heatmap(corPredVrs, Rowv=NA,Colv=NA,scale="none")
heatmap(corHomes, Rowv=NA,Colv=NA,scale="none")

# 3 (a)
HOMES_Train <- HOMES[(1:60),]
HOMES_Test <- HOMES[-(1:60),]
HOMES_Train
HOMES_Test

train_model <- lm(Value ~ Lotsize + Bed + Bath + Rooms + Age, data = HOMES_Train)
train_model

preds <- predict(train_model, newdata=HOMES_Test[,-1]) ## to come up with predictions we don't include the Value column in the newdata argument!

#MSE using all 5 pred vars
sum((HOMES_Test$Value-preds)^2)/length(preds)

# 3 (b) # Only checking combos of Lotsize, Bed, Bath and Rooms
# Lotsize only
model <- lm(Value ~ Lotsize, data = HOMES_Train)
predictions <- predict(model, newdata = HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Bed only
model <- lm(Value ~ Bed, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Bath only
model <- lm(Value ~ Bath, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Rooms only
model <- lm(Value ~ Rooms, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bed
model <- lm(Value ~ Lotsize + Bed, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bath
model <- lm(Value ~ Lotsize + Bath, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Rooms
model <- lm(Value ~ Lotsize + Rooms, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Bed + Bath
model <- lm(Value ~ Bed + Bath, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Bed + Rooms
model <- lm(Value ~ Bed + Rooms, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Bath + Rooms
model <- lm(Value ~ Bath + Rooms, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bed + Bath
model <- lm(Value ~ Lotsize + Bed + Bath, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bed + Rooms
model <- lm(Value ~ Lotsize + Bed + Rooms, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bath + Rooms
model <- lm(Value ~ Lotsize + Bath + Rooms, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Bed + Bath + Rooms
model <- lm(Value ~ Bed + Bath + Rooms, data=HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bed + Bath + Rooms
model <- lm(Value ~ Lotsize + Bed + Bath + Rooms, data = HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

### Using 4 pred vars out of 5 (5 choose 4)
# Lotsize + Bed + Bath + Rooms
model <- lm(Value ~ Lotsize + Bed + Bath + Rooms, data = HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bed + Bath + Age
model <- lm(Value ~ Lotsize + Bed + Bath + Age, data = HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bed + Rooms + Age
model <- lm(Value ~ Lotsize + Bed + Rooms + Age, data = HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Lotsize + Bath + Rooms + Age
model <- lm(Value ~ Lotsize + Bath + Rooms + Age, data = HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

# Bed + Bath + Rooms + Age
model <- lm(Value ~ Bed + Bath + Rooms + Age, data = HOMES_Train)
predictions <- predict(model, newdata=HOMES_Test[,-1])
sum((HOMES_Test$Value-predictions)^2)/length(predictions)

