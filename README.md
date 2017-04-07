install.packages("rpart")
install.packages("gbm")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("compare")
library(ggplot2)
library(rpart)
library(caret)
library(dplyr)
library(compare)

setwd("C:/Users/User/Desktop/RK/Virgin-Reports")
virgin = read.csv("Customer_Summary_Virgin_arrive_20150101-20170330_main.csv", na.strings=c("","NA"))
Virgin1 = select(virgin, -(DepartureTime:ArrivalTime))
dim(Virgin1)
str(Virgin1)

#Type casting
Virgin1$TailID = as.factor(Virgin1$TailID)


#Spliting data into train and test
train = sample_frac(Virgin1, 0.7)
sid = as.numeric(rownames(train))
Virgin_test = Virgin1[-sid,]

test = select(Virgin_test, -Uptime.MTIn.Coverage)
write.csv(Virgin_test,"virgin_test.csv",row.names = F)
write.csv(train,"virgin_train.csv",row.names = F)

dim(train)
dim(test)
summary(train)
#EDA
hist(train$Uptime.MTIn.Coverage)
#Flight Vs Uptime MT In Coverage
ggplot(subset(train, TailID %in% c("N285VA","N281VA","N282VA","N283VA", "N284VA", "N284VA")),
       aes(x = FlightID, y = Uptime.MTIn.Coverage, color=TailID )) + geom_point()

  



set.seed(100)
tr_ctrl = trainControl(method="cv")
cart_grid = expand.grid(.cp=c(0,0.005,0.006,0.009,0.05))
cart_model1 = train(Uptime.MTIn.Coverage ~ FlightID + TailID+From + To + Uptime.MTGate.Gate + Uptime.ACRUGate.Gate + Uptime.ACRUIn.Coverage, train, method="rpart", trControl = tr_ctrl, tuneGrid = cart_grid)
cart_model1$finalModel

test$Uptime.MTIn.Coverage = predict(cart_model1, test)
result = test[,c("FlightID","TailID","From","To","Uptime.MTIn.Coverage")]
write.csv(result,"Flight_result.csv",row.names = F)

