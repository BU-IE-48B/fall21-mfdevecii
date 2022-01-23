#Group Token - 0fad2645-0417-4420-bf1c-45745432e728

library(data.table)
library(lubridate)
library(ggplot2)
library(rpart)
library(rattle)
library(nnet)

data = read.csv("bulk_imbalance.csv")
data = as.data.table(data)
data$date = as.Date(data$date)
data[, day := weekdays(date)]
data[, month := month(date)]

data[, sign := ifelse(net >= 50, 1, 0)]
data[sign == 0, sign := ifelse(net <= (-50), -1, 0)]

data = data[, c(1,2,3,13,14,15)]

data_test = tail(data, 24) #modele karar verince bunu 24 yap
data_train = data[1:(nrow(data) - nrow(data_test)),]

model7 = rpart(as.factor(sign) ~ as.factor(hour) + as.factor(day) + as.factor(month),
               data_train, control = rpart.control(minsplit = 20, minbucket = 10, cp = 0, maxdepth = 5))
data_test[, sign_pred7 := predict(model7, data_test, type = "class")]
