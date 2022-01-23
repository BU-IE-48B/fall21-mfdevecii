#Group Token - 0fad2645-0417-4420-bf1c-45745432e728

library(data.table)
library(lubridate)
library(ggplot2)
library(rpart)
library(rattle)
library(nnet)

setwd("D:/Lectures/(2021-2022) Lectures Senior/IE48B/Project")
data = read.csv("bulk_imbalance.csv")
data = as.data.table(data)
data = data[,-c(3,10,11,12)]
setnames(data, names(data), c('date', 'hour', 'yal_one', 'yal_two', 'yal_three',
                              'yat_one', 'yat_two', 'yat_three'))
data$date = as.Date(data$date)
data$yal_three = as.numeric(data$yal_three)
data$yat_three = as.numeric(data$yat_three)
data[, day := weekdays(date)]
data[, month := month(date)]

weather = read.csv("2022-01-22_weather.csv")
weather = as.data.table(weather)
weather$date = as.Date(weather$date)
weather = weather[order(date,hour),]

data[, total_up := data[,3] + data[,4] + data[,5]]
data[, total_down := data[,6] + data[,7] + data[,8]]
data[, total_imb := total_down - total_up]
data[, sign := ifelse(total_imb >= 50, 1, 0)]
data[sign == 0, sign := ifelse(total_imb <= (-50), -1, 0)]

data = data[date < "2022-01-09"]

count = as.Date("2019-01-01")
data[, DSWRF_surface := 0]
data[, RH_2.m.above.ground := 0]
data[, TMP_2.m.above.ground := 0]
data[, TCDC_low.cloud.layer := 0]
data[, ws_10m := 0]
data[, wdir_10m := 0]
while (count < "2022-01-09")
{
  print(count)
  for (i in 0:23) 
  {
    data[date == count & hour == i, DSWRF_surface := mean(weather[date == count & hour == i & variable == "DSWRF_surface"]$value)]
    data[date == count & hour == i, RH_2.m.above.ground := mean(weather[date == count & hour == i & variable == "RH_2.m.above.ground"]$value)]
    data[date == count & hour == i, TMP_2.m.above.ground := mean(weather[date == count & hour == i & variable == "TMP_2.m.above.ground"]$value)]
    data[date == count & hour == i, TCDC_low.cloud.layer := mean(weather[date == count & hour == i & variable == "TCDC_low.cloud.layer"]$value)]
    data[date == count & hour == i, ws_10m := mean(weather[date == count & hour == i & variable == "ws_10m"]$value)]
    data[date == count & hour == i, wdir_10m := mean(weather[date == count & hour == i & variable == "wdir_10m"]$value)]
  }
  count = count + 1
}

data_test = tail(data, 24)
data_train = data[1:(nrow(data) - nrow(data_test)),]

model1 = lm(total_imb ~ as.factor(hour) + as.factor(day) + as.factor(month), data_train)
data_test[, total_imb_pred1 := predict(model1, data_test)]
data_test[, sign_pred1 := ifelse(total_imb_pred1 >= 50, 1, NA)]
data_test[is.na(sign_pred1), sign_pred1 := ifelse(total_imb_pred1 <= (-50), -1, 0)]
data_test[, acc1 := ifelse(sign == sign_pred1, 1, 0)]
accuracy_1 = sum(data_test$acc1[1:322]) / 322
accuracy_1 
#0.4705882 - accuracy calculated with test set of last 34 hours
#0.5454545 - accuracy calculated with test set of last 154 hours
#0.5324675 - accuracy calculated with test set of last 322 hours
#summary(model2)

model2 = lm(total_imb ~ as.factor(hour) + as.factor(day) + as.factor(month) + DSWRF_surface + RH_2.m.above.ground + TMP_2.m.above.ground + 
              TCDC_low.cloud.layer + ws_10m + wdir_10m, data_train)
data_test[, total_imb_pred2 := predict(model2, data_test)]
data_test[, sign_pred2 := ifelse(total_imb_pred2 >= 50, 1, 0)]
data_test[sign_pred2 == 0, sign_pred2 := ifelse(total_imb_pred2 <= (-50), -1, 0)]
data_test[, acc2 := ifelse(sign == sign_pred2, 1, 0)]
accuracy_2 = sum(data_test$acc2[1:322]) / 322
accuracy_2 
#0.5588235 - accuracy calculated with test set of last 34 hours
#0.5454545 - accuracy calculated with test set of last 154 hours
#0.5279503 - accuracy calculated with test set of last 322 hours
#summary(model3)

model3 = multinom(sign ~ DSWRF_surface + RH_2.m.above.ground + TMP_2.m.above.ground + 
                    TCDC_low.cloud.layer + ws_10m + wdir_10m, data_train)
data_test[, sign_pred3 := predict(model3, newdata = data_test, "class")]
data_test[, acc3 := ifelse(sign == sign_pred3, 1, 0)]
accuracy_3 = sum(data_test$acc3[1:322]) / 322
accuracy_3 
#0.6764706 - accuracy calculated with test set of last 34 hours
#0.6168831 - accuracy calculated with test set of last 154 hours
#0.5838509 - accuracy calculated with test set of last 322 hours
#summary(model4)

model4 = rpart(as.factor(sign) ~ as.factor(hour) + as.factor(day) + as.factor(month),
               data_train, control = rpart.control(minsplit = 20, minbucket = 10, cp = 0, maxdepth = 4))
data_test[, sign_pred4 := predict(model4, data_test, type = "class")]
data_test[, acc4 := ifelse(sign == sign_pred4, 1, 0)]
View(data_test)
accuracy_4 = sum(data_test$acc4[1:322]) / 322
accuracy_4
#fancyRpartPlot(model7)
#0.8235294 - accuracy calculated with test set of last 34 hours
#0.6493506 - accuracy calculated with test set of last 154 hours
#0.5807453 - accuracy calculated with test set of last 322 hours

model5 = rpart(as.factor(sign) ~ DSWRF_surface + RH_2.m.above.ground + TMP_2.m.above.ground + 
                 TCDC_low.cloud.layer + ws_10m + wdir_10m,
               data_train, control = rpart.control(minsplit = 20, minbucket = 10, cp = 0, maxdepth = 5))
data_test[, sign_pred5 := predict(model5, data_test, type = "class")]
data_test[, acc5 := ifelse(sign == sign_pred5, 1, 0)]
accuracy_5 = sum(data_test$acc5[1:322]) / 322
accuracy_5
#0.6176471 - accuracy calculated with test set of last 34 hours
#0.5454545 - accuracy calculated with test set of last 154 hours
#0.5372671 - accuracy calculated with test set of last 322 hours





