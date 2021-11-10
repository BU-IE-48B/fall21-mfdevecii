library(data.table)
library(readr)
library(ggplot2)
library(dplyr)
library(forecast)
library(zoo)
library(stringr)
library(stats)
library(urca)
library(readxl)
library(lubridate)
library(scatterplot3d)

setwd("/Users/mfdevecii/Desktop")
getwd()

dat1=read.csv("trainx.csv",header=FALSE,sep="")
dat2=read.csv("trainy.csv",header=FALSE,sep="")
dat3=read.csv("trainz.csv",header=FALSE,sep="")

d1=data.frame("V0"=1:896)

dt1=data.table(cbind(d1,dat1))
dt2=data.table(cbind(d1,dat2))
dt3=data.table(cbind(d1,dat3))
setnames(dt1,"V1","class")
setnames(dt1,"V0","instance")
setnames(dt2,"V1","class")
setnames(dt2,"V0","instance")
setnames(dt3,"V1","class")
setnames(dt3,"V0","instance")

dt1 = melt(dt1,id.vars=c('class','instance'))

dt1[,time:=as.numeric(gsub("\\D", "", variable))-1]  # nondigitleri sil v2,v3,v4ten, geriye 2,3,4 kalýr ondan da 1 çýkar
dt1 = dt1[order(instance,time)]



dt2 = melt(dt2,id.vars=c('class','instance'))

dt2[,time:=as.numeric(gsub("\\D", "", variable))-1]  # nondigitleri sil v2,v3,v4ten, geriye 2,3,4 kalýr ondan da 1 çýkar
dt2 = dt2[order(instance,time)]



dt3 = melt(dt3,id.vars=c('class','instance'))

dt3[,time:=as.numeric(gsub("\\D", "", variable))-1]  # nondigitleri sil v2,v3,v4ten, geriye 2,3,4 kalýr ondan da 1 çýkar
dt3 = dt3[order(instance,time)]  


xy = merge(dt1,
           dt2,
           by=c("instance","class","time"))


xyz = merge(xy,
            dt3,
            by=c("instance","class","time"))

xyz=xyz[,c(1,2,5,7,9)]

sample1=xyz[instance%in%17,]
sample2=xyz[instance%in%29,] 
sample3=xyz[instance%in%27,] 
sample4=xyz[instance%in%5,] 
sample5=xyz[instance%in%2,] 
sample6=xyz[instance%in%28,] 
sample7=xyz[instance%in%7,] 
sample8=xyz[instance%in%21,] 



func_velocity = function(dat)
{

  for (i in 2:315) 
  {
    dat[i,3] = dat[i,3] + dat[i-1,3]
    dat[i,4] = dat[i,4] + dat[i-1,4]
    dat[i,5] = dat[i,5] + dat[i-1,5]
  }
  return(dat)
}

vel1=func_velocity(sample1)
vel2=func_velocity(sample2)
vel3=func_velocity(sample3)
vel4=func_velocity(sample4)
vel5=func_velocity(sample5)
vel6=func_velocity(sample6)
vel7=func_velocity(sample7)
vel8=func_velocity(sample8)

func_location = function(vel)
{
  temp = vel
  temp[1,3] = vel[1,3]/2
  temp[1,4] = vel[1,4]/2
  temp[1,5] = vel[1,5]/2
  for (i in 2:315) 
  {
    temp[i,3] = temp[i-1,3] + ((vel[i,3] + vel[i-1,3]) / 2)
    temp[i,4] = temp[i-1,4] + ((vel[i,4] + vel[i-1,4]) / 2)
    temp[i,5] = temp[i-1,5] + ((vel[i,5] + vel[i-1,5]) / 2)
  }
  return(temp)
}


loc1=func_location(vel1)
loc2=func_location(vel2)
loc3=func_location(vel3)
loc4=func_location(vel4)
loc5=func_location(vel5)
loc6=func_location(vel6)
loc7=func_location(vel7)
loc8=func_location(vel8)


par(mfrow=c(1,2))

scatterplot3d(loc1[,value.x],loc1[,value.y],loc1[,value] , color = c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                     rep(7, 15)), main = "Gesture 1",
              xlim = c(min(loc1[,3]), max(loc1[,3])),
              ylim = c(min(loc1[,4]), max(loc1[,4])),
              zlim = c(min(loc1[,5]), max(loc1[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 20,pch=19)
scatterplot3d(loc2[,value.x],loc2[,value.y],loc2[,value], color =c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                   rep(7, 15)), main = "Gesture 2",
              xlim = c(min(loc2[,3]), max(loc2[,3])),
              ylim = c(min(loc2[,4]), max(loc2[,4])),
              zlim = c(min(loc2[,5]), max(loc2[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 340,pch=19)
scatterplot3d(loc3[,value.x],loc3[,value.y],loc3[,value], color = c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                    rep(7, 15)), main = "Gesture 3",
              xlim = c(min(loc3[,3]), max(loc3[,3])),
              ylim = c(min(loc3[,4]), max(loc3[,4])),
              zlim = c(min(loc3[,5]), max(loc3[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 300,pch=19)
scatterplot3d(loc4[,value.x],loc4[,value.y],loc4[,value], color = c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                    rep(7, 15)), main = "Gesture 4",
              xlim = c(min(loc4[,3]), max(loc4[,3])),
              ylim = c(min(loc4[,4]), max(loc4[,4])),
              zlim = c(min(loc4[,5]), max(loc4[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 280,pch=19)
scatterplot3d(loc5[,value.x],loc5[,value.y],loc5[,value], color = c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                    rep(7, 15)), main = "Gesture 5",
              xlim = c(min(loc5[,3]), max(loc5[,3])),
              ylim = c(min(loc5[,4]), max(loc5[,4])),
              zlim = c(min(loc5[,5]), max(loc5[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 10,pch=19)
scatterplot3d(loc6[,value.x],loc6[,value.y],loc6[,value], color = c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                    rep(7, 15)), main = "Gesture 6",
              xlim = c(min(loc6[,3]), max(loc6[,3])),
              ylim = c(min(loc6[,4]), max(loc6[,4])),
              zlim = c(min(loc6[,5]), max(loc6[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 30,pch=19)
scatterplot3d(loc7[,value.x],loc7[,value.y],loc7[,value], color = c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                    rep(7, 15)), main = "Gesture 7",
              xlim = c(min(loc7[,3]), max(loc7[,3])),
              ylim = c(min(loc7[,4]), max(loc7[,4])),
              zlim = c(min(loc7[,5]), max(loc7[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 190,pch=19)
scatterplot3d(loc8[,value.x],loc8[,value.y],loc8[,value], color = c(rep(1, 50), rep(2, 50), rep(3, 50), rep(4, 50), rep(5, 50), rep(6, 50),
                                                                    rep(7, 15)), main = "Gesture 8",
              xlim = c(min(loc8[,3]), max(loc8[,3])),
              ylim = c(min(loc8[,4]), max(loc8[,4])),
              zlim = c(min(loc8[,5]), max(loc8[,5])),
              xlab = "X-Axis", ylab = "Y-Axis", zlab = "Z-Axis",angle = 345,pch=19)
par(mfrow=c(1,1))



loc1[, intervals:= rep(1:9,each = 35)]
loc2[, intervals:= rep(1:9,each = 35)]
loc3[, intervals:= rep(1:9,each = 35)]
loc4[, intervals:= rep(1:9,each = 35)]
loc5[, intervals:= rep(1:9,each = 35)]
loc6[, intervals:= rep(1:9,each = 35)]
loc7[, intervals:= rep(1:9,each = 35)]
loc8[, intervals:= rep(1:9,each = 35)]


l1=cbind(loc1[,mean(value.x), by = "intervals"],loc1[,mean(value.y), by = "intervals"],loc1[,mean(value), by = "intervals"])
l2=cbind(loc2[,mean(value.x), by = "intervals"],loc2[,mean(value.y), by = "intervals"],loc2[,mean(value), by = "intervals"])
l3=cbind(loc3[,mean(value.x), by = "intervals"],loc3[,mean(value.y), by = "intervals"],loc3[,mean(value), by = "intervals"])
l4=cbind(loc4[,mean(value.x), by = "intervals"],loc4[,mean(value.y), by = "intervals"],loc4[,mean(value), by = "intervals"])
l5=cbind(loc5[,mean(value.x), by = "intervals"],loc5[,mean(value.y), by = "intervals"],loc5[,mean(value), by = "intervals"])
l6=cbind(loc6[,mean(value.x), by = "intervals"],loc6[,mean(value.y), by = "intervals"],loc6[,mean(value), by = "intervals"])
l7=cbind(loc7[,mean(value.x), by = "intervals"],loc7[,mean(value.y), by = "intervals"],loc7[,mean(value), by = "intervals"])
l8=cbind(loc8[,mean(value.x), by = "intervals"],loc8[,mean(value.y), by = "intervals"],loc8[,mean(value), by = "intervals"])

plotter=function(l,data){
  par(mfrow=c(3,2))
  ts.plot(l[,2],main="X PAA")
  ts.plot(data[,3],main="X DEFAULT")
  ts.plot(l[,4],main="Y PAA")
  ts.plot(data[,4],main="Y DEFAULT")
  ts.plot(l[,6],main="Z PAA")
  ts.plot(data[,5],main="Z DEFAULT")
  
  
  return(l)
}


plotter(l1,loc1)
plotter(l2,loc2)
plotter(l3,loc3)
plotter(l4,loc4)
plotter(l5,loc5)
plotter(l6,loc6)
plotter(l7,loc7)
plotter(l8,loc8)

par(mfrow=c(1,1))


sax = function(l){
  temp1 = data.frame(stringsAsFactors=FALSE)
  
   l=as.data.frame(l)
  for (i in 1:9) {
    
    for (j in 1:3) {
      if ((-30000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(-25000))
      {
        temp1[i,j] = "A"
      }
      else if ((-25000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(-20000))
      {
        temp1[i,j] = "B"
      }
      else if ((-20000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(-15000))
      {
        temp1[i,j] = "C"
      }
      else if ((-15000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(-10000))
      {
        temp1[i,j] = "D"
      }
     
      else if ((-10000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(-5000))
      {
        temp1[i,j] = "E"
      }
      else if ((-5000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(0))
      {
        temp1[i,j] = "F"
      }
      else if ((0)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(5000))
      {
        temp1[i,j] = "G"
      }
      else if ((5000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(10000))
      {
        temp1[i,j] = "H"
      }
      else if ((10000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(15000))
      {
        temp1[i,j] = "I"
      }
      else if ((15000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(20000))
      {
        temp1[i,j] = "J"
      }
      else if ((20000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(25000))
      {
        temp1[i,j] = "K"
      }
      else if ((25000)<=as.numeric(l[i,2*j]) & as.numeric(l[i,2*j])<(30000))
      {
        temp1[i,j] = "L"
      }
     
    }
  }
  colnames(temp1)=c("X","Y","Z")
  return(temp1)
}
  print(sax(l1))
  print(sax(l2))
  print(sax(l3))
  print(sax(l4))
  print(sax(l5))
  print(sax(l6))
  print(sax(l7))
  print(sax(l8))
  