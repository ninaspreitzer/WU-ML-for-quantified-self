library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)


#upload csv files
accelerometer <- read_csv(file.choose())
barometer <- read_csv(file.choose())
gyroscope <- read_csv(file.choose())
linear_accelerometer <- read_csv(file.choose())
location <- read_csv(file.choose())
magnetometer <- read_csv(file.choose())

#rename column names
accelerometer<- rename(accelerometer, c("Time (s)"="time", "X (m/s^2)"="Xms2", "Y (m/s^2)"="Yms2", "Z (m/s^2)"="Zms2"))
barometer <- rename(barometer, c("Time (s)"="time", "X (hPa)"="Xhpa"))
gyroscope <- rename(gyroscope, c("Time (s)"="time", "X (rad/s)"="Xrads", "Y (rad/s)"="Yrads", "Z (rad/s)"="Zrads"))
linear_accelerometer <- rename(linear_accelerometer, c("Time (s)"="time", "X (m/s^2)"="Xms2", "Y (m/s^2)"="Yms2", "Z (m/s^2)"="Zms2"))
location <- rename(location, c("Time (s)"="time", "Latitude (°)"="latitude", "Longitude (°)"="longitude", "Height (m)"="height",  "Velocity (m/s)"="velocity", "Direction (°)"="direction", "Horizontal Accuracy (m)"="hor_acc","Vertical Accuracy (°)"="ver_acc"))
magnetometer <- rename(magnetometer, c("Time (s)"="time", "X (µT)"="Xmut", "Y (µT)"="Ymut", "Z (µT)"="Zmut"))


head(accelerometer)
head(barometer)
head(gyroscope)
head(linear_accelerometer)
show(location)
head(magnetometer)

#extract time columns 
acc_time <- accelerometer$time
bar_time <- barometer$time
gyro_time <- gyroscope$time
linacc_time <- linear_accelerometer$time
loc_time <- location$time
mag_time <- magnetometer$time

#put all time values into one dataframe and drop duplicates
time <- c(acc_time, bar_time, gyro_time, linacc_time, loc_time, mag_time)
time_df <- data.frame(time)
head(time_df)
tail(time_df)
time_df2 <- unique(time_df)
tail(time_df2)


#merge all attributes for each time value 
list_all <- list(accelerometer, barometer, gyroscope, linear_accelerometer,location, magnetometer)
show(list_all)
for(i in list_all) {
  df_all <- merge(df_all, i, by="time", all=TRUE)
}

head(df_all)

#fill NA with last known values
df_all <- na.locf(df_all, na.rm=FALSE)
df_all <- na.locf(df_all, na.rm=FALSE, fromLast=TRUE)

head(df_all)


#define delta t, time steps
dt_1 <- round(max(df_all$time))
dt_0.1 <- round(max(df_all$time)/0.1)
dt_0.01 <- round(max(df_all$time)/0.01)

list_dt <- list(dt_1, dt_0.1, dt_0.01)
name_col <- c("dt_1", "dt_0.1", "dt_0.01")
names(list_dt) <- name_col

#cut dataframes by classifying which measurement belongs to which interval
df_dt1 <- data.frame(df_all, "dt_1" = cut(df_all$time, breaks = list_dt$dt_1, right = F, include.lowest = TRUE ))
df_dt0.1 <- data.frame(df_all, "dt_0.1" = cut(df_all$time, breaks = list_dt$dt_0.1, right = F, include.lowest = TRUE ))
df_dt0.01 <- data.frame(df_all, "dt_0.01" = cut(df_all$time, breaks = list_dt$dt_0.01, right = F, include.lowest = TRUE ))
head(df_dt_1)
head(df_dt_0.1)
head(df_dt_0.01)

#aggregate the intervals by using the mean of all the measurements in one interval
df_agg_dt1 <- aggregate(cbind(time, Xmut.x, Ymut.x, Zmut.x,Xms2.x, Yms2.x, Zms2.x,Xhpa, Xrads, Yrads, Zrads,Xms2.y, Yms2.y, Zms2.y, latitude, longitude, height, velocity, direction, hor_acc, ver_acc, Xmut.y, Ymut.y, Zmut.y)~dt_1, df_dt1, mean)
df_agg_dt0.1 <- aggregate(cbind(time, Xmut.x, Ymut.x, Zmut.x,Xms2.x, Yms2.x, Zms2.x,Xhpa, Xrads, Yrads, Zrads,Xms2.y, Yms2.y, Zms2.y, latitude, longitude, height, velocity, direction, hor_acc, ver_acc, Xmut.y, Ymut.y, Zmut.y)~dt_0.1, df_dt0.1, mean)
df_agg_dt0.01 <- aggregate(cbind(time, Xmut.x, Ymut.x, Zmut.x,Xms2.x, Yms2.x, Zms2.x,Xhpa, Xrads, Yrads, Zrads,Xms2.y, Yms2.y, Zms2.y, latitude, longitude, height, velocity, direction, hor_acc, ver_acc, Xmut.y, Ymut.y, Zmut.y)~dt_0.01, df_dt0.01, mean)














plot_acc_1 <- ggplot(date = df_agg_dt1, mapping = aes(x=time)) + geom_line(aes(y= "Xrads"), color="darkblue")
plot_acc_1 <- ggplot(df_agg_dt1, aes('time', 'Xrads')) + geom_point(color="darkblue")
plot_acc_1  


#plot accelereometer attributes for each delta t 
plot_acc1 <- ggplot(data=df_agg_dt1, mapping = aes(x=time)) + 
  geom_line(aes(y=Xms2.x), color="darkblue") +
  geom_line(aes(y=Yms2.x), color="pink") +
  geom_line(aes(y=Zms2.x), color="lightblue") 

plot_acc0.1 <- ggplot(data=df_agg_dt0.1, mapping = aes(x=time)) + 
  geom_line(aes(y=Xms2.x), color="darkblue") +
  geom_line(aes(y=Yms2.x), color="pink") +
  geom_line(aes(y=Zms2.x), color="lightblue") 


plot_acc0.01 <- ggplot(data=df_agg_dt0.01, mapping = aes(x=time)) + 
  geom_line(aes(y=Xms2.x), color="darkblue") + 
  geom_line(aes(y=Yms2.x), color="pink") +
  geom_line(aes(y=Zms2.x), color="lightblue") 


plot_acc1
plot_acc0.1
plot_acc0.01






  
  