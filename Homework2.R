install.packages("DDoutlier")
install.packages("dplR")
install.packages("devtools")
install_github("vqv/ggbiplot")
install.packages("factoextra")
install.packages("psych")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("DMwR")
install.packages("lattice")
install.packages("grid")


library(psych)
library(factoextra)
library(ggplot2)
library(plyr)
library(scales)
library(grid)
library(devtools)
library(ggbiplot)
library(DDoutlier)
library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(dplR)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(DMwR)
library(lattice)
library(grid)


#loading experiment measuremnts 
accelerometer2 <- Accelerometer2
barometer2 <- Barometer2
gyroscope2 <- Gyroscope2
linear_accelerometer2 <- Linear_Accelerometer2
location2 <- Location2
magnetometer2 <- Magnetometer2

#renaming column names to handle data better 
accelerometer2<- plyr::rename(accelerometer2, c("Time (s)"="time", "X (m/s^2)"="Xms2", "Y (m/s^2)"="Yms2", "Z (m/s^2)"="Zms2"))
barometer2 <- plyr::rename(barometer2, c("Time (s)"="time", "X (hPa)"="Xhpa"))
gyroscope2 <- plyr::rename(gyroscope2, c("Time (s)"="time", "X (rad/s)"="Xrads", "Y (rad/s)"="Yrads", "Z (rad/s)"="Zrads"))
linear_accelerometer2 <- plyr::rename(linear_accelerometer2, c("Time (s)"="time", "X (m/s^2)"="Xms2", "Y (m/s^2)"="Yms2", "Z (m/s^2)"="Zms2"))
location2 <- plyr::rename(location2, c("Time (s)"="time", "Latitude (°)"="latitude", "Longitude (°)"="longitude", "Height (m)"="height",  "Velocity (m/s)"="velocity", "Direction (°)"="direction", "Horizontal Accuracy (m)"="hor_acc","Vertical Accuracy (°)"="ver_acc"))
magnetometer2 <- plyr::rename(magnetometer2, c("Time (s)"="time", "X (µT)"="Xmut", "Y (µT)"="Ymut", "Z (µT)"="Zmut"))

#looking at accelerometer
plot(accelerometer2$time, accelerometer2$Xms2, pch="*")
plot(accelerometer2$time, accelerometer2$Yms2, pch="*")
plot(accelerometer2$time, accelerometer2$Zms2, pch="*")

#looking at gyroscope
plot(gyroscope2$time, gyroscope2$Xrads, pch="*")
plot(gyroscope2$time, gyroscope2$Yrads, pch="*")
plot(gyroscope2$time, gyroscope2$Zrads, pch="*")

#looking at location
plot(location2$time, location2$latitude, pch="*")
plot(location2$time, location2$longitude, pch="*")
plot(location2$time, location2$height, pch="*")
plot(location2$time, location2$velocity, pch="*")
plot(location2$time, location2$direction, pch="*")
plot(location2$time, location2$hor_acc, pch="*") 
plot(location2$time, location2$ver_acc, pch="*")

#looking at magnetometer
plot(magnetometer2$time, magnetometer2$Xmut, pch="*")
plot(magnetometer2$time, magnetometer2$Ymut, pch="*")
plot(magnetometer2$time, magnetometer2$Zmut, pch="*")

#looking at linear accelerometer
plot(linear_accelerometer2$time, linear_accelerometer2$Xms2, pch="*")
plot(linear_accelerometer2$time, linear_accelerometer2$Yms2, pch="*")
plot(linear_accelerometer2$time, linear_accelerometer2$Zms2, pch="*") 


#####TAKING  LINEAR ACCELEROMETER AS EXAMPLE DATA######


#creating datasets just including walking/doing nothing
lin_acc_walking=dplyr::filter(linear_accelerometer2, time > 15)
lin_acc_nothing=dplyr::filter(linear_accelerometer2, time < 15)

#plot walking activity
plot(lin_acc_walking$time, lin_acc_walking$Xms2, pch="*")
plot(lin_acc_walking$time, lin_acc_walking$Yms2, pch="*")
plot(lin_acc_walking$time, lin_acc_walking$Zms2, pch="*") 

#plot doing nothing
plot(lin_acc_nothing$time, lin_acc_nothing$Xms2, pch="*")
plot(lin_acc_nothing$time, lin_acc_nothing$Yms2, pch="*")
plot(lin_acc_nothing$time, lin_acc_nothing$Zms2, pch="*") 


############################################################################
##########################-Chauvenets-Criterion-############################
############################################################################

Chauvenet <- function(datapoints, loop=FALSE){
  numdatapoints <- nrow(datapoints)
  # calculate normalized distance to mean
  dist <- abs(datapoints - colMeans(datapoints))/sapply(datapoints,sd)
  # calculate probability to have seen such a point assuming a normal
  # distribution
  prob <- apply(dist,c(1,2),function(x) numdatapoints*dnorm(x))
  # select only those points which have a probability >= 0.5
  sel <-  apply(prob,c(1,2),function(x) x>=0.5)
  idx <- rowSums(sel) == ncol(datapoints)
  datapoints <- datapoints[idx,]
  
  if(loop == TRUE){
    while(FALSE %in% idx){
      numdatapoints <- nrow(datapoints)
      dist <- abs(datapoints - colMeans(datapoints))/sapply(datapoints,sd)
      prob <- apply(dist,c(1,2),function(x) numdatapoints*dnorm(x))
      sel <-  apply(prob,c(1,2),function(x) x>=0.5)
      idx <- rowSums(sel) == ncol(datapoints)
      datapoints <- datapoints[idx,]
    }
  }
  
  return(datapoints)
}

lin_acc_cc <- linear_accelerometer2

#checking if data is normal distributed (isn't really)
ggdensity(lin_acc_cc$Xms2, fill = "lightgray")
ggqqplot(lin_acc_cc$Xms2)
ggdensity(lin_acc_cc$Yms2, fill = "lightgray")
ggqqplot(lin_acc_cc$Yms2)
ggdensity(lin_acc_cc$Zms2, fill = "lightgray")
ggqqplot(lin_acc_cc$Zms2)

#performing Chauvenets Criterion on all the variables 
lin_acc_cc_X <- dplyr::select(lin_acc_cc, time, Xms2)
lin_acc_cc_X2 <- Chauvenet(lin_acc_cc_X, loop = FALSE)
lin_acc_cc_X2

lin_acc_cc_Y <- dplyr::select(lin_acc_cc, time, Yms2)
lin_acc_cc_Y2 <- Chauvenet(lin_acc_cc_Y, loop = FALSE)
lin_acc_cc_Y2

lin_acc_cc_Z <- dplyr::select(lin_acc_cc, time, Zms2)
lin_acc_cc_Z2 <- Chauvenet(lin_acc_cc_Z, loop = FALSE)
lin_acc_cc_Z2

#just due to curiosity a boxplot to see if boxplot shows outliers
boxplot(Xms2 ~ cut(time, pretty(lin_acc_cc_X$time)), data=lin_acc_cc_X)
boxplot(Xms2 ~ cut(time, pretty(lin_acc_cc_X2$time)), data=lin_acc_cc_X2)

#plot comparison between data with and without outliers
plot(lin_acc_cc_X$time, lin_acc_cc_X$Xms2, pch="*", main="With Outliers", xlab="time", ylab="Xms2") 
plot(lin_acc_cc_X2$time, lin_acc_cc_X2$Xms2, pch="*", main="Without Outliers", xlab="time", ylab="Xms2") 

plot(lin_acc_cc_Y$time, lin_acc_cc_Y$Yms2, pch="*", main="Witht Outliers", xlab="time", ylab="Yms2") 
plot(lin_acc_cc_Y2$time, lin_acc_cc_Y2$Yms2, pch="*", main="Without Outliers", xlab="time", ylab="Yms2") 

plot(lin_acc_cc_Z$time, lin_acc_cc_Z$Zms2, pch="*", main="With Outliers", xlab="time", ylab="Zms2") 
plot(lin_acc_cc_Z2$time, lin_acc_cc_Z2$Zms2, pch="*", main="Without Outliers", xlab="time", ylab="Zms2") 

#join data with the original time data to see which values are now missing
lin_acc_cc_clean <- full_join(lin_acc_time, lin_acc_cc_Z2, by = "time")
sum(is.na(lin_acc_cc_clean$Zms2))

#fill NA with last known values
lin_acc_cc_clean2 <- na.locf(lin_acc_cc_clean, na.rm=FALSE)
lin_acc_cc_clean2 <- na.locf(lin_acc_cc_clean2, na.rm=FALSE, fromLast=TRUE)

#no missing values anymore 
sum(is.na(lin_acc_cc_clean2$Zms2))

#plot with the imputed missing values and without the outliers 
plot(lin_acc_cc_clean2$time, lin_acc_cc_clean2$Zms2, pch="*", main="Imputed Missing Values", xlab="time", ylab="Zms2") 




############################################################################
#######################-Distance-Based-Approach-############################
############################################################################

lin_acc_dba <- linear_accelerometer2


#plot with outliers
plot(lin_acc_dba$time, lin_acc_dba$Xms2, pch="*") 
# Classify observations
lin_acc_dba_X <- dplyr::select(lin_acc_dba, time, Xms2)
lin_acc_dba_X_class <- DB(dataset=lin_acc_dba_X, d=14, fraction=0.5)$classification
table(lin_acc_dba_X_class)
# Remove outliers from dataset
lin_acc_dba_X_clean <- lin_acc_dba_X[lin_acc_dba_X_class=='Inlier',]
#plot without classified outliers 
plot(lin_acc_dba_X_clean$time, lin_acc_dba_X_clean$Xms2, pch="*") 

#perform same with Yms2
plot(lin_acc_dba$time, lin_acc_dba$Yms2, pch="*") 
lin_acc_dba_Y <- dplyr::select(lin_acc_dba, time, Yms2)
lin_acc_dba_Y_class <- DB(dataset=lin_acc_dba_Y, d=13, fraction=0.5)$classification
table(lin_acc_dba_Y_class)
lin_acc_dba_Y_clean <- lin_acc_dba_Y[lin_acc_dba_Y_class=='Inlier',]
plot(lin_acc_dba_Y_clean$time, lin_acc_dba_Y_clean$Yms2, pch="*") 

#perform same with Zms2
plot(lin_acc_dba$time, lin_acc_dba$Zms2, pch="*") 
lin_acc_dba_Z <- dplyr::select(lin_acc_dba, time, Zms2)
lin_acc_dba_Z_class <- DB(dataset=lin_acc_dba_Z, d=15, fraction=0.5)$classification
table(lin_acc_dba_Z_class)
lin_acc_dba_Z_clean <- lin_acc_dba_Z[lin_acc_dba_Z_class=='Inlier',]
plot(lin_acc_dba_Z_clean$time, lin_acc_dba_Z_clean$Zms2, pch="*", main="Without Outliers", xlab="time", ylab="Zms2") 


#join the datasets
lin_acc_dba_clean <- full_join(lin_acc_dba_X_clean, lin_acc_dba_Y_clean, by = "time")
lin_acc_dba_clean <- full_join(lin_acc_dba_clean, lin_acc_dba_Z_clean, by = "time")
#join with original time data
lin_acc_time <- select(linear_accelerometer2, time)
lin_acc_dba_clean2 <- full_join(lin_acc_time, lin_acc_dba_clean, by = "time")

#check missing values
sum(is.na(lin_acc_dba_clean2$Xms2))
sum(is.na(lin_acc_dba_clean2$Yms2))
sum(is.na(lin_acc_dba_clean2$Zms2))

#fill NA with last known values
lin_acc_dba_clean3 <- na.locf(lin_acc_dba_clean2, na.rm=FALSE)
lin_acc_dba_clean3 <- na.locf(lin_acc_dba_clean3, na.rm=FALSE, fromLast=TRUE)

#all missing values imputed
sum(is.na(lin_acc_dba_clean3$Xms2))
sum(is.na(lin_acc_dba_clean3$Yms2))
sum(is.na(lin_acc_dba_clean3$Zms2))

#compare plot of data with outliers to data without outlierd and imputed missing values
plot(linear_accelerometer2$time, linear_accelerometer2$Xms2, pch="*", main="With Outliers", xlab="time", ylab="Xms2")
plot(lin_acc_dba_clean3$time, lin_acc_dba_clean3$Xms2, pch="*", main="Imputed Missing Values", xlab="time", ylab="Xms2") 
plot(linear_accelerometer2$time, linear_accelerometer2$Yms2, pch="*", main="With Outliers", xlab="time", ylab="Yms2")
plot(lin_acc_dba_clean3$time, lin_acc_dba_clean3$Yms2, pch="*", main="Imputed Missing Values", xlab="time", ylab="Yms2") 
plot(linear_accelerometer2$time, linear_accelerometer2$Zms2, pch="*", main="With Outliers", xlab="time", ylab="Zms2")
plot(lin_acc_dba_clean3$time, lin_acc_dba_clean3$Zms2, pch="*", main="Imputed Missing Values", xlab="time", ylab="Zms2") 


############################################################################
############################-Local-Outlier-Factor-##########################
############################################################################

lin_acc_lof <-linear_accelerometer2[,2:4]
lin_acc_lof

#obtain the outlier score with k= 5
outlier.scores <- lofactor(lin_acc_lof, k=5)
#plot density graph
plot(density(outlier.scores))
#top 200 outliers (output are row numbers)
outliers <- order(outlier.scores, decreasing=T)[1:200]
print(outliers)

#look at detected outliers (not much to see)
n <- nrow(lin_acc_lof)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(lin_acc_lof), cex=.5, xlabs=labels)

#plot putliers
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(lin_acc_lof, pch=pch, col=col)

lin_acc_lof2 <- linear_accelerometer2

#exclude rows which are labled as outliers
lin_acc_lof_clean <- lin_acc_lof2[-c(outliers), ]


#plot data with and without outliers 
plot(lin_acc_lof2$time, lin_acc_lof2$Xms2, pch="*")
plot(lin_acc_lof_clean$time, lin_acc_lof_clean$Xms2, pch="*") 

plot(lin_acc_lof2$time, lin_acc_lof2$Yms2, pch="*")
plot(lin_acc_lof_clean$time, lin_acc_lof_clean$Yms2, pch="*") 

plot(lin_acc_lof2$time, lin_acc_lof2$Yms2, pch="*")
plot(lin_acc_lof_clean$time, lin_acc_lof_clean$Yms2, pch="*") 


#join with time from original data
lin_acc_lof_clean2 <- full_join(lin_acc_time, lin_acc_lof_clean, by = "time")

#fill NA with last known values
lin_acc_lof_clean3 <- na.locf(lin_acc_lof_clean2, na.rm=FALSE)
lin_acc_lof_clean3 <- na.locf(lin_acc_lof_clean3, na.rm=FALSE, fromLast=TRUE)
lin_acc_lof_clean3 

#plot with, woithout outliers and with imputed missing values 
plot(lin_acc_lof2$time, lin_acc_lof2$Xms2, pch="*", main="With Outliers", xlab="time", ylab="Xms2")
plot(lin_acc_lof_clean$time, lin_acc_lof_clean$Xms2, pch="*", main="Without Outliers", xlab="time", ylab="Xms2") 
plot(lin_acc_lof_clean3$time, lin_acc_lof_clean3$Xms2, pch="*", main="Imputed Missing Values", xlab="time", ylab="Xms2") 

plot(lin_acc_lof2$time, lin_acc_lof2$Yms2, pch="*", main="With Outliers", xlab="time", ylab="Yms2")
plot(lin_acc_lof_clean$time, lin_acc_lof_clean$Yms2, pch="*", main="Without Outliers", xlab="time", ylab="Yms2") 
plot(lin_acc_lof_clean3$time, lin_acc_lof_clean3$Yms2, pch="*", main="Imputed Missing Values", xlab="time", ylab="Yms2") 

plot(lin_acc_lof2$time, lin_acc_lof2$Zms2, pch="*", main="With Outliers", xlab="time", ylab="Zms2")
plot(lin_acc_lof_clean$time, lin_acc_lof_clean$Zms2, pch="*", main="Without Outliers", xlab="time", ylab="Zms2") 
plot(lin_acc_lof_clean3$time, lin_acc_lof_clean3$Zms2, pch="*", main="Imputed Missing Values", xlab="time", ylab="Zms2") 


############################################################################
############################-Lowpass-Filter-################################
############################################################################

Z <- lin_acc_walking$Zms2

#using butterworth low pass filter, with period of 0.5
bSm <- pass.filt(Z, W=0.05, type="low", method="Butterworth")
plot(Z, type="l", col="black")
lines(bSm, col="red")

#add the new values 
lin_acc_walking_low <- lin_acc_walking
lin_acc_walking_low$Z_low <- c(bSm)
lin_acc_walking_low

#plot the measurements with and without the missing values 
plot(lin_acc_walking$time, lin_acc_walking$Zms2, pch="*",  main="With Outliers", xlab="time", ylab="Zms2") 
plot(lin_acc_walking_low$time, lin_acc_walking_low$Z_low, pch="*", main="Without Outliers", xlab="time", ylab="Zms2") 


############################################################################
#######################-Principal-Component-Analysis-#######################
############################################################################

lin_acc_pca <- linear_accelerometer2

#obtain 3 principal components, each explains percentage of the total variation 
lin_acc_pca2 <- prcomp(lin_acc_pca[,c(2:4)], center = TRUE,scale. = TRUE)
summary(lin_acc_pca2)

#plotting just one second to see the pints
lin_acc_walk_pca=dplyr::filter(lin_acc_walking, time < 16)
lin_acc_walk_pca <- plyr::rename(lin_acc_walk_pca, c("Xms2"="Xwalk", "Yms2"="Ywalk", "Zms2"="Zwalk"))
lin_acc_walk_pca2 <- prcomp(lin_acc_walk_pca[,c(2:4)], center = TRUE,scale. = TRUE)
summary(lin_acc_walk_pca2)
ggbiplot(lin_acc_walk_pca2, labels=rownames(lin_acc_walk_pca))

#percentage of variances by each principal component
lin_acc_pca2 <- prcomp(lin_acc_pca[,c(2:4)], scale. = TRUE)
fviz_eig(lin_acc_pca2)
psych::fa.parallel(lin_acc_pca, fa="pc")






















