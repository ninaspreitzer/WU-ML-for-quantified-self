library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(cluster)
library(scatterplot3d)
library(car)
library(plotly)

accelerometer2 <- Accelerometer2
accelerometer3 <- Raw_Data
accelerometer2
accelerometer2<- plyr::rename(accelerometer2, c("time"="time", "X (m/s^2)"="Xms2", "Y (m/s^2)"="Yms2", "Z (m/s^2)"="Zms2"))
accelerometer3 <- select(accelerometer3, time, Xms2, Yms2, Zms2)
accelerometer3

with(accelerometer3, {
  scatterplot3d(Xms2, Yms2, Zms2)
})


acc_walking=dplyr::filter(accelerometer2, time > 15)
acc_nothing=dplyr::filter(accelerometer2, time < 15)

acc_zpos <- filter(accelerometer2, Zms2 > 5.5)
acc_zneg <- filter(accelerometer2, Zms2 < 5.5)

acc_walkingZ=dplyr::filter(acc_zpos, time > 15)
acc_nothingZ=dplyr::filter(acc_zpos, time < 15)

acc_zpos$label <- c(1)
acc_zneg$label <- c(2)

acc_walking$label <- c("walking")
acc_nothing$label <- c("doingNothing")
acc_walking
acc_nothing

acc_walkingZ$label <- c("walking")
acc_nothingZ$label <- c("doingNothing")

acc <- bind_rows(acc_nothing, acc_walking)
acc2 <- bind_rows(acc_zpos, acc_zneg)
acc3 <- bind_rows(acc_nothingZ, acc_walkingZ)
acc2

### k-means clustering

# check silhouette scores for k-means with different k
kValues = 2:9
silhouetteValues = c()

tmpDat = acc[,c('Xms2', 'Yms2', 'Zms2')]
for (k in kValues) {
  cat("k=", k,"\n")
  resKM = kmeans(tmpDat, centers = k, 
                 iter.max = 50, nstart = 50)
  dai = daisy(tmpDat)
  sil = silhouette(resKM$cluster,dai) 
  silhouetteValues[k-kValues[1]+1] = summary(sil)$avg.width
}
plot(kValues,silhouetteValues,xlab="k",ylab="Silhouette Score  (K-Means)",ylim = c(0,1),type="l",col="blue")
grid()

# run k-means with the highest silhouette score
k = 2
resKM = kmeans(tmpDat, centers = k, 
               iter.max = 50, nstart = 50)
sil = silhouette(resKM$cluster,dai) 
factoextra::fviz_silhouette(sil)

# plot labels and clusters in 3D
fig <- plot_ly(acc, x = ~Xms2, y = ~Yms2, z = ~Zms2, color= rainbow(k)[resKM$cluster], size=1)
fig <- plot_ly(acc, x = ~Xms2, y = ~Yms2, z = ~Zms2, color=acc$label, size=1)
fig





plot(accelerometer2$time, accelerometer2$Xms2, pch="*")
plot(accelerometer2$time, accelerometer2$Yms2, pch="*")
plot(accelerometer2$time, accelerometer2$Zms2, pch=resKM$cluster)







# clustering using k-medoids
tmpDat = accelerometer2[,c('Xms2', 'Yms2', 'Zms2')]
kValues = 2:9
silhouetteValues = c()

for (k in kValues) {
  cat("k=", k,"\n")
  resKMed = pam(tmpDat, k)
  dai = daisy(tmpDat)
  sil = silhouette(resKMed$cluster,dai) 
  silhouetteValues[k-kValues[1]+1] = summary(sil)$avg.width
}
plot(kValues,silhouetteValues,xlab="k",ylab="Silhouette Score (K-Medoids)",ylim = c(0,1),type="l",col="blue")
grid()

# run k-medoids with the highest silhouette score
k = 2
resKMed = pam(tmpDat, k)
sil = silhouette(resKMed$cluster,dai) 
factoextra::fviz_silhouette(sil)

# demonstrate that the k-means and k-medoids lead to very similar clusters
table(resKM$cluster,resKMed$clustering) # cluster numbers are different, but otherwise no big difference


### hierarchical clustering (agglomerative) 
cluster = hclust(dist(tmpDat[,-3]),method = "ward.D")
kMax = 100
memb = cutree(cluster, k = kMax)
sil = silhouette(memb,dai) 
factoextra::fviz_silhouette(sil)

cent = NULL
for(k in 1:kMax){
  cent <- rbind(cent, colMeans(tmpDat[,-3][memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "average", members = table(memb))
plot(hc1)











