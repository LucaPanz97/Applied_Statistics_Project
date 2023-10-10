rm(list=ls())
ls()

setwd("/Users/davidecaffu/Desktop/ProgettoAS")
maindir <- getwd()

#______________________________________________________________________________

# Perform PCA on data related to mean avg_speed of the 6 types of mean_speeds
# for the first considered day (24/10/2018)
# The types of mean_speeds are: Bus, Car, Heavy mean_speeds, 
# Medium mean_speeds, Motorcycles and Taxi.

# Generate vectors where to store data
zone <- NULL
time.slot <- NULL
bus <- NULL
car <- NULL
hv <- NULL
mv <- NULL
moto <- NULL
taxi <- NULL

# File organization: dataset are grouped by time slot in folders named with
# numbers from 1 to 5, indicating the 5 different time slots:
# 1 -> 8:30-9:00
# 2 -> 9:00-9:30
# 3 -> 9:30-10:00
# 4 -> 10:00-10:30
# 5 -> 10:30-11:00

# Open all the dataset and save the data about mean avg_speed of vehicle types
for (folder in seq(1,10)){
  path <- file.path(maindir, folder)
  setwd(file.path(maindir, folder))
  files <- list.files(path)
  time <- folder
  for (i in seq(1,10)){
    print(paste0('reading dataset number ', i,' of time slot ', folder))
    d <- read.csv(pipe(paste0("cut -f1,2,3,4 -d';' ", files[i])), sep = ';')
    zone <- c(zone, i)
    time.slot <- c(time.slot, time)
    mean_speed <- NULL
    names <- c(' Bus', ' Car', ' Heavy Vehicle', ' Medium Vehicle', ' Motorcycle', ' Taxi')
    for (j in seq(1,length(names))){
      mean_speed <- c(mean_speed, mean(d[which(d$type==names[j]),4]))
    }
    bus <- c(bus, mean_speed[1])
    car <- c(car, mean_speed[2])
    hv <- c(hv, mean_speed[3])
    mv <- c(mv, mean_speed[4])
    moto <- c(moto, mean_speed[5])
    taxi <- c(taxi, mean_speed[6])
  }
}
setwd("/Users/davidecaffu/Desktop/ProgettoAS")

mean_speeds <- data.frame(Zone=zone, Time.slot=time.slot, Bus=bus, Car=car, 
                       Heavy.mean_speeds=hv, Medium.mean_speeds=mv, Motorcycles=moto, Taxi=taxi)

#write.csv(mean_speeds, file='20181024_mean_avg_speeds.csv', row.names=FALSE)
mean_speeds <- read.csv('20181024_mean_avg_speeds.csv', header=T)

head(mean_speeds)
dim(mean_speeds)

mean_speeds.label <- mean_speeds[,1:2]
mean_speeds <- mean_speeds[,-(1:2)]

n <- dim(mean_speeds)[1]
p <- dim(mean_speeds)[2]

# Boxplot
x11()
#jpeg(file = "data_boxplot.jpeg")
par(mar=rep(8,4))
boxplot(mean_speeds, las=2, col='gold')
#dev.off()

# Boxplot of scaled data
x11()
#jpeg(file = "scaled_data_boxplot.jpeg")
par(mar=rep(8,4))
boxplot(scale(x=mean_speeds,center = T, scale=F), las=2, col='gold') 
#dev.off()

# Perform the Principal Components Analysis on original data
pc.mean_speeds <- princomp(mean_speeds, scores=T)
pc.mean_speeds
summary(pc.mean_speeds)

# Standard deviation of the components
pc.mean_speeds$sd
# proportion of variance explained by each PC
pc.mean_speeds$sd^2/sum(pc.mean_speeds$sd^2)
# cumulative proportion of explained variance
cumsum(pc.mean_speeds$sd^2)/sum(pc.mean_speeds$sd^2)

# loadings
load.mean_speeds <- pc.mean_speeds$loadings
load.mean_speeds

# graphical representation of the loadings of the first six principal components
x11()
par(mfcol = c(3,2))
for(i in 1:8) barplot(load.mean_speeds[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
#jpeg(file = "data_loadings.jpeg")
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.mean_speeds[,i], ylim = c(-1, 1))
#dev.off()

# loadings interpretation:
# The first PC is a weighted sum of all the vehicles with almost the same weights.
# The second PC is a contrast of buses and heavy vehicles against cars, motorcycles and taxi.
# The third PC is a contrast of buses and motorcycles against cars, heavy 
# and medium vehicles and taxis
x11()
#jpeg(file = "data_explained_var.jpeg")
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.mean_speeds, las=2, main='Principal components', ylim=c(0,2e5))
barplot(sapply(mean_speeds,sd)^2, las=2, main='Original Variables', ylim=c(0,2e5), ylab='Variances')
plot(cumsum(pc.mean_speeds$sd^2)/sum(pc.mean_speeds$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')  #threshold
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(mean_speeds),labels=1:ncol(mean_speeds),las=2)
#dev.off()

# Principal Components Analysis on standardized variables

mean_speeds.sd <- scale(mean_speeds)
mean_speeds.sd <- data.frame(mean_speeds.sd)

head(mean_speeds.sd)

# Boxplot
x11()
#jpeg(file='sd_data_boxplot.jpeg')
par(mar=rep(8,4))
boxplot(mean_speeds.sd, las=2, col='gold')
#dev.off()

pc.mean_speeds <- princomp(mean_speeds.sd, scores=T)
pc.mean_speeds
summary(pc.mean_speeds)

pc.mean_speeds$sd
pc.mean_speeds$sd^2/sum(pc.mean_speeds$sd^2)
cumsum(pc.mean_speeds$sd^2)/sum(pc.mean_speeds$sd^2)

# explained variance
x11()
#jpeg(file='sd_data_explained_var.jpeg')
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.mean_speeds, las=2, main='Principal components', ylim=c(0,5))
barplot(sapply(mean_speeds.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,5), ylab='Variances')
plot(cumsum(pc.mean_speeds$sd^2)/sum(pc.mean_speeds$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')  #threshold
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(mean_speeds.sd),labels=1:ncol(mean_speeds.sd),las=2)
#dev.off()

# loadings
load.mean_speeds <- pc.mean_speeds$loadings
load.mean_speeds

# Loadings interpretation:
# First PC: this time it is a weighted sum of all the categories of vehicle
# with similar loadings
# Second PC: contrast between buses and heavy vehicles with all the others

# graphical representation
x11()
#jpeg(file='sd_data_loadings.jpeg')
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.mean_speeds[,i], ylim = c(-1, 1))
#dev.off()

# scores (projection of each statistical unit on each PC)
scores.mean_speeds <- pc.mean_speeds$scores
scores.mean_speeds

x11()
#jpeg(file='sd_scores.jpeg')
plot(scores.mean_speeds[,1:2])
abline(h=0, v=0, lty=2, col='grey')
#dev.off()
# High PC1: general high speed of vehicles
# Low PC1: general low speed of vehicles
# High PC2: high speed for buses and heavy vehicles
# Low PC2: high speed for cars, medium vehicles, motorcycles, taxis
# Let's use the categorical variables to further interpret the results
head(mean_speeds.label)

# Color according to Zone
mean_speeds.label[,1]
mean_speeds.label[,1] <- factor(mean_speeds.label[,1], levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
col.ramp <- rainbow(10)
col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] = col.ramp[which(mean_speeds.label[i,1] == levels(mean_speeds.label[,1]))]
# above we have a vector containing the assigned color for each unit depending on its zone

x11()
#jpeg(file='color_by_zone.jpeg')
plot(scores.mean_speeds[,1:2], col=col.lab1, pch=19, xlim=c(-8,5), ylim=c(-5,3))
abline(h=-4, v=-7, col=1)
points(scores.mean_speeds[,1], rep(-4, n), col=col.lab1, pch=19)
points(rep(-7, n),scores.mean_speeds[,2], col=col.lab1, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(mean_speeds.label[,1]),fill=rainbow(10),bty='n')
#dev.off()

# Color according to Time Slots
mean_speeds.label[,2]
col.ramp <- rainbow(5)
col.lab2 <- rep(NA, n)
for(i in 1:n)
  col.lab2[i] = col.ramp[which(mean_speeds.label[i,2] == levels(factor(mean_speeds.label[,2])))]

x11()
#jpeg(file='color_by_time.jpeg')
plot(scores.mean_speeds[,1:2], col=col.lab2, pch=19, xlim=c(-9,7), ylim=c(-4,3))
abline(h=-3.5, v=-8, col=1)
points(scores.mean_speeds[,1], rep(-3.5, n), col=col.lab2, pch=19)
points(rep(-8, n),scores.mean_speeds[,2], col=col.lab2, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',c('8:30-9:00','9:00-9:30','9.30-10:00','10:00-10:30','10:30-11:00'),fill=rainbow(5),bty='n')
#dev.off()


