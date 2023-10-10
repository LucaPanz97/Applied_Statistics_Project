rm(list=ls())
ls()

setwd("/Users/davidecaffu/Desktop/ProgettoAS")
maindir <- getwd()

#______________________________________________________________________________

# Perform PCA on data related to absolute frequencies of the 6 types of vehicles
# for the first considered day (24/10/2018)
# The types of vehicles are: Bus, Car, Heavy Vehicles, 
# Medium Vehicles, Motorcycles and Taxi.

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

# Open all the dataset and save the data about frequencies of vehicle types
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
    type.freq <- as.numeric(table(d$type))
    bus <- c(bus, type.freq[1])
    car <- c(car, type.freq[2])
    hv <- c(hv, type.freq[3])
    mv <- c(mv, type.freq[4])
    moto <- c(moto, type.freq[5])
    taxi <- c(taxi, type.freq[6])
  }
}
setwd("/Users/davidecaffu/Desktop/ProgettoAS")

# Generate a dataframe of frequencies of vehicle types, 
# with information about zone and time slot of the observation
vehicles <- data.frame(Zone=zone, Time.slot=time.slot, Bus=bus, Car=car, 
                 Heavy.Vehicles=hv, Medium.Vehicles=mv, Motorcycles=moto, Taxi=taxi)

# Save dataframe as csv file
#write.csv(vehicles, file='20181024_vehicles.csv', row.names=FALSE)
vehicles <- read.csv('20181024_vehicles.csv', header=T)

head(vehicles)
dim(vehicles)

vehicles.label <- vehicles[,1:2]
vehicles <- vehicles[,-(1:2)]

n <- dim(vehicles)[1]
p <- dim(vehicles)[2]

# Boxplot
x11()
jpeg(file = "data_boxplot.jpeg")
par(mar=rep(8,4))
boxplot(vehicles, las=2, col='gold', main='Vehicle type frequencies')
dev.off()

# Boxplot of scaled data
x11()
#jpeg(file = "scaled_data_boxplot.jpeg")
par(mar=rep(8,4))
boxplot(scale(x=vehicles,center = T, scale=F), las=2, col='gold') 
#dev.off()

# Perform the Principal Components Analysis on original data
pc.vehicles <- princomp(vehicles, scores=T)
pc.vehicles
summary(pc.vehicles)

# Standard deviation of the components
pc.vehicles$sd
# proportion of variance explained by each PC
pc.vehicles$sd^2/sum(pc.vehicles$sd^2)
# cumulative proportion of explained variance
cumsum(pc.vehicles$sd^2)/sum(pc.vehicles$sd^2)

# loadings
load.vehicles <- pc.vehicles$loadings
load.vehicles

# graphical representation of the loadings of the first six principal components
x11()
par(mfcol = c(3,2))
for(i in 1:8) barplot(load.vehicles[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
#jpeg(file = "data_loadings.jpeg")
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.vehicles[,i], ylim = c(-1, 1))
#dev.off()

# loadings interpretation:
# The majority of the variability is explained by categories of vehicles
# which are the most present on the road.
# The first PC is a weighted sum of cars, motorcycles and taxi, with a higher
# weight for cars.
# The second PC is a contrast between cars and motorcycles plus taxi.

# explained variance
x11()
#jpeg(file = "data_explained_var.jpeg")
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.vehicles, las=2, main='Principal components', ylim=c(0,2e5))
barplot(sapply(vehicles,sd)^2, las=2, main='Original Variables', ylim=c(0,2e5), 
        ylab='Variances', names=c('Bus','Car','HV', 'MV','Motorcycle','Taxi'))
plot(cumsum(pc.vehicles$sd^2)/sum(pc.vehicles$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')  #threshold
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(vehicles),labels=1:ncol(vehicles),las=2)
#dev.off()

# The first PC explains more than 98% of the total variability. 
# This is due to the masking effect of those 3 variables over the others

# Principal Components Analysis on standardized variables

vehicles.sd <- scale(vehicles)
vehicles.sd <- data.frame(vehicles.sd)

head(vehicles.sd)

# Boxplot
x11()
#jpeg(file='sd_data_boxplot.jpeg')
par(mar=rep(8,4))
boxplot(vehicles.sd, las=2, col='gold')
#dev.off()

pc.vehicles <- princomp(vehicles.sd, scores=T)
pc.vehicles
summary(pc.vehicles)

pc.vehicles$sd
pc.vehicles$sd^2/sum(pc.vehicles$sd^2)
cumsum(pc.vehicles$sd^2)/sum(pc.vehicles$sd^2)

# explained variance
x11()
#jpeg(file='sd_data_explained_var.jpeg')
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.vehicles, las=2, main='Principal components', ylim=c(0,5))
barplot(sapply(vehicles.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,5), ylab='Variances')
plot(cumsum(pc.vehicles$sd^2)/sum(pc.vehicles$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')  #threshold
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(vehicles.sd),labels=1:ncol(vehicles.sd),las=2)
#dev.off()

# loadings
load.vehicles <- pc.vehicles$loadings
load.vehicles

# Loadings interpretation:
# First PC: this time it is a weighted sum of all the categories of vehicle
# with similar loadings, only the one of heavy vehicles is slightly lower
# Second PC: contrast between public transport, taxis and motorcycles 
# with cars and heavy vehicles
# ?????? bus and taxi lanes?

# graphical representation
x11()
#jpeg(file='sd_data_loadings.jpeg')
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.vehicles[,i], ylim = c(-1, 1))
#dev.off()

# scores (projection of each statistical unit on each PC)
scores.vehicles <- pc.vehicles$scores
scores.vehicles

x11()
#jpeg(file='sd_scores.jpeg')
plot(scores.vehicles[,1:2])
abline(h=0, v=0, lty=2, col='grey')
#dev.off()
# High PC1: general high flow of vehicles
# Low PC1: general low flow of vehicles
# High PC2: high flow for public transport, taxis and motorcycles, low for cars and heavy vehicles
# Low PC2: high flow for cars and heavy vehicles, low for public transport, taxis and motorcycles

# Let's use the categorical variables to further interpret the results
head(vehicles.label)

# Color according to Zone
vehicles.label[,1]
vehicles.label[,1] <- factor(vehicles.label[,1], levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
col.ramp <- rainbow(10)
col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] = col.ramp[which(vehicles.label[i,1] == levels(vehicles.label[,1]))]
# above we have a vector containing the assigned color for each unit depending on its zone

x11()
#jpeg(file='color_by_zone.jpeg')
plot(scores.vehicles[,1:2], col=col.lab1, pch=19, xlim=c(-8,5), ylim=c(-5,3), main='Statistical units colored by zone')
abline(h=-4, v=-7, col=1)
points(scores.vehicles[,1], rep(-4, n), col=col.lab1, pch=19)
points(rep(-7, n),scores.vehicles[,2], col=col.lab1, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(vehicles.label[,1]),fill=rainbow(10),bty='n')
#dev.off()

# Color according to Time Slots
vehicles.label[,2]
col.ramp <- rainbow(5)
col.lab2 <- rep(NA, n)
for(i in 1:n)
  col.lab2[i] = col.ramp[which(vehicles.label[i,2] == levels(factor(vehicles.label[,2])))]

x11()
#jpeg(file='color_by_time.jpeg')
plot(scores.vehicles[,1:2], col=col.lab2, pch=19, xlim=c(-9,7), ylim=c(-4,3), main='Statistical units colored by time slots')
abline(h=-3.5, v=-8, col=1)
points(scores.vehicles[,1], rep(-3.5, n), col=col.lab2, pch=19)
points(rep(-8, n),scores.vehicles[,2], col=col.lab2, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',c('8:30-9:00','9:00-9:30','9.30-10:00','10:00-10:30','10:30-11:00'),fill=rainbow(5),bty='n')
#dev.off()
