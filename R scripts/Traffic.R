rm(list=ls())
ls()

setwd("/Users/davidecaffu/Desktop/ProgettoAS/1")

# Variables: track_id; type; traveled_d; avg_speed; lat; lon; speed; lon_acc; lat_acc; time

max_cols <- max(count.fields('20181024_d1_0830_0900.csv', sep = ';'))
traffic <- read.table('20181024_d1_0830_0900.csv', col.names = paste0("V",seq_len(max_cols)),
                      skip=1, fill=TRUE, sep=';')


head(traffic)
dim(traffic)

# Considerations about vehicle types

# Frequencies
type_abs_freq <- table(traffic[,2])  # absolute frequencies
type_abs_freq

type_rel_freq <- table(traffic[,2])/length(traffic[,2])
type_rel_freq

attach(traffic)

# Traveled distance
x11()
hist(V3, main ='Traveled distance', prob=T)
boxplot(V3 ~ V2, data=traffic, cex.axis = 0.7, col='gold', xlab='Vehicle type', ylab='Traveled distance')

# Average speed
x11()
boxplot(V4 ~ V2, data=traffic, cex.axis = 0.7, col='gold', xlab='Vehicle type', ylab='Average speed')
hist(V4, main='Average speed', prob=T)
# Compare with gaussian distribution
lines(seq(0,60,by=0.01), dnorm(seq(0,60,by=0.01), mean(V4), sd(V4)))

# Average speed by classes
# Car
cars <- subset(traffic, V2==" Car")
x11()
hist(cars$V4, main='Average speed', prob=T)
lines(seq(0,60,by=0.01), dnorm(seq(0,60,by=0.01), mean(cars$V4), sd(cars$V4)))

# Taxi
taxis <- subset(traffic, V2==" Taxi")
x11()
hist(taxis$V4, main='Average speed', prob=T)
lines(seq(0,60,by=0.01), dnorm(seq(0,60,by=0.01), mean(taxis$V4), sd(taxis$V4)))

graphics.off()

# Check if we can find some clustering based on distance-avg.speed relation
x11()
plot(V3,V4, xlab='Traveled distance', ylab='Average speed', col=rainbow(6), pch=19)
legend(x='topleft', legend=unique(V2), pch=19, col=rainbow(6), cex=0.8)

oneline_traj <- function(total_line){
  id_variables <- total_line[1:4]
  names <- c('track_id', 'type', 'traveled_d', 'avg_speed', 'lat', 'lon', 'speed', 'lon_acc', 'lat_acc', 'time')
  df <- data.frame(colnames(names))
  for (i in seq(5, length(total_line), by=6)){
    if (!is.na(total_line[i])){
      others = {}
      
      for (j in seq(i,i+5,by=1)){
        others = append(others,total_line[j])}
      
      local_line <- append(id_variables, others)
      
      mat <- matrix(unlist(local_line), nrow=1)
      new <- data.frame(mat)
      df <- rbind(df, new)}
    else{
      colnames(df) <- names
      df$track_id <- as.integer(df$track_id)
      for (i in seq(3,10)){
        df[,i] <- as.numeric(df[,i])
      }
      return(df)}
  }
}

plot_traj <- function(data, n){
  x11()
  for (i in seq(1,n)){
    line <- oneline_traj(data[i,])
    if (i == 1) plot(line[,5], line[,6])
    else lines(line[,5], line[,6])
  }
}

traj <- oneline_traj(traffic[,])
traj
coords <- data.frame(x = traj$lat, y= traj$lon, time=traj$time)
coords
write.csv(coords, file='tra.csv')
plot_traj(traffic)

save(oneline_traj, generate_traj, file = 'gen_traj.RData')


