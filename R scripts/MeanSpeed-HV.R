rm(list=ls())
ls()

setwd("/Users/davidecaffu/Desktop/ProgettoAS")

# Variables: track_id; type; traveled_d; avg_speed; lat; lon; speed; lon_acc; lat_acc; time

# Import data for all the zones, first day, first time interval (8:30-9:00)
d1 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d1_0830_0900.csv"), sep = ';')
d2 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d2_0830_0900.csv"), sep = ';')
d3 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d3_0830_0900.csv"), sep = ';')
d4 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d4_0830_0900.csv"), sep = ';')
d5 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d5_0830_0900.csv"), sep = ';')
d6 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d6_0830_0900.csv"), sep = ';')
d7 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d7_0830_0900.csv"), sep = ';')
d8 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d8_0830_0900.csv"), sep = ';')
d9 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d9_0830_0900.csv"), sep = ';')
d10 <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d10_0830_0900.csv"), sep = ';')

d <- list(NULL)
d[[1]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d1_0830_0900.csv"), sep = ';')
d[[2]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d2_0830_0900.csv"), sep = ';')
d[[3]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d3_0830_0900.csv"), sep = ';')
d[[4]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d4_0830_0900.csv"), sep = ';')
d[[5]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d5_0830_0900.csv"), sep = ';')
d[[6]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d6_0830_0900.csv"), sep = ';')
d[[7]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d7_0830_0900.csv"), sep = ';')
d[[8]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d8_0830_0900.csv"), sep = ';')
d[[9]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d9_0830_0900.csv"), sep = ';')
d[[10]] <- read.csv(pipe("cut -f1,2,3,4 -d';' 20181024_d10_0830_0900.csv"), sep = ';')

# Extract mean avg_speed by zone
mean_avg_speed <- c(NULL)
for (i in seq(1,10)){
  mean_avg_speed <- c(mean_avg_speed, mean(d[[i]]$avg_speed))
}

# Boxplot of avg_speed by zone
x11()
boxplot(d1$avg_speed, d2$avg_speed, d3$avg_speed, d4$avg_speed, d5$avg_speed, 
        d6$avg_speed, d7$avg_speed, d8$avg_speed, d9$avg_speed, d10$avg_speed)

# Generate table of frequencies for the type of vehicle as categorical variable
tab1 <- table(d1$type)
tab2 <- table(d2$type)
tab3 <- table(d3$type)
tab4 <- table(d4$type)
tab5 <- table(d5$type)
tab6 <- table(d6$type)
tab7 <- table(d7$type)
tab8 <- table(d8$type)
tab9 <- table(d9$type)
tab10 <- table(d10$type)

type_tab <- rbind(tab1, tab2, tab3, tab4, tab5, tab6, tab7, tab8, tab9, tab10)
type_df <- as.data.frame(type_tab)
colnames(type_df) <- c('Bus', 'Car', 'Heavy Vehicle', 'Medium Vehicle', 'Motorcycle', 'Taxi')

x11()
plot(type_df$'Heavy Vehicle', mean_avg_speed)

# Linear regression: try to see if there's dependency between the frequencies of vehicle type and mean avg_speed
attach(type_df)
regression <- lm(mean_avg_speed ~ I(`Heavy Vehicle`^-1))

summary(regression)

coef(regression)
vcov(regression) # var cov matrix
residuals(regression)
fitted(regression)

x11()
plot(`Heavy Vehicle`, mean_avg_speed, asp=1,cex=0.75)
abline(coef(regression))
points(`Heavy Vehicle`, fitted(regression), col='red', pch=19)
legend('bottomright',c('Obs.','Fit','Reg. line'),col=c('black','red','black'),lwd=c(1,1,1),lty=c(-1,-1,1),pch=c(c(1,19,-1)))

detach(type_df)
# Plot of the number of heavy vehicles
x11()
barplot(type_df$Bus~seq(1,10), main='Number of buses', xlab='Zone', ylim=c(0,80))
barplot(type_df$'Heavy Vehicle'~seq(1,10), main='Number of heavy vehicles', xlab='Zone', ylim=c(0,50))
barplot(type_df$'Medium Vehicle'~seq(1,10), main='Number of medium vehicles', xlab='Zone', ylim=c(0,120))

x11()
par(mfrow=c(1,2))
boxplot(d1$avg_speed, d2$avg_speed, d3$avg_speed, d4$avg_speed, d5$avg_speed, 
        d6$avg_speed, d7$avg_speed, d8$avg_speed, d9$avg_speed, d10$avg_speed)
#barplot(type_df$X.Bus~seq(1,10), main='Number of buses', xlab='Zone', ylim=c(0,80))
barplot(type_df$'Heavy Vehicle'~seq(1,10), main='Number of heavy vehicles', xlab='Zone', ylim=c(0,50))
#barplot(type_df$X.Medium.Vehicle~seq(1,10), main='Number of medium vehicles', xlab='Zone', ylim=c(0,120))

x11()
boxplot(d1$avg_speed, d2$avg_speed, d3$avg_speed, d4$avg_speed, d5$avg_speed, 
        d6$avg_speed, d7$avg_speed, d8$avg_speed, d9$avg_speed, d10$avg_speed, at = type_df$`Heavy Vehicle`, xlim=range(type_df$`Heavy Vehicle`))

graphics.off()


# Variability of avg_speed in zone 3 is high, let's focus on speed of different types of vehicles
x11()
boxplot(d3$avg_speed ~ d3$type, cex.axis = 0.7, col='gold', xlab='Vehicle type', ylab='Average speed')
# We note that motorcycles have avg_speed distributed on higher values than other types
barplot(type_df$X.Motorcycle~seq(1,10), main='Number of motorcycles', xlab='Zone', ylim=c(0,max(type_df$X.Motorcycle)))

# Try to remove motorcycles data to see if there's relation between avg_speed and the number of heavy vehicles
d1_nomot <- d1[d1$type != ' Motorcycle',]
d2_nomot <- d2[d2$type != ' Motorcycle',]
d3_nomot <- d3[d3$type != ' Motorcycle',]
d4_nomot <- d4[d4$type != ' Motorcycle',]
d5_nomot <- d5[d5$type != ' Motorcycle',]
d6_nomot <- d6[d6$type != ' Motorcycle',]
d7_nomot <- d7[d7$type != ' Motorcycle',]
d8_nomot <- d8[d8$type != ' Motorcycle',]
d9_nomot <- d9[d9$type != ' Motorcycle',]
d10_nomot <- d10[d10$type != ' Motorcycle',]

d_nm <- list(NULL)
for (i in seq(1,10)){
  d_nm[[i]] <- d[[i]][d[[i]]$type != ' Motorcycle',]
}

# Extract mean avg_speed by zone
mean_avg_speed_nm <- c(NULL)
for (i in seq(1,10)){
  mean_avg_speed_nm <- c(mean_avg_speed_nm, mean(d_nm[[i]]$avg_speed))
}

tab1_nomot <- table(d1_nomot$type)
tab2_nomot <- table(d2_nomot$type)
tab3_nomot <- table(d3_nomot$type)
tab4_nomot <- table(d4_nomot$type)
tab5_nomot <- table(d5_nomot$type)
tab6_nomot <- table(d6_nomot$type)
tab7_nomot <- table(d7_nomot$type)
tab8_nomot <- table(d8_nomot$type)
tab9_nomot <- table(d9_nomot$type)
tab10_nomot <- table(d10_nomot$type)

type_tab_nomot <- rbind(table(d1_nomot$type), table(d2_nomot$type), table(d3_nomot$type),
                  table(d4_nomot$type), table(d5_nomot$type), table(d6_nomot$type), 
                  table(d7_nomot$type), table(d8_nomot$type), table(d9_nomot$type), table(d10_nomot$type))
type_df_nomot <- data.frame(type_tab_nomot)
colnames(type_df_nomot) <- c('Bus', 'Car', 'Heavy Vehicle', 'Medium Vehicle', 'Taxi')

x11()
plot(type_df_nomot$'Heavy Vehicle', mean_avg_speed_nm)
plot(type_df_nomot$'Bus', mean_avg_speed_nm)


# Plot of the avg_speed wrt the number of special vehicles
x11()
layout(cbind(c(1,1,1), c(2,3,4)), widths=c(1,1,1), heights=c(1,1,1))
boxplot(d1_nomot$avg_speed, d2_nomot$avg_speed, d3_nomot$avg_speed, d4_nomot$avg_speed, d5_nomot$avg_speed, 
        d6_nomot$avg_speed, d7_nomot$avg_speed, d8_nomot$avg_speed, d9_nomot$avg_speed, d10_nomot$avg_speed)
barplot(type_df_nomot$X.Bus~seq(1,10), ylab ='Buses', xlab='Zone', ylim=c(0,80))
barplot(type_df_nomot$X.Heavy.Vehicle~seq(1,10), ylab ='Heavy vehicles', xlab='Zone', ylim=c(0,50))
barplot(type_df_nomot$X.Medium.Vehicle~seq(1,10), ylab ='Medium vehicles', xlab='Zone', ylim=c(0,120))

# Plots of avg_speed and heavy vehicles by zone
x11()
par(mfrow=c(2,1))
boxplot(d1_nomot$avg_speed, d2_nomot$avg_speed, d3_nomot$avg_speed, d4_nomot$avg_speed, d5_nomot$avg_speed, 
        d6_nomot$avg_speed, d7_nomot$avg_speed, d8_nomot$avg_speed, d9_nomot$avg_speed, d10_nomot$avg_speed, main = 'Avg_speed by zone')
barplot(type_df_nomot$X.Heavy.Vehicle+type_df_nomot$X.Medium.Vehicle~seq(1,10), ylab='Medium+Heavy vehicles', xlab='Zone', ylim=c(0,200))

# Consider only the motorbikes: check if their avg_speed is independent of the area
d1_mot <- d1[d1$type == ' Motorcycle',]
d2_mot <- d2[d2$type == ' Motorcycle',]
d3_mot <- d3[d3$type == ' Motorcycle',]
d4_mot <- d4[d4$type == ' Motorcycle',]
d5_mot <- d5[d5$type == ' Motorcycle',]
d6_mot <- d6[d6$type == ' Motorcycle',]
d7_mot <- d7[d7$type == ' Motorcycle',]
d8_mot <- d8[d8$type == ' Motorcycle',]
d9_mot <- d9[d9$type == ' Motorcycle',]
d10_mot <- d10[d10$type == ' Motorcycle',]

x11()
boxplot(d1_mot$avg_speed, d2_mot$avg_speed, d3_mot$avg_speed, d4_mot$avg_speed, d5_mot$avg_speed, 
        d6_mot$avg_speed, d7_mot$avg_speed, d8_mot$avg_speed, d9_mot$avg_speed, d10_mot$avg_speed, main = 'Avg_speed of motorbikes by zone')




