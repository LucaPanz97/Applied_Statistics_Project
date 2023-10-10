rm(list=ls())
ls()

library(ggmap)

setwd("/Users/davidecaffu/Desktop/ProgettoAS/1")

d1.max_cols <- max(count.fields('20181024_d1_0830_0900.csv', sep = ';'))
d1 <- read.table('20181024_d1_0830_0900.csv', col.names = paste0("V",seq_len(d1.max_cols)),
                      skip=1, fill=TRUE, sep=';')

d2.max_cols <- max(count.fields('20181024_d2_0830_0900.csv', sep = ';'))
d2 <- read.table('20181024_d2_0830_0900.csv', col.names = paste0("V",seq_len(d2.max_cols)),
                 skip=1, fill=TRUE, sep=';')


oneline_traj <- function(total_line, freq){
  step <- freq%/%0.04*6
  id_variables <- total_line[1:4]
  names <- c('track_id', 'type', 'traveled_d', 'avg_speed', 'lat', 'lon', 'speed', 'lon_acc', 'lat_acc', 'time')
  df <- data.frame(colnames(names))
  for (i in seq(5, length(total_line), by=step)){
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

find_corners <- function(data, n){
  x.min <- NULL
  x.max <- NULL
  y.min <- NULL
  y.max <- NULL
 for (i in seq(5,dim(data)[2]-1,by=6)){
   x.min <- min(x.min, min(data[1:n,i], na.rm = TRUE))
   x.max <- max(x.max, max(data[1:n,i], na.rm = TRUE))
   y.min <- min(y.min, min(data[1:n,i+1], na.rm = TRUE))
   y.max <- max(y.max, max(data[1:n,i+1], na.rm = TRUE))
   if ((i-5)%%6000 == 0) print(paste0('Processing the ', i, 'th line'))
 }
  return (c(x.min, x.max, y.min, y.max))
}

colors_gen <- function(line){
  col <- NULL
  for (i in seq(1, dim(line)[1])){
    if (line[i,7] <= 10) col[i] = '#0080FF'
    if (line[i,7] <= 20 & line[i,7] > 10) col[i] = '#00FFFF'
    if (line[i,7] <= 30 & line[i,7] > 20) col[i] = '#00FF00'
    if (line[i,7] <= 40 & line[i,7] > 30) col[i] = '#FFFF00'
    if (line[i,7] <= 50 & line[i,7] > 40) col[i] = '#FF8000'
    if (line[i,7] > 50) col[i] = '#FF0000'
  }
  return (col)
}

plot_traj <- function(data, n, coords=NULL, freq){
  x11()
  if (is.null(coords)){
  corners <- find_corners(data, n)
  print('Finding corners')
  }
  else {
    corners <- coords
    print('Inserted corners: ')
    print(corners)
  }
  plot(NULL, xlim=c(corners[1], corners[2]), ylim=c(corners[3], corners[4]), type="l")
  for (i in seq(1,n)){
    line <- oneline_traj(data[i,], freq)
    line.col <- colors_gen(line)
    if (i%%100==0) print(paste0("Processing the ",i,"th vehicle."))
    lines(line[,5], line[,6], col=line.col)
  }
}


d1.corners <- c(37.97685, 37.98125, 23.73441, 23.73849)
line1 <- oneline_traj(d1[1,], 0.2)
freq = 0.2

plot_traj(d1, dim(d1)[1], d1.corners, freq)
dev.copy2pdf()
