
library(sp)

setwd("/Users/davidecaffu/Desktop/ProgettoAS/1")

d1.max_cols <- max(count.fields('20181024_d1_0830_0900.csv', sep = ';'))
d1 <- read.table('20181024_d1_0830_0900.csv', col.names = paste0("V",seq_len(d1.max_cols)),
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

# Traj dataframe per la singola traiettoria estratta con oneline_traj
# x_beg vettore contenente le latitudini dei vertici del poligono contenente l'inizio della strada da considerare
# y_beg vettore contenente le longitudini dei vertici del poligono contenente l'inizio della strada da considerare
# x_end vettore contenente le latitudini dei vertici del poligono contenente la fine della strada da considerare
# y_end vettore contenente le longitudini dei vertici del poligono contenente la fine della strada da considerare
# esempio x_beg <- c(37.98022, 23.73538, 37.98018, 23.73536) valori a caso

beg <- matrix( data=c(37.980177,23.730035,37.980646,23.732182, 37.979403, 23.732821, 37.978832, 23.731457), 
               nrow=4, ncol=2, byrow=T)

end <- matrix( data=c(37.976446,23.733120,37.977219,23.734910,37.978260,23.733887,37.977824,23.732139),
               nrow=4, ncol=2, byrow=T)
x_beg = beg[,1]
y_beg = beg[,2]
x_end = end[,1]
y_end = end[,2]


check_beginandend <- function(traj, x_beg, y_beg, x_end, y_end){
  if (point.in.polygon(traj[1,5], traj[1,6], x_beg , y_beg))
    if(point.in.polygon(traj[dim(traj)[1],5], traj[1,6], x_end , y_end))
      return (TRUE)
  else if (point.in.polygon(traj[dim(traj)[1],5], traj[1,6], x_end , y_end))
    if(point.in.polygon(traj[1,5], traj[1,6], x_beg , y_beg))
      return (FALSE)
    return (FALSE)
}

for (i in seq(1,dim(d1)[1])){
  if (i%%10==0){
    print('Current trajectory: ')
    print(i)
  }
  count = 1
  my_traj = {}
  traj <- oneline_traj(d1[i,], 0.40)
  if (check_beginandend(traj, x_beg, y_beg, x_end, y_end)){
    my_traj[count] <- traj
    count <- count+1
  }
}
