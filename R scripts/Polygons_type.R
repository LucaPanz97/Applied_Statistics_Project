
library(sp)
library(sf)

# Import dataframe
max_cols <- max(count.fields('1/20181024_d2_0830_0900.csv', sep = ';'))
traffic1 <- read.table('1/20181024_d2_0830_0900.csv', col.names = paste0("V",seq_len(max_cols)),
                       skip=1, fill=TRUE, sep=';')

Bus <- which(traffic1[,2]==' Bus')

# CONSIDER ONLY DESIRED TYPE OF VEHICLES
# Extract single trajectory dataframe with the desired frequency from one row 
oneline_traj_type <- function(total_line, freq, type){
  step <- freq%/%0.04*6
  if (total_line[2] != type)
    next
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

# Generate counter object
generate_counter <- function(n){
  poly.counter <- NULL
  poly.counter$count <- c(rep(0,n))
  poly.counter$speed <- c(rep(0,n))
  return (poly.counter)
}

# Generate counter object from one trajectory (and all the polygons)
onetraj_counter <- function(traj, polygons, poly.counter){
  npoly <- dim(polygons)[1]
  if (length(poly.counter$count)!=npoly){
    print('Error: counter variable is not correct')
    return (NULL)
  }
  for (i in seq(1,dim(traj)[1])){
    x.0 <- traj[i,6]
    y.0 <- traj[i,5]
    # For each point in the traj, cycle on the polygons and break when the check is true
    for (j in seq(1, npoly)){
      x <- c(as.numeric(polygons[j,2:5]))
      y <- c(as.numeric(polygons[j,6:9]))
      if (point.in.polygon(x.0, y.0, x, y)){
        poly.counter$count[j] <- poly.counter$count[j]+1
        poly.counter$speed[j] <- ((poly.counter$speed[j]*(poly.counter$count[j]-1))+traj[i,7])/poly.counter$count[j]
        next
      }
    }
  }
  return (poly.counter)
}

# Plot polygons from dataframe
plot.polygons <- function(poly.coords){
  xmin <- min(poly.coords[,2:5])
  xmax <- max(poly.coords[,2:5])
  ymin <- min(poly.coords[,6:9])
  ymax <- max(poly.coords[,6:9])
  plot(0, 0, xlim=c(xmin,xmax), ylim=c(ymin,ymax))
  for (j in seq(1, dim(poly.coords)[1])){
    x <- c(as.numeric(poly.coords[j,2:5]))
    y <- c(as.numeric(poly.coords[j,6:9]))
    polygon(x,y)
  }
}

# Generate dataframe of polygons from kml file passed by address
# Structure: 'ID', 'LAT1', 'LAT2', 'LAT3', 'LAT4','LON1', 'LON2', 'LON3', 'LON4'
polygons_from_kml <- function(kml_path){
  ccg <- st_read(kml_path)
  ncg <- st_geometry(ccg)
  xy <- st_zm(ncg, drop = TRUE, what = "ZM")
  df <- data.frame(matrix(ncol = 9, nrow = 0))
  for (i in seq(1,length(xy))){
    z <- c(xy[[i]][[1]])
    row <- c(i, z[1:4],z[6:9] )
    df <- rbind(df, row)
  }
  names(df) <- c('ID', 'LAT1', 'LAT2', 'LAT3', 'LAT4','LON1', 'LON2', 'LON3', 'LON4' )
  return(df)
}

# Generate counter from all the trajectories for a zone
alltraj_counter <- function(traffic, freq, polygons, type){
  c <- generate_counter(dim(polygons)[1])
  for (i in seq(1, dim(traffic)[1])){
    traj <- oneline_traj_type(traffic[i,], freq, type)
    
    if(length(dim(traj)[1])!=1)
      next
    
    c <- onetraj_counter(traj, polygons, c)
    if (i%%2==0){
      print('Processed trajectory ')
      print(i)
    }
  }
  return (c)
}

# Plot of the polygons with centroids from st_geometry
ccg <- st_read('KML files/Zona2.kml')
ncg <- st_geometry(ccg)
xy <- st_zm(ncg, drop = TRUE, what = "ZM")
plot(ncg, border = 'grey')
cntrd = st_centroid(ncg)
plot(cntrd, col = 'red', add = TRUE, cex = .5)

# TEST
# Import kml file and extract polygons dataframe
poly1 <- polygons_from_kml('KML files/Zona2.kml')

# Scan all the trajectories and generate the counter object
count <- alltraj_counter(traffic1, 1, poly1, ' Bus')
count

# Generate the counter dataframe and save
count_zona1 <- data.frame(count=count$count, avg_speed=count$speed)
write.table(count_zona1, 'count_zona10.txt')


