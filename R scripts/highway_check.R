
onetraj_check <- function(traj, polygons, our_pol){
  for (i in seq(1,dim(traj)[1])){
    local_check <- FALSE
    x.0 <- traj[i,6]
    y.0 <- traj[i,5]
    # For each point in the traj, cycle on the polygons and break when the check is true
    for (j in our_pol){
      x <- c(as.numeric(polygons[j,2:5]))
      y <- c(as.numeric(polygons[j,6:9]))
      if (point.in.polygon(x.0, y.0, x, y)){
        local_check <- TRUE
        next
      }
    }
    if (local_check == FALSE)
      return(FALSE)
  }
  return (TRUE)
}

alltraj_check <- function(traffic, polygons, freq, our_pol){
  res <- c(NULL)
  for (i in seq(1, dim(traffic)[1])){
    traj <- oneline_traj(traffic[i,], freq)
    
    if(length(dim(traj)[1])!=1)
      next
    
    bool <- onetraj_check(traj, polygons, our_pol)
    
    if (bool)
      res <- c(res, i)
    if (i%%2==0){
      print('Processed trajectory ')
      print(i)
    }
  }
  return (res)
}

index <- c(1,2,5,7,21)
poly1 <- polygons_from_kml('KML files/Zona3.kml')
lis3 <- alltraj_check(traffic1, poly1, 1, index)
lis3 # salvata in scrivania txt file
lis <- as.numeric(lis3)
high3 <- traffic1[lis3,]

index2 <- c(5,9,12,14,20)
poly1 <- polygons_from_kml('KML files/Zona2.kml')
lis2 <- alltraj_check(traffic1, poly1, 1, index2)
lis2
high2 <- traffic1[list2,]



plot_traj <- function(data, ind){
  x11()
  xmin <- min(data[ind,6])
  xmax <- max(data[ind,6])
  ymin <- min(data[ind,5])
  ymax <- max(data[ind,5])
  plot(xmin, ymin, xlim=c(xmin,xmax), ylim=c(ymin,ymax))
  for (i in ind){
    line <- oneline_traj(data[i,], 1)
    lines(line[,6], line[,5])
  }
}

l <- c(NULL)
highway <- NULL
plot_traj(traffic1, lis)
for (j in l){
tr <- oneline_traj(traffic1[lis2[j],], 1)
highway <- rbind(highway,tr[1:140,7])

ind <- c(303,317,  546,  576,  579,  581,
         790,  867,  880,  907,
         969, 1590, 1638, 1645, 1660,
         1666, 1670, 1676, 1865, 1900, 1960, 
         1988, 2138, 2142, 2202, 2329, 2396, 
         2397, 2403, 2451, 2465, 2466, 2467,
         2469, 2470, 2473, 2474, 2476, 2477,
         2478, 2481, 2482, 2483, 2484, 2485,
         2486, 2487, 2488, 2489, 2490, 2492,
         2493, 2494, 2495)

highw1 <- traffic1[ind,]


plot_traj(traffic1, lis3)

line_nodes <- nodes_from_kml('line.kml')

library(geosphere)
library(LearnGeom)

P2 <- c(line_nodes[1,1], line_nodes[1,2])
P1 <- c(line_nodes[2,1], line_nodes[2,2])
line <- CreateLinePoints(P1,P2)
DistancePoints(P1, P2)
max_d <- distm(P1, P2, fun = distHaversine)

sav <- NULL
my_lis <- c(1,2,8,34,40,41,42,44,47,51,52)

library(data.table)


high3 <- readRDS(file='semafori.rds')

plot(NULL,NULL, xlim=c(0,max_d), ylim=c(0,70))
for (i in 1:dim(high3)[1]){
tr <- oneline_traj(high3[i,], 1)
asc <- NULL
for (j in 1:dim(tr)[1]){
  x <- c(tr[j,6], tr[j,5])
  projection <- ProjectPoint(x, line)
  curv_abs <- distm(P1, projection, fun = distHaversine)
  asc <- c(asc,curv_abs)
}
speed <- tr[,7]
lines(asc, speed)
}

semafori <- nodes_from_kml('semafori.kml')
for (i in 1:4){
  p <- c(semafori[i,1], semafori[i,2])
  curv_abs <- distm(P1, p, fun = distHaversine)
  print(curv_abs)
  points(curv_abs,0, col='red', pch=13)
}


#x11()
#plot(tr[,7])
}



higw <- as.data.frame(highway)
write.table(higw, 'highway.txt')
