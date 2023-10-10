library(igraph)


# Import edges and counter datasets
edges <- NULL
for (num in 1:10){
  name <- paste0('Edges/net_zona',as.character(num),'_new.txt')
  ed <- read.table(name, header=T)
  edges <- rbind(edges, ed)
}

counters <- NULL
for (num in 1:10){
  name <- paste0('counters_24_0830_0900/','count_zona',as.character(num),'.txt')
  co <- read.table(name, header=T)
  counters <- rbind(counters, co)
}

vertices <- NULL
for (num in 1:10){
  name <- paste0('Nodes/Zona',as.character(num),'.kml')
  co <- nodes_from_kml(name)
  vertices <- rbind(vertices, co)
}

# Extract weights
weights <- ifelse(counters[,1]!=0, counters[,1]/counters[,2], 0)
weights
boxplot(weights)

edges$weight <- weights
edges$n_arco <- NULL

green <- which(weights < 1e+01) 
yellow <- which(weights >= 1e+01 & weights < 1e+02)
orange <- which(weights >= 1e+02 & weights < 1e+03)
red <- which(weights >= 1e+03)
col<- NULL  

col[green] <- 'green'
col[yellow] <- 'yellow'
col[orange] <- 'orange'
col[red] <- 'red'

# Generate colors percentages
#perc <- NULL
perc <- table(col)*100/350
perc

#perc_df <- data.frame(matrix(ncol = 4, nrow = 0))
perc_df <- rbind(perc_df, perc)

col_names <- c('green','orange','red','yellow')
names(perc_df) <- col_names
write.table(perc_df, 'colors_perc.txt')

# Generate graph
graph <- graph_from_data_frame(d=edges, directed=FALSE)
V(graph)


ord <- NULL
for (i in 1:length(V(graph))){
  ord <- c(ord, V(graph)[i])
}
ord <- as.numeric(names(ord))

coord_vert <- vertices[ord,]

graph <- graph_from_data_frame(d=edges, directed=FALSE, vertices=coord_vert)
graph

is_weighted(graph)

lo <- layout.norm(as.matrix(coord_vert))
x11()
plot(graph, layout = lo, vertex.label=NA, vertex.size=4)

# Minimum walk
# shortest_paths(graph, from_node, to_node)
steps <- as.numeric(names(shortest_paths(graph, which(ord==131), which(ord==45))[[1]][[1]]))
steps
vs <- rbind(vertices[131,],vertices[steps,],vertices[45,])

# NODO 131 OMONIA SQUARE
# NODO 93 CENTRAL CLINIC OF ATHENS
# NODO 142 UNIVERSITA TECNICA NAZIONALE DI ATENE
# NODO 45 SYNTAGMA SQUARE


x11()
plot(graph, 
     vertex.label = NA, 
     vertex.size = 1.5, 
     vertex.color = "black",
     layout = lgl)

ath_bb <- c(
  left = 23.72445,
  bottom = 37.97593,
  right = 23.73964,
  top = 37.99293
)

ath_bb <- c(
  left = 23.71845,
  bottom = 37.97553,
  right = 23.74364,
  top = 37.99293
)
center <- c(23.732045,37.983431)
zoom <- 15
upleft <- c(23.72445,37.99293)
lowright <- c(23.73964,37.97593)


library(ggmap)
library(GGally)

athens <- get_stamenmap(bbox = ath_bb, zoom=18, maptype='terrain')

ggmap(athens)

x11()
plot(athens)
par(new=TRUE)
plot(graph, layout = lo, vertex.label=NA, vertex.size=4)



# ggmap(athens) + ggnet(graph, layout.par= lo, vertex.size = 0, label.cex=0.5)  non funziona

ggmap(athens)
qmplot(LON,LAT,data=vs, col='red')


ggmap(athens) + geom_segment(data=vs,aes(x=LON,y=LAT,
                                         xend=LON, yend=LAT),
                             size=3,alpha=0.75) +
geom_point(data=vs,aes(x=LON,y=LAT,
                       xend=LON, yend=LAT), size=1,alpha=0.75)





# CORREZIONE COUNTERS ZONA 1
# trasferire dati strada 51 in strada 11
co <- read.table('counters_1_0830_0900/count_zona1.txt', header=T)
new_vel11 <- (co[51,1]*co[51,2]+co[11,1]*co[11,2])/(co[11,1]+co[51,1])
new_count11 <- co[11,1]+co[51,1]
nuovo <- co
co[11,] <- c(new_count11, new_vel11)
co <- co[-51,]
write.table(co, 'count_zona1.txt')

# scalare indietro di 2 tutti i nodi maggiori di 40
nod <- read.table('Edges/net_zona2_new.txt', header=T)
old <- nod
for (i in 1:dim(nod)[1]){
  for (j in 2:3){
    if (nod[i,j]>40)
      nod[i,j]=nod[i,j]-2
  }
}
write.table(nod,'net_zona2_new.txt')
