library(fda)

sem <- readRDS(file='semafori.rds')

traffic <- read.table('highway.txt')
highway <- as.matrix(traffic)

abscissa <- 0:452

ind <- c(1,2,4,5,6,7,9,10)
i=11

plot(NULL,NULL, xlim=c(0,max_d), ylim=c(0,55), 
     xlab='Position [m]', ylab='Speed [km/h]', main='Traffic lights locations based on vehicles trajectories')
for (i in ind){
  tr <- oneline_traj(high3[i,], 1)
  asc <- NULL
  for (j in 1:dim(tr)[1]){
    x <- c(tr[j,6], tr[j,5])
    projection <- ProjectPoint(x, line)
    curv_abs <- distm(P1, projection, fun = distHaversine)
    asc <- c(asc,curv_abs)
  }
  speed <- tr[,7]
  abscissa <- floor(asc)
  
  basis <- create.bspline.basis(rangeval=c(min(abscissa),max(abscissa)), nbasis=nbasis, norder=m)
  
  Obs <- speed
  basismat <- eval.basis(abscissa, basis)
  lsfit(basismat, Obs, intercept=FALSE)$coef
  
  Xsp0 <- basismat %*% lsfit(basismat,Obs, intercept=FALSE)$coef
 
  points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
}
lines(asc, speed)


matplot(abscissa,t(highway), type='l', xlab='obs', ylab='speed')

# Smoothing

# Spline
m <- 10           # spline order 
degree <- m-1    # spline degree 
nbasis <- 35

# Create the basis
basis <- create.bspline.basis(rangeval=c(1,140), nbasis=nbasis, norder=m)

Obs <- highway[5,]
basismat <- eval.basis(abscissa, basis)
lsfit(basismat, Obs, intercept=FALSE)$coef

Xsp0 <- basismat %*% lsfit(basismat,Obs, intercept=FALSE)$coef

par(mfrow=c(1,1))
plot(abscissa,Obs,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
abline(v=basis$params)

# Fourier
basis1 <- create.fourier.basis(rangeval=c(1,140),nbasis=20)
data_L.fd <- Data2fd(y = t(highway[1:6,]), argvals=abscissa, basis1)
plot.fd(data_L.fd, main="Fourier")

#KMA
set.seed(1)
fdakma_example <- kma(
  x=abscissa, y0=highway, n.clust = 3, 
  warping.method = 'affine', 
  similarity.method = 'd0.pearson',  # similarity computed as the cosine
  # (correlation)
  center.method = 'k-means'
  #seeds = c(1,21) # you can give a little help to the algorithm...
)

kma.show.results(fdakma_example)

kma.compare_example_3 <- kma.compare (
  x=abscissa, y0=highway, n.clust = 1:3, 
  warping.method = c('NOalignment', 'shift', 'dilation', 'affine'), 
  similarity.method = 'd0.pearson',
  center.method = 'k-means', 
  #seeds = c(1,21,30),
  plot.graph=1)
# interpretation: alignment provides relevant results


