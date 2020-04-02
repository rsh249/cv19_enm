source('R/nyt_county.R')
source('R/port_ecospat_parallel.R')
library(ENMeval)
library(maxnet)
library(cRacle)
library(maptools)
library(parallel)

varnum = c(1,2,4,6,7)
march_clim = march_clim[[varnum]]
#test collinearity of these ^?

# get climate data for each county and plot weighted by popsize

# validate SDM today (March)
occ = cv_ex %>%
  filter(date == last_day) %>%
  group_by(county, V5, V6) %>% 
  mutate(scale=cases)
#occ$scale = occ$scale/sum(occ$scale, na.rm=T)*100000
occ2 = occ %>% 
  filter(!is.na(scale)) %>%
  expand(count = seq(1:(scale)))
# 
# bg = rad_bg(as.data.frame(unique(cv_ex[,c('V6', 'V5')])), 
#             march_clim, 
#             radius = 750, 
#             n=20)
bg <- as(march_clim[[1]], "SpatialPixelsDataFrame")@coords
bg <- bg[sample(1:nrow(bg), size = nrow(occ2), replace=F),]

fc = c("L", "LQ")
set.eval = ENMevaluate(
  occ=occ2[,c('V6', 'V5')],
  env=march_clim,
  rasterPreds = TRUE,
  parallel = TRUE,
  fc = fc,
  numCores = 48,
  method = 'checkerboard2',
  bg.coords = bg[,c('x', 'y')],
  clamp = TRUE,
  RMvalues = c(0.5, 1, 1.5, 2, 2.5, 3)
  #RMvalues = c(0.5, 2)
)

write.csv(set.eval@results, file ='ENMeval_results_raw.csv')
best = which(set.eval@results[, 'AICc'] == min(na.omit(set.eval@results[, 'AICc'])))
ev.set <-
  evaluate(occ[, c('V6', 'V5')], set.eval@bg.pts, set.eval@models[[best]], march_clim)
thr.set <- threshold(ev.set)

# For picking model parameters on the complete set
best_param = set.eval@results[best, 1]
best_arr = strsplit(as.character(best_param), "_")

rm = best_arr[[1]][length(best_arr[[1]])]

fc1 = best_arr[[1]][1:(length(best_arr[[1]]) - 1)]

maxmatr = rbind(set.eval@occ.pts, set.eval@bg.pts)
pres = c(rep(1, nrow(set.eval@occ.pts)), rep(0, nrow(set.eval@bg.pts)))
maxmatr = cbind(maxmatr, pres)
maxextr = raster::extract(march_clim, maxmatr[, c('LON', 'LAT')])
best_mod = maxnet(
  p = maxmatr[, 'pres'],
  data = as.data.frame(maxextr),
  maxnet.formula(
    p = maxmatr[, 'pres'],
    data = as.data.frame(maxextr),
    classes = stringr::str_to_lower(fc1)
  ),
  regmult = as.numeric(rm)
)

#mask for plots
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="United States")
## crop and mask
r2 <- crop(march_clim, extent(SPDF))
march_clim_mask <- mask(r2, SPDF)

m = predict(march_clim_mask, best_mod, clamp=T, type = 'cloglog')

#predict SDM for human density

popdens = cv_new %>%
  group_by(county, V5, V6) %>%
  filter(!is.na(pop)) %>%
  expand(count = seq(1:(pop/1000))) %>%
  rename(LON=V6) %>%
  rename(LAT=V5)
save_na = raster::extract(march_clim, popdens[, c('LON', 'LAT')]) 
save_na = cbind(popdens, save_na)
save_na = na.omit(save_na)
densmatr = rbind(as.data.frame(save_na[,c('LON', 'LAT')]), set.eval@bg.pts)
pres = (c(rep(1, nrow(save_na[,c('LON', 'LAT')])), rep(0, nrow(set.eval@bg.pts))))
densmatr = cbind(densmatr, pres) 
densextr = raster::extract(march_clim, densmatr[, c('LON', 'LAT')])
dens_mod = maxnet(
  p = densmatr[, 'pres'],
  data = as.data.frame(densextr),
  maxnet.formula(
    p = densmatr[, 'pres'],
    data = as.data.frame(densextr),
    classes = stringr::str_to_lower(fc1)
  ),
  regmult = as.numeric(rm)
)

dens.m = predict(march_clim_mask, dens_mod, clamp=T, type = 'cloglog')

#plot SDMs

cv_spdf <- as(m, "SpatialPixelsDataFrame")
cv_df <- as.data.frame(cv_spdf)
colnames(cv_df) <- c("Suitability", "x", "y")
(mapp_cv = ggplot(data=cv_df) +
    geom_tile(aes(x=x, y=y, fill=Suitability)) +
    coord_quickmap() +
    theme_minimal() +
    scale_fill_gradientn(colors=c('violet', 'darkblue', 'grey80', 'darkred', 'darkorange'),
                         na.value='black',
                         limits=c(0,1)) +
    theme(panel.background = element_rect(fill='black')) +
    labs(x="Longitude", y="Latitude", title='SARS-CoV2 Distribution Model')
  
)

pop_spdf <- as(dens.m, "SpatialPixelsDataFrame")
pop_df <- as.data.frame(pop_spdf)
colnames(pop_df) <- c("Suitability", "x", "y")
(mapp_pop = ggplot(data=pop_df) +
    geom_tile(aes(x=x, y=y, fill=Suitability)) +
    coord_quickmap() +
    theme_minimal() +
    scale_fill_gradientn(colors=c('violet', 'darkblue', 'grey80', 'darkred', 'darkorange'),
                         na.value='black',
                         limits=c(0,1)) +
    theme(panel.background = element_rect(fill='black')) +
    labs(x="Longitude", y="Latitude", title='Human Population Distribution Model')
  
)

mapfig = plot_grid(mapp_cv, mapp_pop, nrow=2, ncol=1, labels='AUTO')
ggsave(mapfig, file = 'map_fig3_raw.png', height = 7, width=5, dpi=500 )
ggsave(mapfig, file = 'map_fig3_raw.pdf', height = 7, width=5, dpi=500 )


#niche equivalency
library(ecospat)
library(ENMTools)
Env = march_clim
occd1 = densmatr[, c('LON', 'LAT')]
occd2 = maxmatr[, c('LON', 'LAT')]

bg1 = set.eval@bg.pts
bg2 = set.eval@bg.pts

# Get environmental data
extract1 = na.omit(cbind(occd1[,c('LON', 'LAT')], extract(Env, occd1[,c('LON', 'LAT')]), rep(1, nrow(occd1))))
extract2 = na.omit(cbind(occd2[,c('LON', 'LAT')], extract(Env, occd2[,c('LON', 'LAT')]), rep(1, nrow(occd2))))

colnames(extract1)[ncol(extract1)] = 'occ'
colnames(extract2)[ncol(extract2)] = 'occ'

extbg1 = na.omit(cbind(bg1, extract(Env, bg1), rep(0, nrow(bg1))))
extbg2 = na.omit(cbind(bg2, extract(Env, bg2), rep(0, nrow(bg2))))

colnames(extbg1)[ncol(extbg1)] = 'occ'
colnames(extbg2)[ncol(extbg2)] = 'occ'

dat1 = rbind(extract1, extbg1)
dat2 = rbind(extract2, extbg2)


pca.env <- dudi.pca(
  rbind(dat1, dat2)[,3:(2+nlayers(Env))],
  scannf=FALSE,
  nf=2
)

#Variable contribution
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
scores.globclim<-pca.env$li # PCA scores for the whole study area

scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp1 <- suprow(pca.env,
                     extract1[which(extract1[,(3+nlayers(Env))]==1),3:(2+nlayers(Env))])$li # PCA scores for the species 1 distribution

scores.sp2 <- suprow(pca.env,
                     extract2[which(extract2[,(3+nlayers(Env))]==1),3:(2+nlayers(Env))])$li # PCA scores for the species 1 distribution

scores.clim1 <- suprow(pca.env,dat1[,3:(2+nlayers(Env))])$li # PCA scores for the whole native study area

scores.clim2 <- suprow(pca.env,dat2[,3:(2+nlayers(Env))])$li # PCA scores for the whole native study area


grid.clim1 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim1,
  sp = scores.sp1,
  R = 100,
  th.sp = 0
)
grid.clim2 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim2,
  sp = scores.sp2,
  R = 100,
  th.sp = 0
)

D.overlap <- ecospat.niche.overlap (grid.clim1, grid.clim2, cor=T)$D 
D.overlap

### FUNCTIONS BELOW BORROWED FROM library(ecospat) to solve parallel error...

overlap.eq.gen <- function(repi, z1, z2) {
  if (is.null(z1$y)) {
    # overlap on one axis
    
    occ.pool <- c(z1$sp, z2$sp)  # pool of random occurrences
    rand.row <- sample(1:length(occ.pool), length(z1$sp))  # random reallocation of occurrences to datasets
    sp1.sim <- occ.pool[rand.row]
    sp2.sim <- occ.pool[-rand.row]
  }
  
  if (!is.null(z1$y)) {
    # overlap on two axes
    
    occ.pool <- rbind(z1$sp, z2$sp)  # pool of random occurrences
    row.names(occ.pool)<-c()  # remove the row names
    rand.row <- sample(1:nrow(occ.pool), nrow(z1$sp))  # random reallocation of occurrences to datasets
    sp1.sim <- occ.pool[rand.row, ]
    sp2.sim <- occ.pool[-rand.row, ]
  }
  
  z1.sim <- ecospat.grid.clim.dyn(z1$glob, z1$glob1, data.frame(sp1.sim), R = length(z1$x))  # gridding
  z2.sim <- ecospat.grid.clim.dyn(z2$glob, z2$glob1, data.frame(sp2.sim), R = length(z2$x))
  
  o.i <- ecospat.niche.overlap(z1.sim, z2.sim, cor = TRUE)  # overlap between random and observed niches
  sim.o.D <- o.i$D  # storage of overlaps
  sim.o.I <- o.i$I
  return(c(sim.o.D, sim.o.I))
}

test_ecospat.niche.equivalency.test <- function(z1, z2, rep, alternative = "greater", ncores=1) {
  
  R <- length(z1$x)
  l <- list()
  
  obs.o <- ecospat.niche.overlap(z1, z2, cor = TRUE)  #observed niche overlap
  
  if (ncores == 1){
    sim.o <- as.data.frame(matrix(unlist(lapply(1:rep, overlap.eq.gen, z1, z2)), byrow = TRUE,
                                  ncol = 2))  #simulate random overlap
  }else{
    #number of cores attributed for the permutation test
    cl <- makeCluster(ncores)  #open a cluster for parallelization
    invisible(clusterEvalQ(cl,library("ecospat")))  #import the internal function into the cluster
    sim.o <- as.data.frame(matrix(unlist(parLapply(cl, 1:rep, overlap.eq.gen, z1, z2)), byrow = TRUE,
                                  ncol = 2))  #simulate random overlap
    stopCluster(cl)  #shutdown the cluster
  }
  colnames(sim.o) <- c("D", "I")
  l$sim <- sim.o  # storage
  l$obs <- obs.o  # storage
  
  if (alternative == "greater") {
    l$p.D <- (sum(sim.o$D >= obs.o$D) + 1)/(length(sim.o$D) + 1)  # storage of p-values alternative hypothesis = greater -> test for niche conservatism/convergence
    l$p.I <- (sum(sim.o$I >= obs.o$I) + 1)/(length(sim.o$I) + 1)  # storage of p-values alternative hypothesis = greater -> test for niche conservatism/convergence
  }
  if (alternative == "lower") {
    l$p.D <- (sum(sim.o$D <= obs.o$D) + 1)/(length(sim.o$D) + 1)  # storage of p-values alternative hypothesis = lower -> test for niche divergence
    l$p.I <- (sum(sim.o$I <= obs.o$I) + 1)/(length(sim.o$I) + 1)  # storage of p-values alternative hypothesis = lower -> test for niche divergence
  }
  
  return(l)
}

test_ecospat.niche.similarity.test <- function(z1, z2, rep, alternative = "greater", rand.type = 1, ncores = 1) {
  
  R <- length(z1$x)
  l <- list()
  obs.o <- ecospat.niche.overlap(z1, z2, cor = TRUE)  #observed niche overlap
  z1$z.uncor <- as.matrix(z1$z.uncor)
  z1$Z <- as.matrix(z1$Z)
  z1$z <- as.matrix(z1$z)
  z2$z.uncor <- as.matrix(z2$z.uncor)
  z2$Z <- as.matrix(z2$Z)
  z2$z <- as.matrix(z2$z)
  
  if (ncores==1) {
    sim.o <- as.data.frame(matrix(unlist(lapply(1:rep, overlap.sim.gen, z1, z2, rand.type = rand.type)),
                                  byrow = TRUE, ncol = 2))  #simulate random overlap  
  } else {
    cl <- makeCluster(ncores)  #open a cluster for parallelization
    invisible(clusterEvalQ(cl,library("ecospat")))  #import the internal function into the cluster
    sim.o <- as.data.frame(matrix(unlist(parLapply(cl, 1:rep, overlap.sim.gen, z1, z2, rand.type = rand.type)),
                                  byrow = TRUE, ncol = 2))  #simulate random overlap
    stopCluster(cl)  #shutdown the cluster
  }
  colnames(sim.o) <- c("D", "I")
  l$sim <- sim.o  # storage
  l$obs <- obs.o  # storage
  
  if (alternative == "greater") {
    l$p.D <- (sum(sim.o$D >= obs.o$D) + 1)/(length(sim.o$D) + 1)  # storage of p-values alternative hypothesis = greater -> test for niche conservatism/convergence
    l$p.I <- (sum(sim.o$I >= obs.o$I) + 1)/(length(sim.o$I) + 1)  # storage of p-values alternative hypothesis = greater -> test for niche conservatism/convergence
  }
  if (alternative == "lower") {
    l$p.D <- (sum(sim.o$D <= obs.o$D) + 1)/(length(sim.o$D) + 1)  # storage of p-values alternative hypothesis = lower -> test for niche divergence
    l$p.I <- (sum(sim.o$I <= obs.o$I) + 1)/(length(sim.o$I) + 1)  # storage of p-values alternative hypothesis = lower -> test for niche divergence
  }
  
  return(l)
}

overlap.sim.gen <- function(repi, z1, z2, rand.type = rand.type) {
  R1 <- length(z1$x)
  R2 <- length(z2$x)
  if (is.null(z1$y) & is.null(z2$y)) {
    if (rand.type == 1) {
      # if rand.type = 1, both z1 and z2 are randomly shifted, if rand.type =2, only z2 is randomly
      # shifted
      center.z1 <- which(z1$z.uncor == 1)  # define the centroid of the observed niche
      Z1 <- z1$Z/max(z1$Z)
      rand.center.z1 <- sample(1:R1, size = 1, replace = FALSE, prob = Z1)  # randomly (weighted by environment prevalence) define the new centroid for the niche
      xshift.z1 <- rand.center.z1 - center.z1  # shift on x axis
      z1.sim <- z1
      z1.sim$z <- rep(0, R1)  # set intial densities to 0
      for (i in 1:length(z1$x)) {
        i.trans.z1 <- i + xshift.z1
        if (i.trans.z1 > R1 | i.trans.z1 < 0)
          (next)()  # densities falling out of the env space are not considered
        z1.sim$z[i.trans.z1] <- z1$z[i]  # shift of pixels
      }
      z1.sim$z <- (z1$Z != 0) * 1 * z1.sim$z  # remove densities out of existing environments
      z1.sim$z.cor <- (z1.sim$z/z1$Z)/max((z1.sim$z/z1$Z), na.rm = TRUE)  #transform densities into occupancies
      z1.sim$z.cor[which(is.na(z1.sim$z.cor))] <- 0
      z1.sim$z.uncor <- z1.sim$z/max(z1.sim$z, na.rm = TRUE)
      z1.sim$z.uncor[which(is.na(z1.sim$z.uncor))] <- 0
    }
    
    center.z2 <- which(z2$z.uncor == 1)  # define the centroid of the observed niche
    Z2 <- z2$Z/max(z2$Z)
    rand.center.z2 <- sample(1:R2, size = 1, replace = FALSE, prob = Z2)  # randomly (weighted by environment prevalence) define the new centroid for the niche
    
    xshift.z2 <- rand.center.z2 - center.z2  # shift on x axis
    z2.sim <- z2
    z2.sim$z <- rep(0, R2)  # set intial densities to 0
    for (i in 1:length(z2$x)) {
      i.trans.z2 <- i + xshift.z2
      if (i.trans.z2 > R2 | i.trans.z2 < 0)
        (next)()  # densities falling out of the env space are not considered
      z2.sim$z[i.trans.z2] <- z2$z[i]  # shift of pixels
    }
    z2.sim$z <- (z2$Z != 0) * 1 * z2.sim$z  # remove densities out of existing environments
    z2.sim$z.cor <- (z2.sim$z/z2$Z)/max((z2.sim$z/z2$Z), na.rm = TRUE)  #transform densities into occupancies
    z2.sim$z.cor[which(is.na(z2.sim$z.cor))] <- 0
    z2.sim$z.uncor <- z2.sim$z/max(z2.sim$z, na.rm = TRUE)
    z2.sim$z.uncor[which(is.na(z2.sim$z.uncor))] <- 0
  }
  
  if (!is.null(z2$y) & !is.null(z1$y)) {
    if (rand.type == 1) {
      # if rand.type = 1, both z1 and z2 are randomly shifted, if rand.type =2, only z2 is randomly
      # shifted
      centroid.z1 <- which(z1$z.uncor == 1, arr.ind = TRUE)[1, ]  # define the centroid of the observed niche
      Z1 <- z1$Z/max(z1$Z)
      rand.centroids.z1 <- which(Z1 > 0, arr.ind = TRUE)  # all pixels with existing environments in the study area
      weight.z1 <- Z1[Z1 > 0]
      rand.centroid.z1 <- rand.centroids.z1[sample(1:nrow(rand.centroids.z1), size = 1, replace = FALSE,
                                                   prob = weight.z1), ]  # randomly (weighted by environment prevalence) define the new centroid for the niche
      xshift.z1 <- rand.centroid.z1[1] - centroid.z1[1]  # shift on x axis
      yshift.z1 <- rand.centroid.z1[2] - centroid.z1[2]  # shift on y axis
      z1.sim <- z1
      z1.sim$z <- matrix(rep(0, R1 * R1), ncol = R1, nrow = R1)  # set intial densities to 0
      for (i in 1:R1) {
        for (j in 1:R1) {
          i.trans.z1 <- i + xshift.z1
          j.trans.z1 <- j + yshift.z1
          if (i.trans.z1 > R1 | i.trans.z1 < 0 | j.trans.z1 > R1 | j.trans.z1 < 0)
            (next)()  # densities falling out of the env space are not considered
          #if (j.trans.z1 > R1 | j.trans.z1 < 0)
          #  (next)()
          z1.sim$z[i.trans.z1, j.trans.z1] <- z1$z[i, j]  # shift of pixels
        }
      }
      z1.sim$z <- (z1$Z != 0) * 1 * z1.sim$z  # remove densities out of existing environments
      z1.sim$z.cor <- (z1.sim$z/z1$Z)/max((z1.sim$z/z1$Z), na.rm = TRUE)  #transform densities into occupancies
      z1.sim$z.cor[which(is.na(z1.sim$z.cor))] <- 0
      z1.sim$z.uncor <- z1.sim$z/max(z1.sim$z, na.rm = TRUE)
      z1.sim$z.uncor[which(is.na(z1.sim$z.uncor))] <- 0
    }
    centroid.z2 <- which(z2$z.uncor == 1, arr.ind = TRUE)[1, ]  # define the centroid of the observed niche
    Z2 <- z2$Z/max(z2$Z)
    rand.centroids.z2 <- which(Z2 > 0, arr.ind = TRUE)  # all pixels with existing environments in the study area
    weight.z2 <- Z2[Z2 > 0]
    rand.centroid.z2 <- rand.centroids.z2[sample(1:nrow(rand.centroids.z2), size = 1, replace = FALSE,
                                                 prob = weight.z2), ]  # randomly (weighted by environment prevalence) define the new centroid for the niche
    xshift.z2 <- rand.centroid.z2[1] - centroid.z2[1]  # shift on x axis
    yshift.z2 <- rand.centroid.z2[2] - centroid.z2[2]  # shift on y axis
    z2.sim <- z2
    z2.sim$z <- matrix(rep(0, R2 * R2), ncol = R2, nrow = R2)  # set intial densities to 0
    for (i in 1:R2) {
      for (j in 1:R2) {
        i.trans.z2 <- i + xshift.z2
        j.trans.z2 <- j + yshift.z2
        #if (i.trans.z2 > R2 | i.trans.z2 < 0)
        if (i.trans.z2 > R2 | i.trans.z2 < 0 | j.trans.z2 > R2 | j.trans.z2 < 0)
          (next)()  # densities falling out of the env space are not considered
        #if (j.trans.z2 > R2 | j.trans.z2 < 0)
        #  (next)()
        z2.sim$z[i.trans.z2, j.trans.z2] <- z2$z[i, j]  # shift of pixels
      }
    }
    z2.sim$z <- (z2$Z != 0) * 1 * z2.sim$z  # remove densities out of existing environments
    z2.sim$z.cor <- (z2.sim$z/z2$Z)/max((z2.sim$z/z2$Z), na.rm = TRUE)  #transform densities into occupancies
    z2.sim$z.cor[which(is.na(z2.sim$z.cor))] <- 0
    z2.sim$z.uncor <- z2.sim$z/max(z2.sim$z, na.rm = TRUE)
    z2.sim$z.uncor[which(is.na(z2.sim$z.uncor))] <- 0
  }
  
  if (rand.type == 1) {
    o.i <- ecospat.niche.overlap(z1.sim, z2.sim, cor = TRUE)
  }
  if (rand.type == 2)
  {
    o.i <- ecospat.niche.overlap(z1, z2.sim, cor = TRUE)
  }  # overlap between random and observed niches
  sim.o.D <- o.i$D  # storage of overlaps
  sim.o.I <- o.i$I
  return(c(sim.o.D, sim.o.I))
}


####### test and visualize nich

eq.test <- test_ecospat.niche.equivalency.test(grid.clim1, grid.clim2,
                                               rep=500,
                                               ncores=48,
                                               alternative = "greater") ##rep = 1000 recommended for operational runs


sim.test <- test_ecospat.niche.similarity.test(grid.clim1, grid.clim2,
                                               rep=500, 
                                               ncores=48,
                                               alternative = "greater",
                                               rand.type=2) 

pdf('overlap_graphic_raw.png', height=7, width = 5, units='in', res=500)
par(mfrow=c(2,1))
ecospat.plot.overlap.test(eq.test, "D", "Overlap")
mtext("A", side = 3, adj = 0.05, line = -1.3)
ecospat.plot.overlap.test(sim.test, "D", "Similarity")
mtext("B", side = 3, adj = 0.05, line = -1.3)
dev.off()

pdf('overlap_graphic_raw.pdf', height=7, width = 5)
par(mfrow=c(2,1))
ecospat.plot.overlap.test(eq.test, "D", "Overlap")
mtext("A", side = 3, adj = 0.05, line = -1.3)
ecospat.plot.overlap.test(sim.test, "D", "Similarity")
mtext("B", side = 3, adj = 0.05, line = -1.3)
dev.off()








