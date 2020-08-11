
source('R/nyt_county.R')
source('R/port_ecospat_parallel.R')
library(ENMeval)
library(maxnet)
library(cRacle)
library(maptools)
library(parallel)
nclus=24
rasterOptions(maxmemory=2e+11, memfrac=0.5, chunksize=2e+07)


varnum = c(1,2,4,6,7)
mc2 = march_clim
march_clim_sub = march_clim[[varnum]]
#test collinearity of these ^?

# get climate data for each county and plot weighted by popsize
# null model: is climate a factor in the US distribution

# validate SDM today (March)
occ = cv_ex %>%
  filter(date == last_day) %>%
  group_by(county, V5, V6) %>% 
  mutate(scale=cases)
occ2 = occ %>% 
  filter(!is.na(scale)) %>%
  expand(count = seq(1:(scale)))

#bg = rad_bg(as.data.frame(unique(cv_ex[,c('V6', 'V5')])), 
#            march_clim_sub, 
#            radius = 750, 
#            n=20)
bg <- as(march_clim_sub[[1]], "SpatialPixelsDataFrame")@coords
bg <- bg[sample(1:nrow(bg), size = nrow(occ2), replace=F),]

fc = c("L", "LQ")
set.eval = ENMevaluate(
  occ=occ2[,c('V6', 'V5')],
  env=march_clim_sub,
  rasterPreds = TRUE,
  parallel = TRUE,
  fc = fc,
  numCores = nclus,
  method = 'checkerboard2',
  bg.coords = bg[,c('x', 'y')],
  clamp = TRUE,
  RMvalues = c(0.5, 1, 1.5, 2, 2.5, 3)
  #RMvalues = c(0.5, 2)
)

write.csv(set.eval@results, file ='ENMeval_results_raw.csv')

best = set.eval@results %>%
  filter(avg.test.AUC == max(avg.test.AUC)) %>%
  filter(AICc == min(AICc)) %>%
  dplyr::select(settings) 
best = best[1,1]
#best = which(set.eval@results[, 'AICc'] == min(na.omit(set.eval@results[, 'AICc'])))
ev.set <-
  evaluate(occ[, c('V6', 'V5')], set.eval@bg.pts, set.eval@models[[best]], march_clim_sub)
thr.set <- threshold(ev.set)

# For picking model parameters on the complete set
best_param = set.eval@results[best, 1]
best_arr = strsplit(as.character(best_param), "_")

rm = best_arr[[1]][length(best_arr[[1]])]

fc1 = best_arr[[1]][1:(length(best_arr[[1]]) - 1)]

maxmatr = rbind(set.eval@occ.pts, set.eval@bg.pts)
pres = c(rep(1, nrow(set.eval@occ.pts)), rep(0, nrow(set.eval@bg.pts)))
maxmatr = cbind(maxmatr, pres)
maxextr = raster::extract(mc2, maxmatr[, c('LON', 'LAT')], buffer= 5000)
mextr_fin = apply(maxextr[[1]], 2, mean)
for(i in 2:length(maxextr)){
  if(length(maxextr[[i]])>1){
    mextr_fin=rbind(mextr_fin, apply(maxextr[[i]], 2, mean))
  } else {
    mextr_fin=rbind(mextr_fin, rep(NA, nlayers(mc2)))
  }
}
extr_all = cbind(maxmatr, mextr_fin)
extr_all = na.omit(extr_all)
maxmatr = extr_all[,1:3]
maxextr = extr_all[,4:ncol(extr_all)]


best_mod = maxnet(
  p = maxmatr[, 'pres'],
  data = as.data.frame(maxextr[,varnum]),
  maxnet.formula(
    p = maxmatr[, 'pres'],
    data = as.data.frame(maxextr[,varnum]),
    classes = stringr::str_to_lower(fc1)
  ),
  regmult = as.numeric(rm)
)

#mask for plots
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="United States")
## crop and mask
r2 <- crop(march_clim_sub, extent(SPDF))
march_clim_sub_mask <- mask(r2, SPDF)

m = predict(march_clim_sub_mask, best_mod, clamp=T, type = 'cloglog')

#predict SDM for human density

popdens = cv_new %>%
  group_by(county, V5, V6) %>%
  filter(!is.na(pop)) %>%
  expand(count = seq(1:(pop/1000))) %>%
  rename(LON=V6) %>%
  rename(LAT=V5)
save_na = raster::extract(march_clim_sub[[1]], popdens[, c('LON', 'LAT')]) 
save_na = cbind(popdens, save_na)
save_na = na.omit(save_na)
densmatr = rbind(as.data.frame(save_na[,c('LON', 'LAT')]), set.eval@bg.pts)
pres = (c(rep(1, nrow(save_na[,c('LON', 'LAT')])), rep(0, nrow(set.eval@bg.pts))))
densmatr = cbind(densmatr, pres) 
densextr = raster::extract(mc2, densmatr[, c('LON', 'LAT')], buffer=5000)
dextr_fin = apply(densextr[[1]], 2, mean)
for(i in 2:length(densextr)){
  if(length(densextr[[i]])>1){
    dextr_fin=rbind(dextr_fin, apply(densextr[[i]], 2, mean))
  } else {
    dextr_fin=rbind(dextr_fin, rep(NA, nlayers(mc2)))
  }
}
dextr_all = cbind(densmatr, dextr_fin)
dextr_all = na.omit(dextr_all)
densmatr = dextr_all[,1:3]
densextr = dextr_all[,4:ncol(dextr_all)]



dens_mod = maxnet(
  p = densmatr[, 'pres'],
  data = as.data.frame(densextr[,varnum]),
  maxnet.formula(
    p = densmatr[, 'pres'],
    data = as.data.frame(densextr[,varnum]),
    classes = stringr::str_to_lower(fc1)
  ),
  regmult = as.numeric(rm)
)

dens.m = predict(march_clim_sub_mask, dens_mod, clamp=T, type = 'cloglog')

#stats tests
cv_extr_pres = maxextr[maxmatr[,'pres']==1,]
pop_extr_pres = densextr[densmatr[,'pres']==1,]
stat_coll = data.frame(var=character(),
                       Up=numeric(), Up.adjust = numeric(), Utest_stat=numeric(),
                       KSp = numeric(), KSp.adjust = numeric(), KStest_stat=numeric(),
                       cv.n = numeric(), pop.n=numeric(), stringsAsFactors = F)
for(i in 1:ncol(cv_extr_pres)){
  cv.var = cv_extr_pres[,i]
  pop.var = pop_extr_pres[,i]
  wtest = wilcox.test(cv.var, pop.var, paired=F)
  kstest = ks.test(cv.var, pop.var)
  p.adj = p.adjust(wtest$p.value, method = 'holm', n = ncol(cv_extr_pres))
  ksp.adj = p.adjust(kstest$p.value, method = 'holm', n = ncol(cv_extr_pres))
  
  stat_coll[i,] = c(colnames(cv_extr_pres)[[i]], wtest$p.value, p.adj, wtest$statistic,
                    kstest$p.value, ksp.adj, kstest$statistic,
                    length(cv.var), length(pop.var))
}
#write.table(stat_coll, file='raw_stats.csv', sep=',')


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
ggsave(mapfig, file = 'FigureS2.png', height = 7, width=5, dpi=500 )
ggsave(mapfig, file = 'FigureS2.pdf', height = 7, width=5, dpi=500 )


#niche equivalency
library(ecospat)

Env = march_clim_sub
occd1 = densmatr[, c('LON', 'LAT')]
occd2 = maxmatr[, c('LON', 'LAT')]

# Get environmental data
extract1 = cbind(occd1, densextr[,varnum], densmatr[,'pres'])
extract2 = cbind(occd2, maxextr[,varnum], maxmatr[,'pres'])


colnames(extract1)[ncol(extract1)] = 'occ'
colnames(extract2)[ncol(extract2)] = 'occ'

dat1 = extract1
dat2 = extract2

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


####### test and visualize niche

eq.test <- test_ecospat.niche.equivalency.test(grid.clim1, grid.clim2,
                                               rep=500,
                                               ncores=nclus,
                                               alternative = "greater") ##rep = 1000 recommended for operational runs


sim.test <- test_ecospat.niche.similarity.test(grid.clim1, grid.clim2,
                                               rep=500, 
                                               ncores=nclus,
                                               alternative = "greater",
                                               rand.type=2) 

png('FigureS3.png', height=7, width = 5, units='in', res=500)
par(mfrow=c(2,1))
ecospat.plot.overlap.test(eq.test, "D", "Overlap")
mtext("A", side = 3, adj = 0.05, line = -1.3)
ecospat.plot.overlap.test(sim.test, "D", "Similarity")
mtext("B", side = 3, adj = 0.05, line = -1.3)
dev.off()


pdf('FigureS3.pdf', height=7, width = 5)
par(mfrow=c(2,1))
ecospat.plot.overlap.test(eq.test, "D", "Overlap")
mtext("A", side = 3, adj = 0.05, line = -1.3)
ecospat.plot.overlap.test(sim.test, "D", "Similarity")
mtext("B", side = 3, adj = 0.05, line = -1.3)
dev.off()



