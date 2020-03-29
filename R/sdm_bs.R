source('R/nyt_county.R')
library(ENMeval)
library(maxnet)
library(cRacle)
library(maptools)

varnum = c(1,2,3,4,5,6,7)
march_clim = march_clim[[varnum]]
# get climate data for each county and plot weighted by popsize
# null model: is climate a factor in the US distribution

# validate SDM today (March)
occ = cv_ex %>%
  filter(date == last_day) %>%
  group_by(county, V5, V6) %>% 
  mutate(scale=cases/pop)
occ$scale = occ$scale/sum(occ$scale, na.rm=T)*100000
occ2 = occ %>% 
  filter(!is.na(scale)) %>%
  expand(count = seq(1:(scale)))

bg = rad_bg(as.data.frame(unique(cv_ex[,c('V6', 'V5')])), 
            march_clim, 
            radius = 750, 
            n=20)
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

m = predict(march_clim_mask, best_mod, clamp=T, type = 'logistic')

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

dens.m = predict(march_clim_mask, dens_mod, clamp=T, type = 'logistic')


# project SDM into Tristate for May/June
may_clim = cl_stack[[grep("05", names(cl_stack))]]
r2 <- crop(may_clim, extent(SPDF))
may_clim <- mask(r2, SPDF)
may_clim = may_clim[[varnum]]
names(may_clim) = names(march_clim)
m.may = predict(may_clim, best_mod, clamp=T, type = 'logistic')
plot(m.may - m) # set upggplot comparing March to May

test_spdf <- as(m, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Degrees", "x", "y")
(mapp_march = ggplot(data=test_df) +  
    geom_tile(aes(x=x, y=y, fill=Degrees)) + 
    coord_quickmap() + 
    theme_minimal() + 
    scale_fill_gradient2(low = "darkblue",
                         mid = "white",
                         high = "darkred",
                         na.value='black', 
                         limits=c(0,1)) +
    theme(panel.background = element_rect(fill='black')) +
    labs(x="Longitude", y="Latitude", title='March Distribution Model') 
  
)

test_spdf <- as(m.may, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Degrees", "x", "y")
(mapp_may = ggplot(data=test_df) +  
    geom_tile(aes(x=x, y=y, fill=Degrees)) + 
    coord_quickmap() + 
    theme_minimal() + 
    scale_fill_gradient2(low = "darkblue",
                         mid = "white",
                         high = "darkred",
                         na.value='black', 
                         limits=c(0,1)) +
    theme(panel.background = element_rect(fill='black')) +
    labs(x="Longitude", y="Latitude", title='May Distribution Model') 
  
)

#plot change
test_spdf <- as(m.may-m, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Degrees", "x", "y")
(mapp_change = ggplot(data=test_df) +  
    geom_tile(aes(x=x, y=y, fill=Degrees)) + 
    coord_quickmap() + 
    theme_minimal() + 
    scale_fill_gradient2(low = "darkblue",
                         mid = "white",
                         high = "darkred",
                         na.value='black', 
                         limits=c(-1,1)) +
    theme(panel.background = element_rect(fill='black')) +
    labs(x="Longitude", y="Latitude", title='Relative Predicted Change (March - May)') 
  
)

(mapfig = plot_grid(mapp_march, mapp_may, mapp_change, nrow=3, ncol=1))

ggsave(mapfig, file ='map_figure.png', height=9, width = 5, dpi=600)

#plot tristate change
necorr = extent(c(-78, -70, 38, 44))
plot(m, ext=necorr)


# SDM of Humans in the US

# Project NYC




