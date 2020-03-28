source('R/nyt_county.R')



# get climate data for each county and plot weighted by popsize
# null model: is climate a factor in the US distribution

# validate SDM today (March)
occ = cv_new %>%
  filter(date == last_day) %>%
  group_by(county, V5, V6) %>% 
  expand(count = seq(1:cases))

bg = rad_bg(as.data.frame(unique(cv_ex[,c('V6', 'V5')])), march_clim, radius = 200, n=20)

fc = c("L", "LQ")
set.eval = ENMevaluate(
  occ[,c('V6', 'V5')],
  march_clim,
  rasterPreds = TRUE,
  parallel = TRUE,
  fc = fc,
  numCores = 12,
  method = 'block',
  bg.coords = bg[,c('lon', 'lat')],
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
m = predict(march_clim, best_mod, clamp=T, type = 'cloglog')
p.test = raster::extract(m, set.eval@occ.pts)
ab.test = raster::extract(m, set.eval@bg.pts)
e.sub = evaluate(p.test, ab.test,
                 best_mod)
th.sub = threshold(e.sub)
thr = m > th.sub$kappa
#  plot(thr, col = c('black', 'blue'), main = 'no thin')
bin.occ = raster::extract(thr, set.eval@occ.pts)
bin.abs = raster::extract(thr, set.eval@bg.pts)
ev.set.bin <- evaluate(bin.occ, bin.abs)


# project SDM into Tristate for May/June
may_clim = cl_stack[[grep("05", names(cl_stack))]]
#may_clim = may_clim[[-grep("srad", names(may_clim))]]
r2 <- crop(may_clim, extent(SPDF))
may_clim <- mask(r2, SPDF)
names(may_clim) = names(march_clim)
m.may = predict(may_clim, best_mod, clamp=T, type = 'cloglog')
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
plot_grid(mapp_march, mapp_may, mapp_change, nrow=3, ncol=1)


#plot tristate change
necorr = extent(c(-78, -70, 38, 44))
plot(m, ext=necorr)


# SDM of Humans in the US

# Project NYC




