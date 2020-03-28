library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(stringr)
library(raster)
library(tidycensus)
library(ENMeval)
library(cowplot)
library(maxnet)
library(cRacle)
library(rasterExtras)
library(usmap)


# load NYT County data
cv_dat = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  mutate(state = ifelse(state %in% state.name,
                        state.abb[match(state, state.name)],
                        state))

last_day = last(cv_dat$date)
#get geodata for US municipalities
if(file.exists("data/US.zip")){} else{
  download.file('https://download.geonames.org/export/dump/US.zip', 'data/US.zip')
  system('unzip data/US.zip -d data')
}
US = data.table::fread('data/US.txt')
colnames(US)[2] = 'county'
colnames(US)[11] = 'state'
head(US)

# get population data w/tidycensus
census_api_key('3eef6660d69eefaca172cd41c483f746ecd6c287', overwrite = T, install = T)
readRenviron("~/.Renviron")
#v18 <- load_variables(2018, "acs5", cache = TRUE)
popCounty<- get_decennial(geography = "county", year = 2010, 
                          variables = "P001001")  %>%  
  mutate(state=unlist(lapply(strsplit(NAME,", "),function(x) x[2])),
         county=gsub(",.*","",NAME)) %>%
  mutate(county=unlist(lapply(strsplit(county," "),function(x) x[1]))) %>%
  mutate(state = ifelse(state %in% state.name,
                        state.abb[match(state, state.name)],
                        state))




#join covid19 case records and geocoding by county name and state
cv_new = cv_dat %>% 
  left_join(US, by=c('county', 'state')) %>%
  left_join(popCounty, by = c('county', 'state')) %>%
  dplyr::select(date, county, state, fips, cases, deaths, variable, value, V5, V6 ) %>%
  mutate(cvar = variable) %>%
  mutate(pop = value) %>% 
  dplyr::select(date, county, state, fips, cases, deaths, cvar, pop, V5, V6 )
  




# get climate data

wc2_dl = c('wc2.1_10m_tmin.zip',
           'wc2.1_10m_tavg.zip',
           'wc2.1_10m_srad.zip',
           'wc2.1_10m_wind.zip',
           'wc2.1_10m_vapr.zip',
           'wc2.1_10m_prec.zip')

if(any(file.exists(paste('data/', wc2_dl, sep='')))){} else{
  for(i in wc2_dl) {
    download.file(
      paste(
        'http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/',
        i,
        sep = ''
      ),
      paste('data/', i, sep = '')
    )
  }
  
  files = list.files(pattern = 'wc', 'data/', full.names = T)
  
  for (i in files) {
    system(paste('unzip ', i, ' -d data', sep = ''))
  }
}
#read climate data
cl_stack = raster::stack(list.files('data', pattern='.tif', full.names = T))
cl_stack = crop(cl_stack, extent(c(-130, -40, 20, 55)))

march_clim = cl_stack[[grep("03", names(cl_stack))]]
march_clim = march_clim[[-grep("srad", names(march_clim))]]
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="United States")
## crop and mask
r2 <- crop(march_clim, extent(SPDF))
march_clim <- mask(r2, SPDF)

cv_ex = raster::extract(march_clim, cv_new[,c('V6', 'V5')], method='bilinear' )
cv_ex = cbind(cv_new, cv_ex)

all = popCounty %>%
  left_join(US, by=c('county', 'state')) %>%
  mutate(pop=value)
all_ex = raster::extract(march_clim, all[,c('V6', 'V5')], method='bilinear')
all_ex = cbind(all, all_ex)
#plot
a1 = ggplot(cv_ex %>% filter(date == '2020-03-25') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle('March 25')



a2 = ggplot(cv_ex %>% filter(date == '2020-03-11') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle('March 11')

b1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tmin_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_tmin_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temparature (C)') +
  ylab('Density')



b2 = ggplot(cv_ex %>% filter(date == '2020-03-11') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tmin_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_tmin_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temperature (C)') +
  ylab('Density')


c1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_srad_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_srad_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Solar Radiation') +
  ylab('Density')



c2 = ggplot(cv_ex %>% filter(date == '2020-03-11') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_srad_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_srad_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Solar Radiation') +
  ylab('Density')


d1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_vapr_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_vapr_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')



d2 = ggplot(cv_ex %>% filter(date == '2020-03-11') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_vapr_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_vapr_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')

cp = plot_grid(a2, a1, b2, b1, c2, c1, d2, d1, ncol=2, nrow=4, label="AUTO")

ggsave(cp, file='compare_2wk.png', height=12, width=9, dpi=600)


ggplot(cv_ex %>% filter(date == '2020-03-12') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tmin_03, weight=(cases/pop)), colour='red') + 
  geom_density(data=all_ex, aes(x=wc2.1_10m_tmin_03))


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
m = predict(march_clim, best_mod, type = 'cloglog')
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
may_clim = may_clim[[-grep("srad", names(may_clim))]]
r2 <- crop(may_clim, extent(SPDF))
may_clim <- mask(r2, SPDF)
names(may_clim) = names(march_clim)
m.may = predict(may_clim, best_mod, type = 'cloglog')
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

ny_march = crop(m, necorr)
test_spdf <- as(ny_march, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Degrees", "x", "y")
(mapp = ggplot(data=test_df) +  
  geom_tile(aes(x=x, y=y, fill=Degrees)) + 
  coord_quickmap() + theme_bw() + scale_fill_gsea() +
  labs(x="Longitude", y="Latitude", title='BIEN Vegetation Plots') +
  geom_point(data=cv_ex, 
             aes(x=V6, y=V5), 
             alpha=0.6, 
             pch = 20, 
             size=0.3) 
  
)

# SDM of Humans in the US

# Project NYC




