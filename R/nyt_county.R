library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(stringr)
library(raster)
library(tidycensus)
library(cowplot)



# load NYT County data
cv_dat = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  mutate(state = ifelse(state %in% state.name,
                        state.abb[match(state, state.name)],
                        state)) %>%
  mutate(date = as.Date(date))
last_day = last(cv_dat$date)
#get geodata for US municipalities
if(file.exists("data/US.zip")){} else{
  download.file('https://download.geonames.org/export/dump/US.zip', 'data/US.zip')
  system('unzip data/US.zip -d data')
}
US = data.table::fread('data/US.txt')
colnames(US)[2] = 'county'
colnames(US)[11] = 'state'
US = US %>%
  mutate(county = replace(county, county=="Queens" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="Bronx" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="New" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="Richmond" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="Kings" & state=="NY", "New York City")) 

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
                        state)) %>%
  mutate(county = replace(county, county=="Queens" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="Bronx" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="New" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="Richmond" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="Kings" & state=="NY", "New York City")) %>%
  group_by(county, state, variable) %>%
  summarize(value = sum(value))



#join covid19 case records and geocoding by county name and state
cv_new = cv_dat %>% 
  left_join(US, by=c('county', 'state')) %>%
  left_join(popCounty, by = c('county', 'state')) %>%
  dplyr::select(date, county, state, fips, cases, deaths, variable, value, V5, V6 ) %>%
  mutate(cvar = variable) %>%
  mutate(pop = value) %>% 
  dplyr::select(date, county, state, fips, cases, deaths, cvar, pop, V5, V6 )
  




# get climate data

wc2_dl = c('wc2.1_2.5m_tmin.zip',
           'wc2.1_2.5m_tmax.zip',
           'wc2.1_2.5m_tavg.zip',
           'wc2.1_2.5m_srad.zip',
           'wc2.1_2.5m_wind.zip',
           'wc2.1_2.5m_vapr.zip',
           'wc2.1_2.5m_prec.zip')

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
cl_stack = crop(cl_stack, extent(c(-130, -60, 20, 55)))

march_clim = cl_stack[[grep("03", names(cl_stack))]]
#march_clim = march_clim[[-grep("wind", names(march_clim))]]

cv_new = cv_new %>%
  filter(!is.na(V5)) %>%
  filter(!is.na(V6))
cv_ex = raster::extract(march_clim, 
                        cv_new[,c('V6', 'V5')], 
                        buffer= 5000)
cv_fin = apply(cv_ex[[1]], 2, mean)
for(i in 2:length(cv_ex)){
  if(length(cv_ex[[i]])>1){
    cv_fin=rbind(cv_fin, apply(cv_ex[[i]], 2, mean))
  } else {
    cv_fin=rbind(cv_fin, rep(NA, nlayers(march_clim)))
  }
}
cv_ex = cbind(cv_new, cv_fin)

all = popCounty %>%
  left_join(US, by=c('county', 'state')) %>%
  mutate(pop=value)
all_ex = raster::extract(march_clim, all[,c('V6', 'V5')], buffer=5000)
all_fin = cv_fin[0,]
for(i in 1:length(all_ex)){
  if(length(all_ex[[i]])>1){
    all_fin=rbind(all_fin, apply(all_ex[[i]], 2, mean))
  } else {
    all_fin=rbind(all_fin, rep(NA, ncol(all_fin)))
  }
}
all_fin=as.data.frame(all_fin)
all_ex2 = cbind(as.data.frame(all), all_fin)

#plot
a1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle('March 25')



a2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle('March 11')

b1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmin_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmin_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temparature (C)') +
  ylab('Density')



b2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmin_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmin_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temperature (C)') +
  ylab('Density')

bb1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmax_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmax_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Max Temparature (C)') +
  ylab('Density')



bb2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmax_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmax_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Max Temperature (C)') +
  ylab('Density')



c1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_srad_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_srad_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Solar Radiation') +
  ylab('Density')



c2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_srad_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_srad_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Solar Radiation') +
  ylab('Density')


d1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_vapr_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_vapr_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')



d2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_vapr_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_vapr_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')

cp = plot_grid(a2, a1, bb2, bb1, b2, b1, c2, c1, d2, d1, ncol=2, nrow=5, label="AUTO")

ggsave(cp, file='compare_2wk.png', height=12, width=9, dpi=600)

