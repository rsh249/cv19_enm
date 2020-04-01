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
#last_day = last(cv_dat$date)
last_day = as.Date("2020-03-30")

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
  mutate(county = replace(county, county=="Kings" & state=="NY", "New York City")) %>%
  mutate(county = replace(county, county=="Clay" & state=="MO", "Kansas City")) %>%
  mutate(county = replace(county, county=="Cass" & state=="MO", "Kansas City")) %>%
  mutate(county = replace(county, county=="Jackson" & state=="MO", "Kansas City")) %>%
  mutate(county = replace(county, county=="Platte" & state=="MO", "Kansas City")) %>%
  group_by(county, state) %>%
  summarize(V5=mean(V5), V6=mean(V6)) %>%
  mutate(county = str_replace(county, " County", "")) %>%
  distinct()


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
  mutate(county = replace(county, county=="Clay" & state=="MO", "Kansas City")) %>%
  mutate(county = replace(county, county=="Cass" & state=="MO", "Kansas City")) %>%
  mutate(county = replace(county, county=="Jackson" & state=="MO", "Kansas City")) %>%
  mutate(county = replace(county, county=="Platte" & state=="MO", "Kansas City")) %>%
  group_by(county, state, variable) %>%
  summarize(value = sum(value)) %>%
  distinct()



#join covid19 case records and geocoding by county name and state
cv_new = cv_dat  %>%
  mutate(county = str_replace(county, " city", "")) %>%
  group_by(county, state, date)%>%
  summarize(cases = sum(cases), deaths = sum(deaths)) %>%
  inner_join(US, by=c('county', 'state')) %>%
  inner_join(popCounty, by = c('county', 'state')) %>%
  dplyr::select(date, county, state, cases, deaths, variable, value, V5, V6 ) %>%
  mutate(cvar = variable) %>%
  mutate(pop = value) %>% 
  dplyr::select(date, county, state, cases, deaths, cvar, pop, V5, V6 ) %>%
  mutate(county = replace(county, county=='St. Louis city', 'St. Louis')) %>%
  group_by(county, state, date) %>%
  slice(n())

t1 = cv_dat %>% filter(date == last_day) 
t2 = cv_new %>% filter(date == last_day) 
sum(t1$cases) #sanity check: Is this number today's number?
sum(t2$cases) #sanity check: Is this number today's number?
cv_dropout = cv_new %>%
  filter(is.na(V5)) %>%
  filter(is.na(V6)) #if this is not nrow() == 0 then inspect
cv_new = cv_new %>%
  filter(!is.na(V5)) %>%
  filter(!is.na(V6))


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
cv_ex = cbind(as.data.frame(cv_new), as.data.frame(cv_fin))

#plot scaling of cases/pop and pop/sum(pop)
ggplot(data=cv_new %>% filter(date==last_day)) +
  geom_point(aes(x=pop, y=(cases/pop)/sum(cases/pop, na.rm=T))) 

(pop_raw = ggplot(data=cv_new %>% filter(date==last_day)) +
  geom_point(aes(x=pop/1000000, y=cases, alpha=0.2)) +
  scale_y_log10() + 
  theme_minimal() +
  theme(legend.position = 'none') +
  xlab('Population (millions)') +
  ylab('Cases') +
  ggtitle("Case Count and Locality Population")
)

(pop_scale = ggplot(data=cv_new %>% filter(date==last_day)) +
    geom_point(aes(x=pop/1000000, y=(cases/pop), alpha=0.2)) +
    theme_minimal() +
    theme(legend.position = 'none') +
    xlab('Population (millions)') +
    ylab('Cases / Total Population') +
    ggtitle("Case Rate (per capita) and Locality Population")
)

pop_map = plot_grid(pop_raw, pop_scale, nrow=1, ncol=2, labels='AUTO')
ggsave(pop_map, file='cases_v_pop.png')
ggsave(pop_map, file='cases_v_pop.pdf')

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
  ggtitle(last_day)



a2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle(last_day - 14)


a3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle(last_day - 28)

b1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmin_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmin_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temperature (C)') +
  ylab('Density')

b2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmin_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmin_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temperature (C)') +
  ylab('Density')

b3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
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

bb3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
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

c3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
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

d3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_vapr_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_vapr_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')

cp = plot_grid(a3, a2, a1, bb3, bb2, bb1, b3, b2, b1, c3, c2, c1, d3, d2, d1, ncol=3, nrow=5, labels="AUTO")

ggsave(cp, file='compare_2wk.png', height=9, width=9, dpi=600)
ggsave(cp, file='compare_2wk.pdf', height=9, width=9, dpi=600)

# make unweighted figures
#plot
ua1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tavg_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tavg_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle(last_day)



ua2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tavg_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tavg_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle(last_day - 14)


ua3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tavg_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tavg_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density') + 
  ggtitle(last_day - 28)

ub1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmin_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmin_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temperature (C)') +
  ylab('Density')

ub2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmin_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmin_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temperature (C)') +
  ylab('Density')

ub3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmin_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmin_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Min Temperature (C)') +
  ylab('Density')

ubb1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmax_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmax_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Max Temparature (C)') +
  ylab('Density')



ubb2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmax_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmax_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Max Temperature (C)') +
  ylab('Density')

ubb3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_tmax_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_tmax_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Max Temperature (C)') +
  ylab('Density')


uc1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_srad_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_srad_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Solar Radiation') +
  ylab('Density')



uc2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_srad_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_srad_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Solar Radiation') +
  ylab('Density')

uc3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_srad_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_srad_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Solar Radiation') +
  ylab('Density')

ud1 = ggplot(cv_ex %>% filter(date == last_day) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_vapr_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_vapr_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')



ud2 = ggplot(cv_ex %>% filter(date == last_day - 14) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_vapr_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_vapr_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')

ud3 = ggplot(cv_ex %>% filter(date == last_day - 28) %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_2.5m_vapr_03), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex2, aes(x=wc2.1_2.5m_vapr_03), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Water Vapor Pressure (kPa)') +
  ylab('Density')

ucp = plot_grid(ua3, ua2, ua1, ubb3, ubb2, ubb1, ub3, ub2, ub1, uc3, uc2, uc1, ud3, ud2, ud1, ncol=3, nrow=5, labels="AUTO")

ggsave(ucp, file='compare_2wk_notweighted.png', height=9, width=9, dpi=600)
ggsave(ucp, file='compare_2wk_notweighted.pdf', height=9, width=9, dpi=600)


