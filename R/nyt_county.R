library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(stringr)
library(raster)
library(tidycensus)
library(ENMeval)
library(cowplot)


# load NYT County data
cv_dat = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  mutate(state = ifelse(state %in% state.name,
                        state.abb[match(state, state.name)],
                        state))

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

march_clim = cl_stack[[grep("03", names(cl_stack))]]
cv_ex = extract(march_clim, cv_new[,c('V6', 'V5')], method='bilinear' )
cv_ex = cbind(cv_new, cv_ex)

all = popCounty %>%
  left_join(US, by=c('county', 'state')) %>%
  mutate(pop=value)
all_ex = extract(march_clim, all[,c('V6', 'V5')], method='bilinear')
all_ex = cbind(all, all_ex)
#plot
a1 = ggplot(cv_ex %>% filter(date == '2020-03-25') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density')



a2 = ggplot(cv_ex %>% filter(date == '2020-03-11') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tavg_03, weight=(cases/pop)/sum(cases/pop)), colour='darkred', fill='darkred', alpha=0.1)+
  geom_density(data=all_ex, aes(x=wc2.1_10m_tavg_03, weight=pop/sum(pop)), colour='darkblue', fill='darkblue', alpha=0.1) +
  theme_minimal() + 
  xlab('Average Temperature (C)') +
  ylab('Density')

b1 = ggplot(cv_ex %>% filter(date == '2020-03-25') %>% filter(!is.na(pop))) +
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


c1 = ggplot(cv_ex %>% filter(date == '2020-03-25') %>% filter(!is.na(pop))) +
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


cp = plot_grid(a1,a2,b1, b2, c1, c2, ncol=2, nrow=3, label="AUTO")

ggsave(cp, file='compare_2wk.png', height=12, width=9, dpi=600)

ggplot(cv_ex %>% filter(date == '2020-03-12') %>% filter(!is.na(pop))) +
  geom_density(aes(x=wc2.1_10m_tmin_03, weight=(cases/pop)), colour='red') + 
  geom_density(data=all_ex, aes(x=wc2.1_10m_tmin_03))


# get climate data for each county and plot weighted by popsize
# null model: is climate a factor in the US distribution

# validate SDM today (March)
# project SDM into Tristate for May/June

# SDM of Humans in the US

# Project NYC




