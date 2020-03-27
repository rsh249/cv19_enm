library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(stringr)

# load usafacts data
cv_dat = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv')

#get geodata for US municipalities
download.file('https://download.geonames.org/export/dump/US.zip', 'data/US.zip')
system('unzip data/US.zip -d data')

US = data.table::fread('data/US.txt')
colnames(US)[2] = 'County.Name'
colnames(US)[11] = 'State'
head(US)

#join covid19 case records and geocoding by county name and state
cv_dat %>% 
  left_join(US, by=c('County.Name', 'State')) %>%
  select(countyFIPS, County.Name, State, stateFIPS, V5, V6, grep('X', colnames(cv_dat), value=T)) %>%
  melt(id.vars=c('County.Name', 'countyFIPS', 'State', 'stateFIPS', 'V5', 'V6'), 
       measure.vars = grep('X', colnames(cv_dat), value=T)
  ) %>% 
  mutate(variable=str_replace(variable, "X", "")) %>%
  mutate(variable=as.Date(variable, "%m.%d.%y")) 

# get climate data
wc2_dl = c('wc2.1_10m_tmin.zip',
           'wc2.1_10m_tavg.zip',
           'wc2.1_10m_srad.zip',
           'wc2.1_10m_wind.zip',
           'wc2.1_10m_vapr.zip',
           'wc2.1_10m_prec.zip')
for(i in wc2_dl){
  download.file(paste('http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/', i, sep=''), 
                paste('data/', i, sep=''))
}

files = list.files(pattern='wc', 'data/', full.names=T)
system(paste('unzip data/*.zip -d data')


