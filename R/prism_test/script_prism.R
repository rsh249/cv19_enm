library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(stringr)
library(raster)
library(tidycensus)

# load usafacts data
cv_dat = read.csv('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv')

#get geodata for US municipalities
if(file.exists("data/US.zip")){} else{
  download.file('https://download.geonames.org/export/dump/US.zip', 'data/US.zip')
  system('unzip data/US.zip -d data')
}
US = data.table::fread('data/US.txt')
colnames(US)[2] = 'County.Name'
colnames(US)[11] = 'State'
head(US)

# get population data w/tidycensus
census_api_key('3eef6660d69eefaca172cd41c483f746ecd6c287', overwrite = T, install = T)
readRenviron("~/.Renviron")
#v18 <- load_variables(2018, "acs5", cache = TRUE)
popCounty<- get_decennial(geography = "county", year = 2010, 
                             variables = "P001001")  %>%  
    mutate(State=unlist(lapply(strsplit(NAME,", "),function(x) x[2])),
                        County.Name=gsub(",.*","",NAME)) %>%
    mutate(State = ifelse(State %in% state.name,
                                 state.abb[match(State, state.name)],
                                 State))




#join covid19 case records and geocoding by county name and state
cv_new = cv_dat %>% 
  left_join(US, by=c('County.Name', 'State')) %>%
  left_join(popCounty, by = c('County.Name', 'State')) %>%
  dplyr::select(countyFIPS, County.Name, State, stateFIPS, V5, V6, variable, value, grep('X', colnames(cv_dat), value=T)) %>%
  mutate(cvar = variable) %>%
  mutate(pop = value) %>%
  melt(id.vars=c('County.Name', 'countyFIPS', 'State', 'stateFIPS', 'V5', 'V6', 'cvar', 'pop'), 
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
#plot
ggplot(cv_ex) + 
  geom_point(aes(x=wc2.1_10m_tavg_03, y=value/pop)) +
  theme_minimal()


