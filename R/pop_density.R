# create population density predictor variable
library(tidycensus)
library(rasterExtras)
library(tidyverse)
library(raster)
library(maptools)
# get population data w/tidycensus
census_api_key('3eef6660d69eefaca172cd41c483f746ecd6c287', overwrite = T, install = T)
readRenviron("~/.Renviron")

popCounty<- get_decennial(geography='county', year = 2010, 
                          variables = "P001001")  %>%  
  mutate(state=unlist(lapply(strsplit(NAME,", "),function(x) x[2])),
         county=gsub(",.*","",NAME)) %>%
  mutate(county=unlist(lapply(strsplit(county," "),function(x) x[1]))) %>%
  mutate(state = ifelse(state %in% state.name,
                        state.abb[match(state, state.name)],
                        state))

US = data.table::fread('data/US.txt')
colnames(US)[2] = 'county'
colnames(US)[11] = 'state'
head(US)

county_pop = US %>%
  left_join(popCounty, by = c('county', 'state')) %>%
  filter(!is.na(value))

occ = county_pop %>%
  group_by(county, V5, V6) %>% 
  expand(count = seq(1:(value/1000)))

r = raster::raster('data/wc2.1_10m_prec_01.tif')
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="United States")
## crop and mask
r <- raster::crop(r, raster::extent(SPDF))
r <- raster::mask(r, SPDF)

geo = gkde(grid=r, 
           occ[,c('V6', 'V5')], 
           parallel=T, 
           nclus=16, 
           maxram=128, 
           dist.method='Haversine')

writeRaster(geo, filename = 'data/popden')

         