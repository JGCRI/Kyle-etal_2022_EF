# Load every library that is used by any R code files in this module
library(assertthat)
library(dplyr)
library(egg)
library(fmsb)
library(forcats)
library(ggplot2)
library(readr)
library(readxl)
library(rgcam)
library(sf)
library(stringr)
library(tidyr)
#library(tmap)

#SAMout<-prj

SAMout <- loadProject(proj='./outdata/SAM-matrix.dat') # new runs 8 scenarios
#SAMout_v0<-loadProject(proj='./outdata/SAM-matrix_v0.dat') # old runs 5 scenarios

#gcam_home='~/git/gcam-sam/'
#gcam_home = "/Users/D3P747/Desktop/stash/sam/"
gcam_home = "C:/Users/niaz981/Downloads/Modelling/GCAM/gcam-umd-stash-LnDfund/"

###FILTERS###
allcrops =c('Corn','FiberCrop','Fruits', 'Legumes','MiscCrop','NutsSeeds','OilCrop','OtherGrain','OilPalm','Rice','RootTuber',
           'Soybean','SugarCrop','Vegetables','Wheat','FodderGrass','FodderHerb','biomass','biomassGrass', 'biomassTree')
cropsnobio =c('Corn','FiberCrop','Fruits', 'Legumes','MiscCrop','NutsSeeds','OilCrop','OtherGrain','OilPalm','Rice','RootTuber',
           'Soybean','SugarCrop','Vegetables','Wheat','FodderGrass','FodderHerb')
foodcrops =c('Corn','Fruits', 'Legumes','NutsSeeds','OilCrop','OtherGrain','OilPalm','Rice','RootTuber',
            'Soybean','SugarCrop','Vegetables','Wheat')

uncropped <-c('Forest','Grassland','OtherArableLand','ProtectedGrassland','ProtectedShrubland',
             'ProtectedUnmanagedForest','ProtectedUnmanagedPasture','Shrubland',
             'UnmanagedForest','UnmanagedPasture','UrbanLand','Tundra','RockIceDesert')
biomass = c('biomass_grass','biomass_tree')
fodder = c('FodderHerb','FodderGrass')
animals = c('Dairy','Beef','Poultry','Pork','SheepGoat','OtherMeat_Fish')
otheragsector <- c('Pasture','biomassGrass','biomassTree', "OtherArableLand", "ProtectedUnmanagedPasture", "UnmanagedPasture")
regional_ag <- c("regional beef" ,"regional corn", "regional dairy", "regional fibercrop" , "regional fruits",
               "regional legumes", "regional misccrop", "regional nuts_seeds", "regional oilcrop", "regional othergrain",
               "regional palmfruit","regional pork","regional poultry","regional rice" ,"regional root_tuber",
               "regional sheepgoat", "regional soybean", "regional sugarcrop", "regional vegetables", "regional wheat","regional biomass")

metaregions <- read_csv('inputs/metaregions.csv')
GCAMreg <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/common/GCAM_region_names.csv"),skip=6)
basin_glu <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/water/basin_to_country_mapping.csv"), skip=7)
iso_reg <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/common/iso_GCAM_regID.csv"),skip=6) %>%
  rename(country=country_name)


#modelfuture=seq(2020,2050,by=10)
modelfuture = seq(2020,2100,by=10)

gdp_deflator <- function(year, base_year) {
  # This time series is the BEA "A191RD3A086NBEA" product
  # Downloaded April 13, 2017 from https://fred.stlouisfed.org/series/A191RD3A086NBEA
  gdp_years <- 1929:2016
  gdp <- c(9.896, 9.535, 8.555, 7.553, 7.345, 7.749, 7.908, 8.001, 8.347,
           8.109, 8.033, 8.131, 8.68, 9.369, 9.795, 10.027, 10.288, 11.618,
           12.887, 13.605, 13.581, 13.745, 14.716, 14.972, 15.157, 15.298,
           15.559, 16.091, 16.625, 17.001, 17.237, 17.476, 17.669, 17.886,
           18.088, 18.366, 18.702, 19.227, 19.786, 20.627, 21.642, 22.784,
           23.941, 24.978, 26.337, 28.703, 31.361, 33.083, 35.135, 37.602,
           40.706, 44.377, 48.52, 51.53, 53.565, 55.466, 57.24, 58.395,
           59.885, 61.982, 64.392, 66.773, 68.996, 70.569, 72.248, 73.785,
           75.324, 76.699, 78.012, 78.859, 80.065, 81.887, 83.754, 85.039,
           86.735, 89.12, 91.988, 94.814, 97.337, 99.246, 100, 101.221,
           103.311, 105.214, 106.913, 108.828, 109.998, 111.445)
  names(gdp) <- gdp_years

  assert_that(all(year %in% gdp_years))
  assert_that(all(base_year %in% gdp_years))

  as.vector(unlist(gdp[as.character(year)] / gdp[as.character(base_year)]))
}

