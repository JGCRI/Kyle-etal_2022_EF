
source("R/proj_load.R")
source("R/landuse.R")
source("R/nutbal.R")
source("R/water.R")
source("R/ghg_emissions.R")
source("R/ag_an_markets.R")
source("R/food_afford_refdiet.R")

gyrvals=c('#2f6c20','#f2ef54','#c94711')
rygvals=c('#c94711','#f2ef54','#2f6c20')


region_vals <- Nsur %>%
  bind_rows(Psur) %>%
  bind_rows(forest_plot) %>%
  bind_rows(ghg_totals) %>%
  bind_rows(exportpct) %>%
  bind_rows(AgLabProd) %>%
  bind_rows(food_afford) %>%
  bind_rows(Hindex) %>%
  bind_rows(watbal_region) %>%
  left_join(GCAMreg, by = "region")%>%
  mutate(variable=recode(variable,
                         "forest_frac"="LCC",
                         "Emissions"="GHG",
                         "AgLabProd"="AGDP",
                         "Export_pct"="TROP",
                         "Hindex"="RSH",
                         "Foodafford"="RSE"))
write_csv(region_vals,'outdata/region_vals.csv')

region_vals <- read_csv('outdata/region_vals.csv')
SAMthresh <- read_csv('outdata/SAM_thresholds.csv')


# Uncomment to plot scaled data

# region_vals %>%
#   select(-Unit) %>%
#   group_by(variable) %>%
#   summarize(min=quantile(value,na.rm=T,probs = 0.1),
#             max=quantile(value,na.rm=T,probs = 0.9)) %>%
#   left_join(region_vals, by = "variable") %>%
#   mutate(value_scaled=(value-min)/(max-min)*100) %>%
#   mutate(value_scaled=if_else(value_scaled>0, if_else(value_scaled>100,100,value_scaled), 0)) %>%
#   select(-value) %>%  rename(value = value_scaled) -> region_vals

SAMthresh%>%
  right_join(region_vals, by = "variable") %>%
  mutate(value=value*trans,
         green=green*trans,
         red=red*trans)  ->region_thresh


#regions <- st_read('inputs/regions_simpl.shp')
regions <- st_read('inputs/shapefiles/region_boundaries_moirai_landcells_3p1_0p5arcmin.shp')
# st_is_valid(regions) # check if shapefile is correct



GCAMreg %>% rename("GCAM_30_re"=GCAM_region_ID) %>%
  left_join(region_thresh) %>%
  #mutate(scenario_long=recode(scenario, PotYld_Diet="Combination", Reference45="Emissions reduction", PotYld="Potential Yield", DietaryTransition="Dietary Transition")) %>%
  mutate(scenario_long=recode(scenario,
                              "REF"="Reference",
                              "YLD"="Yield Intensification",
                              "FLX"="Flexitarian Diet",
                              "CO2"="CO2 Emissions Price",
                              "FLX_CO2"="Flexitarian Diet & \nCO2 Emissions Price",
                              "YLD_CO2"="Yield Intensification & \nCO2 Emissions Price",
                              "YLD_FLX"="Yield Intensification & \nFlexitarian Diet",
                              "YLD_FLX_CO2"="Combination",
  )) %>%
  as.data.frame() -> regions_data

region_shape <- merge(regions, regions_data)

negvals<-region_shape %>% filter(variable %in% c("TROP","RSE","AGDP")) %>%
  mutate(value=-value)


##
# share of top 3 commodities -- Xin requested this as possible alternative to H-index
##
GCAMreg %>% rename("GCAM_30_re"=GCAM_region_ID) %>%
  left_join(share_top3) %>%
  mutate(scenario_long=recode(scenario,
                              "REF"="Reference",
                              "YLD"="Yield Intensification",
                              "FLX"="Flexitarian Diet",
                              "CO2"="CO2 Emissions Price",
                              "FLX_CO2"="Flexitarian Diet & \nCO2 Emissions Price",
                              "YLD_CO2"="Yield Intensification & \nCO2 Emissions Price",
                              "YLD_FLX"="Yield Intensification & \nFlexitarian Diet",
                              "YLD_FLX_CO2"="Combination",
  )) %>%
  #mutate(scenario_long=recode(scenario, PotYld_Diet="Combination", Reference45="Emissions reduction", PotYld="Potential Yield", DietaryTransition="Dietary Transition")) %>%
  as.data.frame() -> share_plot

share_shape<-merge(regions, share_plot)




#plotdf<-share_shape %>% filter(scenario==scen,year==2050)
#tm_shape(plotdf)+
#  tm_fill("value",title="TOP",palette=rygvals)

for(scen in unique(region_vals$scenario)){
  plotdf<-share_shape %>% filter(scenario==scen,year==2050)
  scenario_name<-first(plotdf$scenario_long)
  map<-tm_shape(plotdf)+
    tm_fill("value",title="TOP",palette=rygvals)+
    tm_layout(title=scenario_name)
  filename=paste("2050_TOP",scen,sep='_')
  tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
}

##
# fast version with grouped variables
##

indicators <- unique(region_vals$variable)

#for(vari in c("Nsur","Psur","LCC","GHG","RSE")){
for(vari in indicators){
  for(scen in listScenarios(SAMout)){
    for(years in c(2020,2050)){
    plot<-region_shape %>% filter(year==years,variable==vari,scenario==scen)
    scenario_name<-first(plot$scenario_long)
    map<-tm_shape(plot)+
      tm_polygons("value",title=vari,palette=rygvals)+
      tm_layout(title=scen)
    filename=paste(years,vari,scenario_name,sep='_')
    tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
  }}}

# proper maps
for(vari in c("Nsur")){
  for(scen in listScenarios(SAMout)[1]){
    for(years in c(2050)){
      plot<-region_shape %>% filter(year==years,variable==vari,scenario==scen) #%>%
        #mutate(value=100*value)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=rygvals,breaks=c(-Inf,0,20,40,60,80,100,Inf))+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }}}

for(vari in c("Psur")){
  for(scen in listScenarios(SAMout)){
    for(years in c(2050)){
      plot<-region_shape %>% filter(year==years,variable==vari,scenario==scen)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=rygvals,breaks=c(-Inf,0,5,10,15,20,25,Inf))+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }}}

for(vari in c("LCC")){
  for(scen in listScenarios(SAMout)){
    for(years in c(2050)){
      plot<-region_shape %>% filter(year==years,variable==vari,scenario==scen)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=rygvals,breaks=c(-Inf,0,0.01,0.02,0.03,0.04,Inf))+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }}}

for(vari in c("GHG")){
  for(scen in listScenarios(SAMout)){
    for(years in c(2050)){
      plot<-region_shape %>% filter(year==years,variable==vari,scenario==scen)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=rygvals,breaks=c(0,2,4,6,8,10,Inf))+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }}}

for(vari in c("RSH")){
  for(scen in listScenarios(SAMout)){
    for(years in c(2050)){
      plot<-region_shape %>% filter(year==years,variable==vari,scenario==scen)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=gyrvals,breaks=c(0,1,1.5,2,2.5,3))+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }}}



for(vari in c("TROP")){
  for(scen in listScenarios(SAMout)){
    for(years in c(2050)){
      plot<-negvals %>% filter(year==years,variable==vari,scenario==scen)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=gyrvals)+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }
  }
}


for(vari in c("AGDP")){
  for(scen in listScenarios(SAMout)){
    for(years in c(2050)){
      plot<-negvals %>% filter(year==years,variable==vari,scenario==scen)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=gyrvals,breaks=c(0,1000,5000,10000,100000,Inf))+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }
  }
}


for(vari in c("RSE")){
  for(scen in listScenarios(SAMout)){
    for(years in c(2050)){
      plot<-negvals %>% filter(year==years,variable==vari,scenario==scen)
      scenario_name<-first(plot$scenario_long)
      map<-tm_shape(plot)+
        tm_polygons("value",title=vari,palette=gyrvals,breaks=c(0,1,2,3,Inf))+
        tm_layout(title=scenario_name)
      filename=paste(years,vari,scen,sep='_')
      tmap_save(map,filename = paste0('./figures/maps_new/',filename,'.png'))
    }
  }
}
