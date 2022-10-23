#####
# maps by basin
#####
source("R/vals_basin.R")

gyrvals=c('#2f6c20','#f2ef54','#c94711')
rygvals=c('#c94711','#f2ef54','#2f6c20')

basins<-st_read('inputs/basins_simpl.shp')
greenland<-basins<-st_read('inputs/greenland.shp')

basin_vals<-read_csv('outdata/basin_vals.csv') %>%
  mutate(scenario_long=recode(scenario,
                              PotYld_Diet="Combination",
                              Reference45="Emissions reduction",
                              PotYld="Potential Yield",
                              DietaryTransition="Dietary Transition"))



basin_vals %>%
  rename("GLU"="GCAM_basin_ID")%>% filter(year==2050,variable=="Nsur")->Nsur_shape
merge(basins,Nsur_shape)->Nsur_basin

basin_vals %>% rename("GLU"="GCAM_basin_ID")%>% filter(year==2050,variable=="Psur")->Psur_shape
merge(basins,Psur_shape)->Psur_basin

basin_vals %>% rename("GLU"="GCAM_basin_ID")%>% filter(year==2050,variable=="LCC")->LCC_shape
merge(basins,LCC_shape)->LCC_basin

basin_vals %>% rename("GLU"="GCAM_basin_ID")%>% filter(year==2050,variable=="SUSI")->SUSI_shape
merge(basins,SUSI_shape)->SUSI_basin

#
# st_write(Nsur_basin,"./outdata/Nsur_basin.shp")
# st_write(Psur_basin,"./outdata/Psur_basin.shp")
# st_write(LCC_basin,"./outdata/LCC_basin.shp")
# st_write(SUSI_basin,"./outdata/SUSI_basin.shp")

# bounds
# variable    val90       val10
# * <chr>       <dbl>       <dbl>
#   1 GHG       12.8      0.0230
# 2 LCC        0.0441  -0.0000712
# 3 Nsur     136.     -70.7
# 4 Psur      20.7     -1.68
# 5 SUSI       1.14     0.00742

#Nsur
for(scen in unique(basin_vals$scenario)){
  plotdf<-Nsur_basin %>% filter(scenario==scen)
  scenario_name=first(plotdf$scenario_long)
  map<-tm_shape(plotdf)+
    tm_polygons("value",title="Nsur",palette=gyrvals,
                breaks=c(-Inf,0,25,50,75,100,125,150,175,200,Inf))+
    tm_shape(greenland)+tm_polygons(col=NA)+
    tm_layout(title=scenario_name)
  filename=paste("2050_Nsur",scen,sep='-')
  tmap_save(map,filename = paste0('./figures/maps/basin_',filename,'.png'))
}



#Psur
for(scen in listScenarios(SAMout)){
  plotdf<-Psur_basin[Psur_basin$scenario==scen,]
  scenario_name=first(plotdf$scenario_long)
  map<-tm_shape(plotdf)+
    tm_polygons("value",title="Psur",palette=gyrvals,breaks=c(-Inf,0,5,10,15,20,Inf))+
    tm_shape(greenland)+tm_polygons(col=NA)+
    tm_layout(title=scenario_name)
  filename=paste("2050_Psur",scen,sep='-')
  tmap_save(map,filename = paste0('./figures/mapsnew/basin_',filename,'.png'))
}


#LCC
for(scen in listScenarios(SAMout)){
  plotdf<-LCC_basin[LCC_basin$scenario==scen,]
  scenario_name=first(plotdf$scenario_long)
  map<-tm_shape(plotdf)+
    tm_polygons("value",title="LCC",palette=gyrvals,
                breaks=c(-Inf,0,0.01,0.02,0.03,0.04,0.05,Inf))+
    tm_shape(greenland)+tm_polygons(col=NA)+
    tm_layout(title=scenario_name)
  filename=paste("2050_LCC",scen,sep='-')
  tmap_save(map,filename = paste0('./figures/maps/basin_',filename,'.png'))
}

#SUSI
for(scen in listScenarios(SAMout)){
  plotdf<-SUSI_basin[SUSI_basin$scenario==scen,]
  scenario_name<-first(plotdf$scenario_long)
  map<-tm_shape(plotdf)+
    tm_polygons("value",title="SUSI",palette=gyrvals,breaks=c(0,1,2,3,Inf))+
    tm_shape(greenland)+tm_polygons(col=NA)+
    tm_layout(title=scenario_name)
  filename=paste("2050_SUSI",scen,sep='-')
  tmap_save(map,filename = paste0('./figures/mapsnew/basin_',filename,'.png'))
}

#GHG
for(scen in listScenarios(SAMout)){
  plotdf<-GHG_basin[GHG_basin$scenario==scen,]
  scenario_name=first(plotdf$scenario_long)
  map<-tm_shape(plotdf)+
    tm_polygons("value",title="GHG",palette=gyrvals,breaks=c(0,2,4,6,8,10,12,Inf))+
    tm_shape(greenland)+tm_polygons(col=NA)+
    tm_layout(title=scenario_name)
  filename=paste(years,vari,scen,sep='-')
  tmap_save(map,filename = paste0('./figures/mapsnew/basin_',filename,'.png'))
}

#Nsur by total basin area
Nsur_totalarea %>%
  left_join(basin_glu%>% select(GLU_name,GCAM_basin_ID),by = "GLU_name") %>%
  mutate(scenario_long=recode(scenario,
                              PotYld_Diet="Combination",
                              Reference45="Emissions reduction",
                              PotYld="Potential Yield",
                              DietaryTransition="Dietary Transition")) %>%
  rename("GLU"="GCAM_basin_ID")%>%
  select(scenario,GLU,GLU_name,year,Nsur_totha,scenario_long) %>%
  filter(year==2050)->Nsur_totarea
merge(basins,Nsur_totarea)->Nsur_totbasin

for(scen in unique(basin_vals$scenario)){
  plotdf<-Nsur_totbasin %>% filter(scenario==scen)
  scenario_name=first(plotdf$scenario_long)
  map<-tm_shape(plotdf)+
    tm_polygons("Nsur_totha",title="Nsur-total area",palette=gyrvals,
                breaks=c(-Inf,0,1,2,3,4,5,Inf))+
    tm_shape(greenland)+tm_polygons(col=NA)+
    tm_layout(title=scenario_name)
  filename=paste("2050_Nsur_totarea",scen,sep='-')
  tmap_save(map,filename = paste0('./figures/maps/basin_',filename,'.png'))
}
