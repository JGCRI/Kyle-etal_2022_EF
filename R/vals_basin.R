
# Nutrient (P and N) content of crops by GCAM commodity ----
nutcont<-read_csv("inputs/NutCont_commod.csv")
# N fixation (kgN per kg yield)
nfix<-read_csv("inputs/Nfix_commod_region.csv") %>%
  select(GCAM_commodity,GCAM_region_ID,Nfixkg.yld) %>%
  filter(Nfixkg.yld>0)

land_detail<-getQuery(SAMout,"detailed land allocation")

land_detail %>%
  separate(landleaf,c('GCAM_commodity','GLU_name','irr','mgt')) %>%
  mutate(GCAM_commodity = sub("C4|Tree", "", GCAM_commodity)) -> land_det

land_det %>% filter(!is.na(irr))%>%
  group_by(scenario,GCAM_commodity,GLU_name,year) %>%
  summarize(value=sum(value))%>%
  rename("landthouskm2"=value) -> land_crop_basin
write_csv(land_crop_basin, 'outdata/land_crop_basin.csv')

# Alternatively, read from saved CSV file in the outdata folder
# land_crop_basin<- read_csv('outdata/land_crop_basin.csv')


Nfert_basin<-getQuery(SAMout,"fertilizer consumption by ag tech")

Nfert_basin %>%
  separate(subsector, c('GCAM_commodity','GLU_name')) %>%
  mutate(GCAM_commodity = sub("C4|Tree", "", GCAM_commodity)) %>%
  group_by(scenario,GCAM_commodity,GLU_name,year) %>%
  summarize(value=sum(value)) %>%
  rename("NfertMt"=value)->Nfert_crop_basin
write_csv(Nfert_crop_basin, 'outdata/Nfert_crop_basin.csv')

# Alternatively, read from saved CSV file in the outdata folder
#Nfert_crop_basin <- read_csv('outdata/Nfert_crop_basin.csv')


Pfert_basin<-getQuery(SAMout,"P fertilizer consumption by ag tech")

Pfert_basin %>%
  separate(subsector,c('GCAM_commodity','GLU_name')) %>%
  mutate(GCAM_commodity = sub("C4|Tree", "", GCAM_commodity)) %>%
  group_by(scenario,GCAM_commodity,GLU_name,year) %>%
  summarize(value=sum(value))%>%
  rename("PfertMt"=value)->Pfert_crop_basin
write_csv(Pfert_crop_basin, 'outdata/Pfert_crop_basin.csv')

# Alternatively, read from saved CSV file in the outdata folder
# Pfert_crop_basin<-read_csv('outdata/Pfert_crop_basin.csv')

cropprod_basin<-getQuery(SAMout,"ag production by subsector (land use region)")

cropprod_basin%>%
  filter(!output%in%c("Forest","Pasture")) %>%
  separate(subsector,c('GCAM_commodity','GLU_name')) %>%
  mutate(GCAM_commodity = sub("C4|Tree", "", GCAM_commodity)) %>%
  group_by(scenario,region,GCAM_commodity,GLU_name,year) %>%
  summarize(value=sum(value))->prod_crop_reg_basin
write_csv(prod_crop_reg_basin, 'outdata/prod_crop_reg_basin.csv')

# Alternatively, read from saved CSV file in the outdata folder
# prod_crop_reg_basin<-read_csv('outdata/prod_crop_reg_basin.csv')

# This /inputs/ data file contains scenario-specific information; needs to be re-generated in inputs/R/manure_fraction.R if new scenarios are used
nonfertN_basin<-read_csv('inputs/nonfertN_basin.csv')

prod_crop_reg_basin %>%
  group_by(scenario,GLU_name,GCAM_commodity,year) %>%
  summarize(value=sum(value)) %>%
  ungroup()->prod_crop_basin

prod_crop_reg_basin %>% filter(year==2020) %>%
  group_by(region,GLU_name) %>%
  summarize(totval=sum(value)) %>%
  ungroup()->prod_reg_basin

prod_crop_reg_basin %>% filter(year==2020) %>%
  group_by(region) %>%
  summarize(regval=sum(value)) %>%
  ungroup()->prod_reg

prod_reg_basin %>%
  left_join(prod_reg,by = "region") %>%
  mutate(basinfrac=regval/totval) %>%
  group_by(GLU_name) %>%
  top_n(n=1, wt = basinfrac) %>%
  ungroup() %>%
  select(region,GLU_name) %>%
  left_join(GCAMreg,by = "region")->GLU_reg_matchups

prod_crop_basin%>%
  # convert biomass units from EJ to Mt (17.5 MJ/kg biomass)
  filter(GCAM_commodity %in% c("biomassGrass","biomassTree", "biomass")) %>%
  mutate(value=value*1000/17.5, Units="Mt")%>%
  bind_rows(prod_crop_basin %>% filter(!GCAM_commodity %in% c("biomassGrass","biomassTree")))%>%
  # combine production, fertilizer consumption, and N content/fixation for nutrient i/o
  rename("ProdMt"=value) %>%
  full_join(Nfert_crop_basin, by = c("scenario", "GLU_name", "GCAM_commodity", "year")) %>%
  full_join(Pfert_crop_basin, by = c("scenario", "GLU_name", "GCAM_commodity", "year")) %>%
  left_join(land_crop_basin, by = c("scenario", "GLU_name", "GCAM_commodity", "year")) %>%
  mutate(GCAM_commodity=recode(GCAM_commodity,"biomassGrass"="biomass","biomassTree"="biomass"))  %>%
  left_join(nutcont, "GCAM_commodity") %>%
  left_join(GLU_reg_matchups,by = c("GLU_name")) %>%
  left_join(nfix,by = c("GCAM_commodity", "GCAM_region_ID")) %>%
  mutate(Nfixkg.yld=replace_na(Nfixkg.yld,0)) %>%
  drop_na(NfertMt) %>%
  mutate(Nin=Nfixkg.yld*ProdMt+NfertMt,
       Nout=ProdMt*contentN,
       Pin=PfertMt,
       Pout=ProdMt*contentP)->nut_basin

nut_basin %>% filter(GCAM_commodity=="FodderHerb") %>%
  mutate(Nout=Nin) %>%
  bind_rows(nut_basin %>% filter(GCAM_commodity != "FodderHerb")) %>%
  group_by(scenario,GLU_name,year) %>%
  summarize(Nin=sum(Nin,na.rm=T),
            Pin=sum(Pin, na.rm=T),
            Nout=sum(Nout,na.rm=T),
            Pout=sum(Pout,na.rm=T),
            landthouskm2=sum(landthouskm2, na.rm=T),
            ProdMt=sum(ProdMt,na.rm=T)) %>%
  left_join(nonfertN_basin, by = c("scenario", "GLU_name", "year")) %>%
  filter(year %in% modelfuture)%>%
  mutate(Nin=Nin+manureMt+NdepMt,
         NsurMt=Nin-Nout,
         PsurMt=Pin-Pout,
         Nsurkg=NsurMt*1e9,
         Psurkg=PsurMt*1e9,
         Nsurkgha=Nsurkg/(landthouskm2*1e5),
         Psurkgha=Psurkg/(landthouskm2*1e5)) %>%
  ungroup()->NPsur_cor

basin_area<-read_csv("inputs/basin_total_area.csv")

NPsur_cor %>%
  mutate(GLU=as.numeric(substr(GLU,4,nchar(GLU)))) %>%
  left_join(basin_area, by = "GLU") %>%
  select(scenario,GLU_name,year,Nsurkg,area_ha) %>%
  mutate(Nsur_totha=Nsurkg/area_ha)->Nsur_totalarea


NPsur_cor %>%
  select(scenario,GLU_name,GLU,year,Nsurkgha,Psurkgha) %>%
  rename(Nsur=Nsurkgha,Psur=Psurkgha) %>%
  pivot_longer(-c(scenario,GLU_name,GLU,year),names_to='variable') %>%
  mutate(Unit="kg/ha")-> NPsur_basin



######
#landuse ----
#####

cropland2015<-land_det %>% filter(year == 2015,
                                  GCAM_commodity %in% allcrops,
                                  value > 1) %>% # remove marginal ag areas: e.g. PacArctic, SinaiP
  group_by(scenario, GLU_name) %>%
  summarize(cropland=sum(value)) %>%
  ungroup()

forest_change <- land_det %>%  filter(grepl("Forest", GCAM_commodity)) %>%
  group_by(scenario, GLU_name, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, GLU_name) %>%
  mutate(forest_base = value[year == 2015]) %>%
  ungroup() %>%
  filter(year %in% modelfuture) %>%
  mutate(nyear = year - 2015,
         forest_change = (forest_base - value) / nyear) %>%
  left_join(cropland2015, by = c("scenario", "GLU_name")) %>%
  mutate(value = if_else(is.na(cropland), 0, forest_change / cropland),
         Unit="ha/ha",
         variable = "LCC") %>%
  select(scenario, GLU_name, year, variable, Unit, value)

nonco2<-getQuery(SAMout, "nonCO2 emissions by subsector")

nonco2 %>% filter(sector %in% allcrops) %>%
  separate(subsector,c('GCAM_commodity','GLU_name'),sep="_") %>%
  mutate(GCAM_commodity = sector) -> ag_nonco2

ag_nonco2 %>%
  mutate(ghg=substr(ghg,1,3)) %>%
  group_by(scenario,region,GLU_name,ghg,year) %>%
  summarize(value=sum(value),Units=first(Units)) %>%
  ungroup()->nonco2_totals_basin

gwp<-tibble(ghg=c('CO2','CH4','N20'),
            gwp=c(1,28,265))

nonco2_totals_basin %>%
  left_join(gwp, by="ghg")%>%
  mutate(ghg_gwp=value*gwp) %>%
  group_by(scenario,GLU_name,year) %>%
  summarize(GHG=sum(ghg_gwp,na.rm=T)) %>%
  ungroup() %>%
  filter(year %in% modelfuture) %>%
  pivot_longer(-c(scenario,GLU_name,year),names_to='variable') %>%
  mutate(Unit="tC_eq") ->nonco2_ag_basin

old<-read_csv('outdata/basin_vals.csv')%>% filter(variable%in%c("GHG","SUSI"))

basin_vals<-NPsur_basin  %>%
  bind_rows(forest_change) %>%
  bind_rows(nonco2_ag_basin) %>%
  # watbal_basin is generated in water.R
  bind_rows(watbal_basin) %>%
  left_join(basin_glu%>% select(GLU_name,GCAM_basin_ID),by = "GLU_name") %>%
  select(-GLU)

write_csv(basin_vals,'outdata/basin_vals.csv')


basin_vals %>%
  group_by(variable) %>%
  summarize(val90=quantile(value,0.9,na.rm=T), val10=quantile(value,0.1,na.rm=T))
