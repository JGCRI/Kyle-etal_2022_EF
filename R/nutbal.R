###
# Read in external input data from N fertilizer database (Zhang et al. 2015)
###

# Nutrient (P and N) content of crops by GCAM region and commodity
nutcont<-read_csv("inputs/NutCont_commod.csv")
# N fixation (kgN per kg yield)
nfix<-read_csv("inputs/Nfix_commod_region.csv") %>%
  select(GCAM_commodity,GCAM_region_ID,Nfixkg.yld) %>%
  filter(Nfixkg.yld>0)

# This /inputs/ data file contains scenario-specific information; needs to be re-generated in inputs/R/manure_fraction.R if new scenarios are used
nonfertN<-read_csv('./inputs/nonfertN.csv') %>%
  filter(!region %in% no_SAM_regions)

####
# Read in GCAM outputs from .dat file; process and combine for i/o relationships
###
landbycrop<-getQuery(SAMout,"land allocation by crop") %>%
  filter(!region %in% no_SAM_regions) %>%
  mutate(landleaf = sub("C4|Tree", "", landleaf))

crops_all<-landbycrop %>% filter(landleaf %in% allcrops, year %in% modelfuture) %>%
  group_by(scenario,region,year) %>%
  summarize(cropland=sum(value)) %>%
  ungroup()

Nfertcons<-getQuery(SAMout,"fertilizer consumption by crop type")%>%
  filter(!region %in% no_SAM_regions,
         sector != "Exports_fertilizer") %>%
  select(scenario,region,sector,year,value) %>%
  rename("GCAM_commodity"=sector,"NfertMt"=value)

Pfertcons<-getQuery(SAMout,"P_fertilizer consumption by crop type") %>%
  filter(!region %in% no_SAM_regions) %>%
  select(scenario,region,sector,year,value) %>%
  rename("GCAM_commodity"=sector,"PfertMt"=value)

cropprod<-getQuery(SAMout,"ag production by crop type")%>%
  filter(!region %in% no_SAM_regions,
         !output%in%c("Forest","Pasture")) %>%
  select(scenario,region,output,year,value) %>%
  rename("GCAM_commodity"=output)

landbycrop %>%
  rename('GCAM_commodity'=landleaf) %>%
  mutate(GCAM_commodity=recode(GCAM_commodity,'biomassGrass'='biomass','biomassTree'='biomass')) %>%
  group_by(scenario,region,GCAM_commodity,year) %>%
  summarize(value=sum(value,na.rm=T)) %>%
  rename(landthouskm2=value)->cropland

cropprod %>%
  # convert biomass units from EJ to Mt (17.5 MJ/kg biomass)
  filter(GCAM_commodity=="biomass") %>%
  mutate(value=value*1000/17.5, Units="Mt")%>%
  bind_rows(cropprod %>% filter(GCAM_commodity!='biomass')) %>%
# combine production, fertilizer consumption, and N content/fixation for nutrient i/o
  select(scenario,region,GCAM_commodity,year,value) %>%
  rename("ProdMt"=value) %>%
  full_join(Nfertcons, by = c("scenario", "region", "GCAM_commodity", "year")) %>%
  full_join(Pfertcons, by = c("scenario", "region", "GCAM_commodity", "year"))%>%
  left_join(GCAMreg, by = "region") %>%
  left_join(nutcont, "GCAM_commodity") %>%
  left_join(nfix,by = c("GCAM_commodity", "GCAM_region_ID"))%>%
  left_join(cropland, by = c("scenario", "region", "GCAM_commodity", "year")) %>%
  mutate(Nfixkg.yld=replace_na(Nfixkg.yld,0),
         NinMt=NfertMt+Nfixkg.yld*ProdMt,
         NoutMt=ProdMt*contentN,
         PinMt=PfertMt,
         PoutMt=ProdMt*contentP,
         NsurMt=NinMt-NoutMt,
         PsurMt=PinMt-PoutMt) %>%
  drop_na(NfertMt)->nutio #drop cases with production but no N fertilizer:


#
# Australia_NZ
# Rice
#
# Australia_NZ
# Soybean
#
# Canada
# FodderGrass
#
# EU-15
# PalmFruit
#
# Europe_Non_EU
# FodderGrass
#
# Japan
# FiberCrop
#
# Japan
# OilCrop
#
# Mexico
# Soybean
#
# Pakistan
# Soybean
#
# South Africa
# Rice
#
# South Africa
# Soybean
#
# South America_Northern
# Wheat

# calculate N and P surplus
nutio %>%
  mutate(Nin=Nfixkg.yld*ProdMt+NfertMt,
         Nout=ProdMt*contentN,
         Pin=PfertMt,
         Pout=ProdMt*contentP) %>%
  select(scenario,region,GCAM_commodity,year,ProdMt,Nin,Nout,Pin,Pout) %>%
  mutate(Nsur=Nin-Nout, Psur=Pin-Pout) %>%
  filter(GCAM_commodity %in% allcrops) %>%
  mutate(Units="Mt")->nutbal


nutio %>%
  group_by(scenario,region,year) %>%
  summarize(NinMt=sum(NinMt),NoutMt=sum(NoutMt),landthouskm2=sum(landthouskm2)) %>%
  ungroup() %>%
  full_join(nonfertN, by = c("scenario", "region", "year"))%>%
  filter(year%in%modelfuture) %>%
  mutate(NinMt_cor=NinMt+manureMt+NdepMt,
      Nsur=NinMt_cor-NoutMt,
      Nsurkg=Nsur*1e9,cropland=landthouskm2*1e5,
      Nsurha=Nsurkg/cropland)%>%
  select(scenario,region,year,Nsurha) %>%
  rename(Nsur=Nsurha) %>%
  pivot_longer(-c(scenario,region,year),names_to='variable') %>%
  mutate(Unit="kg/ha")-> Nsur
#
#
#
#
# nutbal %>% filter(GCAM_commodity %in% allcrops,year %in% modelfuture,Nin>0) %>%
#   group_by(scenario,region,year) %>%
#   summarize(Nsur=sum(Nsur,na.rm=T)) %>%
#   ungroup() %>%
#   left_join(crops_all, by = c("scenario", "region", "year")) %>%
#   mutate(Nsurkg=Nsur*1e6,cropland=cropland*1e5) %>%
#   mutate(Nsurha=Nsurkg/cropland) %>%
#   select(scenario,region,year,Nsurha) %>%
#   rename(Nsur=Nsurha) %>%
#   pivot_longer(-c(scenario,region,year),names_to='variable') %>%
#   mutate(Unit="kg/ha",value=10*value)-> Nsur


nutbal %>% filter(GCAM_commodity %in% allcrops,year %in% modelfuture) %>%
  group_by(scenario,region,year) %>%
  summarize(Psur=sum(Psur,na.rm=T)) %>%
  ungroup() %>%
  left_join(crops_all, by = c("scenario", "region", "year")) %>%
  mutate(Psurkg=Psur*1e6,cropland=cropland*1e5) %>%
  mutate(Psurha=Psurkg/cropland) %>%
  select(scenario,region,year,Psurha) %>%
  rename(Psur=Psurha) %>%
  pivot_longer(-c(scenario,region,year),names_to='variable') %>%
  mutate(Unit="kg/ha",value=1000*value)-> Psur

#rm(nutcont, Nfertcons, Pfertcons, nutio, nutbal)

