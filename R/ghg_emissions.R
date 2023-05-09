
###
# temp MI kludge
##
# library(readxl)
# MIemissions<-'~/Dropbox/JGCRI/SAM/SAM-matrix/inputs/temp_nonco2.xlsx'
# nonco245<-read_xlsx(MIemissions,sheet=1,skip=1) %>%
#   bind_rows(read_xlsx(MIemissions,sheet=2,skip=1)) %>%
#   bind_rows(read_xlsx(MIemissions,sheet=3,skip=1)) %>%
#   bind_rows(read_xlsx(MIemissions,sheet=4,skip=1)) %>%
#   pivot_longer(-c(Units,scenario,region,sector,GHG),names_to="year") %>%
#   mutate(scenario=recode(scenario, "DietaryTransition45,date=2021-9-2T07:24:54-08:00"="DietaryTransition45",
#                          "PotYld45,date=2021-8-2T15:20:36-08:00" = "PotYld45",
#                          "PotYld_Diet45,date=2021-5-2T05:51:10-08:00"="PotYld_Diet45",
#                          "Reference45,date=2021-23-2T14:07:37-08:00"="Reference45"),
#          year=as.numeric(year))  ->nonco245


# comes from landuse.R but just in case
# landbycrop<-getQuery(SAMout,"land allocation by crop")  %>%
#     filter(!region %in% no_SAM_regions) %>%
#     mutate(landleaf = sub("C4|Tree", "", landleaf))

#ag land including pasture
landbycrop %>%
  filter(landleaf%in%c(allcrops,animals,otheragsector), year %in% modelfuture)%>%
  group_by(scenario,region,year) %>%
  summarize(agland=sum(value)*1e5) ->agland_ha

# GHGs from ag
nonco2<-getQuery(SAMout,"nonCO2 emissions by sector")  %>%
  filter(!region %in% no_SAM_regions,
         sector %in% c(allcrops,animals,otheragsector))

nonco2 %>%
  mutate(ghg=substr(ghg,1,3)) %>%
  group_by(scenario,region,ghg,year) %>%
  summarize(value=sum(value),Units=first(Units)) %>%
  ungroup()->nonco2_totals

gwp <- tibble(ghg=c('CO2','CH4','N20'),
            gwp=c(1,28,265))

nonco2_totals %>%
  left_join(gwp, by="ghg")%>%
  mutate(ghg_gwp=value*gwp) %>%
  group_by(scenario,region,year) %>%
  summarize(nonco2=sum(ghg_gwp,na.rm=T)) %>%
  ungroup()->nonco2_eq

# CO2
co2sector <- getQuery(SAMout,"CO2 emissions by sector (no bio)") %>%
  filter(!region %in% no_SAM_regions,
         sector %in% c(allcrops, animals))

co2sector %>%
  group_by(scenario,region, year) %>%
  summarize(co2=sum(value))->co2_totals

co2_totals %>%
  full_join(nonco2_eq, by = c("scenario", "region", "year")) %>%
  # if agricultural energy use isn't endogenous, tied to agricultural activities, then this may be zero
  # users can add the string "agricultural energy use" to the filter(sector %in% c(...)) statement above
  # this isn't done at present because the energy used by "agricultural energy use" is driven by GDP, not activities in the ag sector
  replace_na(list(co2 = 0)) %>%
  left_join(agland_ha) %>%
  filter(year %in% modelfuture)%>%
  rowwise() %>%
  mutate(co2tot = co2+nonco2, value = co2tot/agland*1e6) %>% # outside co2_totals co2 exists as independent R dataset
  mutate(variable="Emissions", Unit="tC_eq/ha/year") %>%
  select(scenario,region,year,variable,value,Unit) %>%
  ungroup()-> ghg_totals

