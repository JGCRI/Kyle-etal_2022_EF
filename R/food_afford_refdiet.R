
foodcons<-getQuery(SAMout,'food consumption by type (specific)')
ref_diet<-read_csv('inputs/Ref_diet.csv')
income1q<-read_csv('inputs/income_1q.csv')

ref_diet %>%
  group_by(GCAM_commodity) %>%
  summarize(kg_year=sum(kg_year)) %>%
  ungroup()->ref_commod

Grains <- c('Corn','Rice','Wheat','OtherGrain')
Meat <- c('Beef','Pork','SheepGoat')

foodcons %>%
  filter(subsector %in% Grains)%>%
  group_by(scenario,region,year) %>%
  summarize(total=sum(value))->grain_total

foodcons %>%
  filter(subsector %in% Grains) %>%
  select(scenario,region,year,subsector,value) %>%
  pivot_wider(names_from = subsector, values_from=value) %>%
  left_join(grain_total) %>%
  mutate(Corn=Corn/total,Rice=Rice/total,Wheat=Wheat/total,OtherGrain=OtherGrain/total) %>%
  select(-total) %>%
  pivot_longer(-c(scenario,region,year),names_to='GCAM_commodity') %>%
  mutate(kg_year=value*ref_diet$kg_year[ref_diet$Group=="Grains"])->grain_kg

# meat consumption
foodcons %>%
  #filter(subsector %in% Meat)%>% # Old gcam had meat in subsector, now its in technology
  filter(technology %in% Meat) %>%
  group_by(scenario,region,year) %>%
  summarize(total=sum(value))->meat_total

foodcons %>%
  #filter(subsector %in% Meat) %>%
  filter(technology %in% Meat) %>%
  #select(scenario,region,year,subsector,value) %>%
  #pivot_wider(names_from = subsector, values_from=value) %>%
  select(scenario,region,year,technology,value) %>%
  pivot_wider(names_from = technology, values_from=value) %>%
  left_join(meat_total, by = c("scenario", "region", "year")) %>%
  mutate(Beef=Beef/total,Pork=Pork/total,SheepGoat=SheepGoat/total) %>%
  select(-total) %>%
  pivot_longer(-c(scenario,region,year),names_to='GCAM_commodity') %>%
  mutate(kg_year=value*21)->meat_kg


ag_an_value %>%
  filter(GCAM_commodity %in% Grains) %>%
  left_join(grain_kg, by = c("scenario", "region", "GCAM_commodity", "year")) %>%
  select(scenario,region,GCAM_commodity,year,price2010,kg_year)->grains


ag_an_value %>%
  filter(GCAM_commodity %in% Meat) %>%
  left_join(meat_kg, by = c("scenario", "region", "GCAM_commodity", "year")) %>%
  select(scenario,region,GCAM_commodity,year,price2010,kg_year)->meat


ag_an_value %>%
      filter(!(GCAM_commodity %in% c(Grains,Meat))) %>%
      left_join(ref_commod, by = c( "GCAM_commodity")) %>%
      select(scenario,region,GCAM_commodity,year,price2010,kg_year) %>%
  filter(GCAM_commodity%in% c(foodcrops,animals))->otherfood

ref_cost<-grains %>% bind_rows(meat) %>% bind_rows(otherfood) %>%
  mutate(cost_year=kg_year*price2010)

ref_cost %>%
    group_by(scenario,region,year) %>%
    summarize(cost_year=sum(cost_year, na.rm=T)) %>% #NA vals are e.g. Pork in Pakistan
    left_join(income1q,by = c("region", "year")) %>%
    filter(year %in% modelfuture) %>%
    mutate(value=gdpcap1q/cost_year) %>%
  mutate(variable="Foodafford",
         Unit="--")%>%
  select(scenario,region,year, variable, value)->food_afford

rm(foodcons,ref_diet,income1q,ref_commod,ref_cost, grain_total,
   grain_kg, meat_total, meat_kg, grains, meat, otherfood)

