
irr_withdrawals<-getQuery(SAMout,"irrigation water withdrawals by crop type and land region") %>%
  filter(!region %in% no_SAM_regions)
runoff_total<-getQuery(SAMout,"Basin level available runoff") %>%
  filter(!region %in% no_SAM_regions)
avail_frac<-read_csv('./inputs/avail_frac.csv')

#basin names in outputs have errors, need to replace with correct names
basin_cor<-read_csv("inputs/basin_name_cor.csv")
runoff_total %>%
  separate(basin, sep="_",into=c('GLU_name','thing')) %>%
  select(-thing) %>%
  left_join(basin_glu %>%
              select(GCAM_basin_ID,GLU_name),
            by='GLU_name') %>%
  rename("total_runoff"=value) %>%
  filter(year %in% modelfuture) %>%
  left_join(avail_frac,by="GLU_name") %>%
  mutate(avail_cor=if_else(avail_frac>0.8,0.8,avail_frac),
         # setting 0.5 as the default for basins whose available fractions aren't in the avail_frac.csv dataset
         avail_cor = if_else(is.na(avail_cor), 0.5, avail_cor)) %>%
  mutate(sust_avail=total_runoff*avail_cor) %>%
  select(scenario,GLU_name,GCAM_basin_ID,year,total_runoff,sust_avail)->sust_basin

irr_withdrawals %>%
  filter(str_detect(input,'water_td_irr')) %>%
  separate(input,sep="_",into=c('one','two','three','GLU_name','five')) %>%
  group_by(scenario,region,GLU_name,year) %>%
  summarize(irr_total=sum(value, na.rm = T)) %>%
  ungroup()->irr_basin

irr_basin %>%
  filter(year %in% modelfuture) %>%
  inner_join(sust_basin, by = c("scenario", "GLU_name", "year")) -> irr_sust

# 10/6/22 GPK edit - this indicator needs to be calculated in each region x basin as the total basin's irrigation / supply
# (not the region x basin's irrigation / the total basin's supply)
irr_sust_basin <- group_by(irr_sust, scenario, GLU_name, year, GCAM_basin_ID) %>%
  summarise(irr_total = sum(irr_total),
            total_runoff = mean(total_runoff),
            sust_avail = mean(sust_avail)) %>%
  ungroup() %>%
  mutate(value = irr_total/sust_avail) %>%
  select(scenario, GLU_name, GCAM_basin_ID, year, value)

# Join in the basin's SUSI indicator
irr_sust %>% left_join(irr_sust_basin, by = c("scenario", "GLU_name", "GCAM_basin_ID", "year")) %>%
  left_join(GCAMreg,by = "region") %>%
  select(scenario,region,GCAM_basin_ID,GLU_name,year,value)%>%
  mutate(variable="SUSI",
         Unit="pct") %>%
  select(scenario,region,GLU_name,year,variable,value,Unit,GCAM_basin_ID) %>%
  ungroup()->watbal

# To aggregate from region x basin to region, irrigated cropland is the appropriate weighting factor
watbal %>%
  left_join(irr_basin, by = c("scenario", "region", "GLU_name", "year")) %>%
  mutate(weighted_value = value * irr_total) %>%
  group_by(scenario,region,year, variable, Unit) %>%
  summarize(weighted_value=sum(weighted_value),
            irr_total = sum(irr_total)) %>%
  ungroup() %>%
  mutate(value = weighted_value / irr_total) %>%
  select(scenario, region, year, variable, Unit, value) -> watbal_region

watbal %>%
  group_by(scenario,GLU_name,year, variable, Unit) %>%
  summarize(value=mean(value)) %>%
  ungroup()->watbal_basin

watbal  %>%
  mutate(color=if_else(value<0.3, "green","yellow")) %>%
  mutate(color=if_else(value>0.6,"red",color)) %>%
  mutate(color=fct_relevel(color,c("green","yellow","red")))->watcolor


watcolor %>%
  group_by(scenario,variable,year) %>%
  summarize(green=sum(color=="green"),yellow=sum(color=="yellow"),red=sum(color=="red")) %>%
  mutate(total=green+yellow+red)%>%
  ungroup()%>%
  pivot_longer(-c(scenario,variable,year,total),names_to="color",values_to = "count") %>%
  mutate(pct=count/total) %>%
  mutate(color=fct_relevel(color,c("green","yellow","red")),
         variable="sustwater")  ->wat_colors

rm(irr_withdrawals,runoff_total,avail_frac,basin_cor,
   sust_basin,irr_basin,irr_sust)
