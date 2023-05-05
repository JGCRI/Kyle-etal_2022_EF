# For a global map of agricultural sustainability index
# Hassan Niazi, Oct 2022

# Inputs: region_vals.csv containing all 8 scenarios and all 9 indicators. Select region based on resolution of interest, as in a region could a country, energy region, or a basin etc.

# Idea

# In a region r
# Tn year y = 2100

# Sustainability score = (coverage + confidence) / 2

# Coverage -> # no indicators in green range in year y in scenario s / total indicators -> 0-1 fraction
# this sweep will establish how robustly a region is performing across all indicators (more the indicators in green, better the region)

# Confidence -> # of scenarios in which indicator i is green / total scenarios -> 0-1 fraction
# this sweep with establish how confident we are to see a particular indicator performing well (more the scenarios showing an indicator in green, better the confidence for that indicator in a particular region)

# read data
region_vals <- read_csv("./outdata/region_vals.csv")
SAMthresh <- read_csv('outdata/SAM_thresholds_v1.csv')

GCAMreg <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/common/GCAM_region_names.csv"),skip=6)
regions <- unique(region_vals$region)


# Transforming, Scaling and Normalizing

# transform thresholds
SAMthresh -> thresh_tr

thresh_tr %>%  filter(variable=="GHG") %>%  mutate(green=-log10(green),red=-log10(red))->ghgtr

thresh_tr %>%  filter(variable%in%c('TROP','AGDP')) %>%  mutate(green=log10(green),red=log10(red))->agtrop

thresh_tr %>%  filter(variable=="LCC") %>%  mutate(green=-log10(green+0.23),red=-log10(red+0.23))->LCCthr

thresh_tr %>%  filter(variable%in% c("Nsur","Psur"))%>%  mutate(green=-green,red=-red)->NPsurtrans

thresh_tr %>%  filter(variable%in%c('RSH',"RSE"))->Resil

trans_thresholds <- ghgtr %>%
  bind_rows(agtrop) %>%  bind_rows(LCCthr) %>%  bind_rows(NPsurtrans) %>%  bind_rows(Resil) %>%
  select(-trans)

region_vals %>%  select(-Unit) %>%
  pivot_wider(names_from = variable, values_from = value)  -> varswide

varswide %>%
  filter(year>2010) %>%
  mutate(SUSI=-log10(SUSI),
         Psur=-Psur,
         Nsur=-Nsur,
         LCC=-log10(LCC+0.23),
         GHG=-log10(GHG),
         AGDP=log10(AGDP),
         TROP=log10(TROP),
         RSH=RSH,
         RSE=RSE) %>%
  select(scenario,region,year,GCAM_region_ID,SUSI,Psur,Nsur,LCC,GHG,AGDP,TROP,RSH,RSE) %>%
  pivot_longer(cols=c(SUSI,Psur,Nsur,LCC,GHG,AGDP,TROP,RSH,RSE),
               names_to='variable',values_to='value_trans') -> varstrans

# scaling and normalizing
varstrans %>%
  group_by(variable) %>%
  summarize(min=quantile(value_trans,na.rm=T,probs = 0.1),
            max=quantile(value_trans,na.rm=T,probs = 0.9)) -> varsbounds # 10 and 90 percentiles as bounds for scaling

varstrans %>%
  left_join(varsbounds, by = "variable") %>%
  mutate(valnorm=(value_trans-min)/(max-min)*100) -> norm_vars # normalized values as a percentage

norm_vars %>%
  select(scenario,year,GCAM_region_ID,variable, valnorm)%>%
  mutate(valnorm = if_else(valnorm>0, if_else(valnorm>100,100,valnorm), 0)) -> varstrans_scaled


# normalized and scaled data
df <- varstrans_scaled %>% select(GCAM_region_ID, year, scenario, variable, valnorm) %>% rename(value=valnorm)


# aggregation based score: single year and region example
df %>% replace(is.na(.), 0) %>%  group_by(variable) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-row) -> indicators_matrix #%>% filter(year == 2100 & GCAM_region_ID == '1')

indicator_weights <- matrix(c(1:1), nrow=9, ncol=1)
scenario_weights <- matrix(c(1:1), nrow=1, ncol=8)

indicators_matrix %>%  #filter(year == 2100 & region == 'India')
  filter(year == 2100 & GCAM_region_ID == '1') -> indicators_reg_yr
(as.matrix(indicators_reg_yr[4:12])%*%indicator_weights) / length(indicator_weights) -> agg_indicators #coverage
(scenario_weights %*% agg_indicators) / length(scenario_weights) -> agg_indicators_scenarios #confidence and score


# score: loop for 3 years and all regions
years <- c(2020, 2050, 2100) # 3 maps to see changes in sustainability rating of a region over time

score_yr <- data.frame(year = numeric(), GCAM_region_ID=numeric(), score = numeric()) # initializing
temp <- list()

for (yr in years){
  for (id in unique(indicators_matrix$GCAM_region_ID)) {
    indicators_matrix %>%
      filter(year == yr & GCAM_region_ID == id) -> indicators_reg_yr
    (as.matrix(indicators_reg_yr[4:12])%*%indicator_weights) / length(indicator_weights) -> agg_indicators #coverage
    (scenario_weights %*% agg_indicators) / length(scenario_weights) -> agg_indicators_scenarios #confidence and score
    score_yr[id, 1] <- yr
    score_yr[id, 2] <- id
    score_yr[id, 3] <- agg_indicators_scenarios
  }
  temp[[yr]] <- score_yr # store each output from the inner loop in a list
}
# collapse the list down to a single data frame
score_all <- do.call(rbind, temp) %>% left_join(GCAMreg, by = "GCAM_region_ID")



# maps of scores
# 1. using rmap
data_scores = data.frame(names = score_all$region,
                         value = score_all$score,
                         year = score_all$year
)

score_maps_reg <-
  rmap::map(data_scores,                    # data OR data_scaled
            underLayer = rmap::mapGCAMReg32, #mapGCAMLand
            subRegion = "names",
            #subRegionAlt = "ids",
            value = "value",
            x = "year",
            #x = c(2020,2050,2100),           # for limited years - preferred
            #row = "indicators",
            #col = "scenario",
            #ncol = 5,
            legendFixedBreaks=c(0, 33, 66, 100), # use palette with breaks
            #palette = c('gray','forestgreen','gold1','darkred'),
            palette = c('gray','darkred','gold1','forestgreen'), #reversing the colors order because now green (higher values closer to 100) means higher sustainability
            animate = T,
            show = T,
            folder = "figures/scores/rmaps",
            save = T)










### ARCHIEVED / UNFINISHED #################################################################

# 2. tmap
regions <- st_read('inputs/shapefiles/region_boundaries_moirai_landcells_3p1_0p5arcmin.shp')

map<-tm_shape(plotdf)+
  tm_fill("value",title="TOP",palette=rygvals)+
  tm_layout(title=scenario_name)



# a table with indicators according to threshold category i.e. green, yellow, and red

df %>% select(-Unit) %>%
  pivot_wider(names_from = variable, values_from = value)

SAMthresh %>%
  right_join(df, by = "variable") %>%
  mutate(value=value*trans, green=green*trans, red=red*trans)  %>%
  mutate(color=if_else(value>red, "red","yellow")) %>%
  mutate(color=if_else(value<green,"green",color)) %>%
  filter(year %in% c(2020,2050,2100)) %>%
  select(scenario, year, GCAM_region_ID, region, variable, color) %>%
  mutate(color=fct_relevel(color,c("green","yellow","red"))) %>%
  #filter(is.na(color) == T) %>%  #agdp and susi not being generated for Taiwan
  #mutate(color=replace_na(color,'green')) %>%
  pivot_wider(names_from = variable, values_from = color) %>%
  replace(is.na(.), 'green') %>% # hard coding NA as greens (only for some indicators in Taiwan) -> try removing at the end
  mutate(scenario=fct_relevel(scenario,c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2")))  -> colorcoded # all regions, all indicators, all scenarios, 3 years


# 1st sweep for coverage: threshold frequency for each indicator across all scenarios

colorcoded %>% group_by(scenario, year, GCAM_region_ID) %>% count('green')

summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) ->a

for (yr in years) {
  for (id in unique(colorcoded$GCAM_region_ID)) {
    colorcoded %>%
      filter(year == yr & GCAM_region_ID == id) -> a
    #mutate(covgreen = )
    #table(colorcoded['Nsur'])/length(colorcoded$Nsur)

  }
}




# 2nd sweep for confidence

# sustainability score
prob_sust_score = (cov + con) / 2

# maps


