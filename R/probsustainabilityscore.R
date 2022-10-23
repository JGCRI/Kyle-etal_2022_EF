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

regions <- unique(region_vals$region)

years <- c(2020, 2050, 2100) # 3 maps to see changes in sustainability rating of a region

df <- region_vals

# a table with indicators according to threshold category i.e. green, yellow, and red

# pivot wider, ind vs scenario for each region, if value greater then thresholds replace the value with green, yellow or red, final table ind vs scenario with green, yellow, red label for each region

# is scaling and normalizing required for this? probably yes to be consistent with other plots

df %>% select(-Unit) %>%
  pivot_wider(names_from = variable, values_from = value)

# prepare for multiplication
df %>% select(-Unit) %>% replace(is.na(.), 0) %>%  group_by(variable) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-row) %>% filter(year == 2100 & region == 'India')

indicators_matrix

indicator_weights <- matrix(c(1:9),1,9)
scenario_weights <- matrix(c(1:8),8,1)

indicators_matrix %>%  filter(year == 2100 & region == 'India')

indicators_matrix[] %*% indicator_weights

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
psustscore = (cov + con) / 2

# maps


