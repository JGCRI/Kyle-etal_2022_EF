# library(tidyverse)
# library(sf)
# library(tmap)

#source("R/proj_load.R")
source("R/landuse.R")
source("R/nutbal.R")
source("R/water.R")
source("R/ghg_emissions.R")
source("R/ag_an_markets.R")
source("R/food_afford_refdiet.R")

# region_vals <- Nsur %>%
#   bind_rows(Psur) %>%
#   bind_rows(forest_plot) %>%
#   bind_rows(ghg_totals) %>%
#   bind_rows(exportpct) %>%
#   bind_rows(AgLabProd) %>%
#   bind_rows(food_afford) %>%
#   bind_rows(Hindex) %>%
#   left_join(GCAMreg, by = "region")%>%
#   mutate(variable=recode(variable,
#                          "forest_frac"="LCC",
#                          "Emissions"="GHG",
#                          "AgLabProd"="AGDP",
#                          "Export_pct"="TROP",
#                          "Hindex"="RSH",
#                          "Foodafford"="RSE"))

region_vals <- read_csv('outdata/region_vals.csv')
SAMthresh <- read_csv('outdata/SAM_thresholds_v1.csv')

SAMthresh %>%
  right_join(region_vals, by = "variable") %>%
  mutate(value = value * trans,
         green = green * trans,
         red = red * trans)  -> region_thresh

# regions <- st_read('inputs/shapefiles/region_boundaries_moirai_landcells_3p1_0p5arcmin.shp')

# GCAMreg %>% rename("GCAM_30_re"=GCAM_region_ID) %>%
#    left_join(region_thresh) %>% as.data.frame()->regions_data
#
# region_shape <- merge(regions,regions_data)

region_thresh %>%
  mutate(color = if_else(value > red, "red", "yellow")) %>%
  mutate(color = if_else(value < green, "green", color)) %>%
  filter(year %in% c(2020, 2050, 2100)) %>%
  mutate(color = fct_relevel(color, c("green", "yellow", "red"))) %>%
  left_join(pop_agemploy_pred %>% select(GCAM_region_ID, poptot), by = "GCAM_region_ID") ->
  region_colors

region_colors %>%
  mutate(color = replace_na(color, 'green')) %>%
  group_by(scenario, variable, year, color) %>%
  summarize(value = sum(poptot, na.rm = T)) %>%
  pivot_wider(names_from = color, values_from = value) %>%
  mutate(red = replace_na(red, 0),
         yellow = replace_na(yellow, 0),
         green = replace_na(green, 0)) %>%
  mutate(total = red + yellow + green) %>%
  mutate(green = green / total,
         yellow = yellow / total,
         red = red / total) %>%
  select(-total) %>%
  pivot_longer(-c(scenario, variable, year),
               names_to = "color",
               values_to = "pct") %>%
  mutate(color = fct_relevel(color, c("green", "yellow", "red"))) %>%
  left_join(SAMthresh %>% select(variable, long_name), by = "variable") %>%
  ungroup() -> scen_colors

#scen_colors %>%
#  filter(scenario %in% c("Reference","PotYld","PotYld_Diet","PotYld_Diet45"))%>%
#  mutate(scenario=fct_relevel(scenario,c('Reference','PotYld','PotYld_Diet','PotYld_Diet45')))%>%
#  mutate(scenario_long=recode(scenario, "PotYld"="Yield Gap Closure",
#                              "PotYld_Diet"="Yield Gap Closure + \nDietary Transition",
#                              "PotYld_Diet45"="Yield Gap Closure + \n Dietary Trainsition \nEmissions reduction"))->colorplot


scen_colors %>%
  filter(scenario %in% c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2"))%>%
  mutate(scenario=fct_relevel(scenario,c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2")))%>%
  mutate(scenario_long=recode(scenario,
                              "REF"="Reference",
                              "YLD"="Yield Intensification",
                              "FLX"="Flexitarian Diet",
                              "CO2"="CO2 Emissions Price",
                              "FLX_CO2"="Flexitarian Diet & \nCO2 Emissions Price",
                              "YLD_CO2"="Yield Intensification & \nCO2 Emissions Price",
                              "YLD_FLX"="Yield Intensification & \nFlexitarian Diet",
                              "YLD_FLX_CO2"="Combination",
                              ))->colorplot

#rgbvals=c('#2f6c20','#f2ef54','#c94711')
rgbvals=c('forestgreen','gold1','darkred')

popfrac <- colorplot %>%
  #filter(year==2050) %>%
  ggplot() + geom_col(aes(y = long_name, x = pct, fill = color)) +
  #facet_wrap(~scenario_long)+
  facet_grid(scenario_long ~ year) +
  scale_fill_manual(values = rgbvals) +
  labs(y = " ", x = "Fraction of Population in Each Threshold Class") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))

popfrac

#ggsave("figure3_fraction_thresholds_population_v1.png", popfrac, path = 'figures/', width = 11.25, height = 15, units = 'in', dpi = 200)


# this code does the same thing as above
scen_colors %>%
  #filter(year==2050) %>%
  #ggplot()+geom_col(aes(x=scenario,y=pct,fill=color))+
  ggplot() + geom_col(aes(y = long_name, x = pct, fill = color)) +
  #facet_grid(year~long_name)+
  facet_grid(scenario ~ year) +
  scale_fill_manual(values = rgbvals) +
  labs(y = "", x = "Fraction of Regions in Each Threshold Class") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text = element_text(face="bold")
  )

###
# transformations
###
