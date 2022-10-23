# Some experimentation and diagnostic plots for SAM work
# Hassan Niazi, October 2022

# inputs: basin_vals, region_vals, SAM_thresholds

library(tidyverse)
library(rmap)
library(fmsb)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(scales)


# load pre-req data ----

gcam_home = "/Users/niaz981/Downloads/Modelling/GCAM/gcam_learning/gcam-core/"
basin_glu <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/water/basin_to_country_mapping.csv"), skip=7)
basin_vals <- read_csv('outdata/basin_vals.csv')
region_vals <- read_csv('outdata/region_vals.csv')



# rmaps maps by regions

region_vals %>%
  select(-Unit) %>%
  group_by(variable) %>%
  summarize(min=quantile(value,na.rm=T,probs = 0.1),
            max=quantile(value,na.rm=T,probs = 0.9)) %>%
  left_join(region_vals, by = "variable") %>%
  mutate(value_scaled=(value-min)/(max-min)*100) %>%
  mutate(value_scaled=if_else(value_scaled>0, if_else(value_scaled>100,100,value_scaled), 0)) -> region_data_scaled # not ideal

# absolute range based scaling
# region_vals %>%
#   select(-Unit) %>%
#   group_by(variable) %>%
#   summarize(min=min(value,na.rm=T),
#            max=max(value,na.rm=T)) %>%
#   left_join(region_vals, by = "variable") %>%
#   mutate(value_scaled = (((value-min)/(max-min)*100))) -> region_data_scaled

data_scaled_reg = data.frame(names = region_data_scaled$region,
                             value = region_data_scaled$value_scaled,
                             year = region_data_scaled$year,
                             indicators = region_data_scaled$variable,
                             scenario = region_data_scaled$scenario
                             )

data_scaled_reg = data_scaled_reg %>%
  dplyr::mutate(scenario = factor(scenario, levels = c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2")))


data_scaled_reg %>% filter(year>=2020) -> data_scaled_reg_plot

r <- ggplot(data = data_scaled_reg_plot) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))


#data_scaled %>%
#  filter(names == key_basins) %>%
#  ggplot() +
#  geom_violin(aes(year, value, fill=scenario), trim = F, adjust = 1.75) +
#  facet_wrap(~names)

r +
  geom_violin(aes(year, value, fill=scenario), trim = F, adjust = .75) +
  facet_wrap(~indicators)

r +
  #geom_violin(aes(year, value, fill=scenario), trim = F, adjust = .75, alpha=0.5) +
  geom_boxplot(aes(scenario, value, fill=scenario), outline=FALSE, outlier.shape = NA) +
  #scale_x_continuous(limits = c(2020, 2100),oob = rescale_none) +
  labs(y="Scaled Indicators",x="Scenario") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~indicators) +
  theme(axis.text.x = element_blank())

r +
  geom_bar(position="dodge", stat="identity", aes(year, value, fill=scenario)) +
  facet_wrap(~indicators) +
  scale_y_continuous(limits = c(0, 100),oob = rescale_none) +
  #scale_x_continuous(limits = c(2020, 2100),oob = rescale_none) +
  labs(y="Scaled Indicators",x="Scenario") +
  scale_fill_brewer(palette = "RdBu")
#scale_fill_viridis(discrete=T, option="cividis")

r +
  geom_jitter(aes(year, value, color=scenario), alpha = 0.75) +
  scale_color_brewer(palette = "RdBu") +
  facet_wrap(~indicators)

r +
  geom_density(aes(value, fill=scenario), adjust = 1.5, alpha=0.5) +
  #scale_fill_viridis(discrete=T, option="cividis") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(x="Scaled Indicators",y="Probability Density") +
  facet_wrap(~indicators)



# rmap
scen_indi_years_reg <-
  rmap::map(data_scaled_reg,                    # data OR data_scaled
            underLayer = rmap::mapGCAMReg32, #mapGCAMLand
            subRegion = "names",
            #subRegionAlt = "ids",
            value = "value",
            x = "year",
            #x = c(2020,2050,2100),           # for limited years - preferred
            class = "indicators",
            scenario = "scenario",
            #row = "indicators",
            #col = "scenario",
            #ncol = 5,
            animate = F,
            show = F,
            folder = "figures/rmaps/regions/years",
            save = T)



# reverting basin names to incorrect names because rmap shape file doesn't recognize correct basin names
basin_glu %>% select(GCAM_basin_ID, Basin_long_name) -> basin_longname
basin_cor <- read_csv("inputs/basin_name_cor.csv")

basin_cor %>%
  filter(Basin_name != "Madasgacar") %>%
  rename(Basin_long_name = "Basin_name_cor") %>%
  left_join(basin_longname, by = "Basin_long_name") %>%
  rename(Basin_name_cor = "Basin_long_name") -> changed_basins

basin_longname_all <- merge(basin_longname, changed_basins, all = T)

basin_longname_all$Basin_long_name <- ifelse(is.na(basin_longname_all$Basin_name), basin_longname_all$Basin_long_name, basin_longname_all$Basin_name)

basin_longname_all$Basin_name <- basin_longname_all$Basin_name_cor <- NULL

# scale all indicators from 0-100

basin_vals %>%
  select(-Unit) %>%
  filter(variable != "LCC") %>%
  group_by(variable) %>%
  summarize(min=min(value,na.rm=T),
            max=max(value,na.rm=T)) %>%
  #summarize(min=quantile(value,na.rm=T,probs = 0.01),
  #          max=quantile(value,na.rm=T,probs = 0.99)) %>%
  left_join(basin_vals, by = "variable") %>%
  mutate(value_scaled = (((value-min)/(max-min)*100))) %>%
  left_join(basin_longname_all, by="GCAM_basin_ID") %>%
  select(scenario, GLU_name, year, variable, value_scaled, Unit, GCAM_basin_ID, Basin_long_name) -> basin_data_scaled



# data prep for basins ----

# absolute values
basin_vals %>%
  left_join(basin_longname_all, by="GCAM_basin_ID") %>%
  filter(variable != "LCC") -> basin_data         # only until LCC gets fixed, remove after

data = data.frame(names = basin_data$Basin_long_name,
                  value = basin_data$value,
                  year = basin_data$year,
                  indicators = basin_data$variable,
                  scenario = basin_data$scenario
                  )
data = data %>%
  dplyr::mutate(scenario = factor(scenario, levels = c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2")))

# scaled values
data_scaled = data.frame(names = basin_data_scaled$Basin_long_name,
                  value = basin_data_scaled$value_scaled,
                  year = basin_data_scaled$year,
                  indicators = basin_data_scaled$variable,
                  scenario = basin_data_scaled$scenario
                  )

data_scaled = data_scaled %>%
  dplyr::mutate(scenario = factor(scenario, levels = c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2")))

# figures ----

# violins by key basins
key_basins = c("California_River", "Missouri_River", "Nile", "Central_Iran", "Amu_Darya",
               "Arabian_Peninsula", "Indus", "Sabarmati", "Krishna", "Ziya_He_Interior")

g <- ggplot(data = data_scaled) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#data_scaled %>%
#  filter(names == key_basins) %>%
#  ggplot() +
#  geom_violin(aes(year, value, fill=scenario), trim = F, adjust = 1.75) +
#  facet_wrap(~names)

g +
  geom_violin(aes(year, value, fill=scenario), trim = F, adjust = .75) +
  facet_wrap(~indicators)

g +
  #geom_violin(aes(year, value, fill=scenario), trim = F, adjust = .75, alpha=0.5) +
  geom_boxplot(aes(year, value, fill=scenario), outline=FALSE, outlier.shape = NA) +
  facet_wrap(~indicators)

g +
  geom_bar(position="dodge", stat="identity", aes(year, value, fill=scenario)) +
  facet_wrap(~indicators) +
  scale_y_continuous(limits = c(40, 100),oob = rescale_none) +
  scale_fill_brewer(palette = "RdBu")
  #scale_fill_viridis(discrete=T, option="cividis")

g +
  geom_jitter(aes(year, value, color=scenario), alpha = 0.75) +
  scale_color_brewer(palette = "RdBu") +
  facet_wrap(~indicators)

g +
  geom_density(aes(value, fill=scenario), adjust = 0.5, alpha=0.25) +
  scale_fill_viridis(discrete=T, option="viridis") +
  facet_wrap(~indicators)


# maps ----

# map: multi-scenario, multi-indicator, individual years plot
scen_indi_years <-
  rmap::map(data_scaled,                    # data OR data_scaled
          underLayer = rmap::mapGCAMBasins, #mapGCAMLand
          subRegion = "names",
          #subRegionAlt = "ids",
          value = "value",
          x = "year",
          #x = c(2020,2050,2100),           # for limited years - preferred
          class = "indicators",
          scenario = "scenario",
          #row = "indicators",
          #col = "scenario",
          #ncol = 5,
          animate = F,
          show = F,
          folder = "figures/rmaps/basins/years",
          save = T)


# map: multi-scenario, multi-indicator, difference by years plot

scen_indi_yearsdiff <-
  rmap::map(data,
          underLayer = rmap::mapGCAMBasins, #mapGCAMLand
          subRegion = "names",
          value = "value",
          #x = "year",
          #x = c("2020","2050","2100"),           # for limited years - prefered
          xRef = 2020,                            # for difference plots
          xDiff = c(2050, 2100),                  # generate for 2020, 2050, 2100
          class = "indicators",
          scenario = "scenario",
          #ncol = 5,
          animate = F,
          show = F,
          folder = "figures/rmaps/basins/yeardiff",
          save = T
          )

scen_indi_yearsdiff$map_param_KMEANS_xDiffAbs -> yearsdiff_abs
scen_indi_yearsdiff$map_param_KMEANS_xDiffPrcnt -> yearsdiff_pct

# map: multi-indicator, multi-year, difference by scenario for year 2050 and 2100
indi_years_scendiff <-
  rmap::map(data,
          underLayer = rmap::mapGCAMBasins, #mapGCAMLand
          subRegion = "names",
          value = "value",
          x = "year",
          #x = c("2020","2050","2100"),           # for limited years - prefered
          scenRef = "REF",
          #scenDiff = c("scen2","scen3"), # Can omit this to get difference against all scenarios
          class = "indicators",
          scenario = "scenario",
          #ncol = 5,
          animate = F,
          show = F,
          folder = "figures/rmaps/basins/scenariodiff",
          save = T
          )

indi_years_scendiff$map_param_KMEANS_DiffAbs -> scendiff_abs
indi_years_scendiff$map_param_KMEANS_DiffPrcnt -> scendiff_pct

# map: simpler global map: drop one of the analysis dimension

