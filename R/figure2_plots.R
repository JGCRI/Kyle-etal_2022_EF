# This code file produces the 4-panel Figure 2 (first figure in the results)
Fig2_scenarios <- c("REF", "YLD", "FLX", "CO2")
scenario_colors <- c(REF = 'black',
                     YLD = 'green',
                     FLX = 'orange',
                     CO2 = 'skyblue')

# Figure 2a: total global cropland by scenario
total_cropland <- filter(landbycrop, landleaf %in% allcrops,
                         scenario %in% Fig2_scenarios,
                         year %in% modelfuture) %>%
  group_by(scenario, year) %>%
  summarise(value = sum(value) / 1000) %>%
  ungroup()

# Ancillary calculations for Figure 2a
print(paste0("FLX cropland delta: ",
             round(total_cropland$value[total_cropland$scenario == "FLX" & total_cropland$year == 2100] /
                     total_cropland$value[total_cropland$scenario == "REF" & total_cropland$year == 2100] - 1,
                   digits = 3)))
print(paste0("YLD cropland delta: ",
             round(total_cropland$value[total_cropland$scenario == "YLD" & total_cropland$year == 2100] /
                     total_cropland$value[total_cropland$scenario == "REF" & total_cropland$year == 2100] - 1,
                   digits = 3)))
print(paste0("CO2 cropland delta: ",
             round(total_cropland$value[total_cropland$scenario == "CO2" & total_cropland$year == 2100] /
                     total_cropland$value[total_cropland$scenario == "REF" & total_cropland$year == 2100] - 1,
                   digits = 3)))

# Generate Figure 2a
ggplot(total_cropland, aes(x = year, y = value)) +
  geom_line(aes(color = scenario)) +
  scale_color_manual(values = scenario_colors) +
  theme_bw() +
  xlab("") +
  ylab("Million km2") +
  ggtitle("a") +
  theme(plot.title = element_text(vjust = -7, hjust = 0.03)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.84, 0.25))
ggsave("figures/figure2a.png", height = 3.5, width = 3, units = "in")

# Figure 2b. Radiative forcing by scenario
forcing <- read_csv("inputs/forcing_total.csv", skip = 1) %>%
  mutate(scenario = substr(scenario, 1, 3)) %>%
  select(-region, -`forcing-total`, -Units) %>%
  gather(key = "year", value = "value", -scenario) %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% modelfuture)

# Ancillary calculations for Figure 2b
print(paste0("FLX radiative forcing delta: ",
             round(forcing$value[forcing$scenario == "FLX" & forcing$year == 2100] /
                     forcing$value[forcing$scenario == "REF" & forcing$year == 2100] - 1,
                   digits = 3)))

# Generate Figure 2b
ggplot(forcing, aes(x = year, y = value)) +
  geom_line(aes(color = scenario)) +
  scale_color_manual(values = scenario_colors) +
  theme_bw() +
  xlab("") +
  ylab("W/m2") +
  ggtitle("b") +
  theme(plot.title = element_text(vjust = -7, hjust = 0.03)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.84, 0.3))
ggsave("figures/figure2b.png", height = 3.5, width = 3, units = "in")

# Figure 2c: kcal/pers/d by scenario and food commodity class
foodcons<-getQuery(SAMout,'food consumption by type (specific)')
global_pop<-getQuery(SAMout,'population by region') %>%
  group_by(scenario, year) %>%
  summarise(pop = sum(value)) %>%
  ungroup()

veg_protein <- c("Legumes", "NutsSeeds", "Soybean")
staples <- c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat")
other_crops <- cropsnobio[!cropsnobio %in% c(veg_protein, staples, animals)]
diet_2050 <- foodcons %>%
  mutate(category = if_else(technology %in% veg_protein, "Veg protein",
                            if_else(technology %in% animals, "Animal",
                                    if_else(technology %in% staples, "Staples", "Other crops")))) %>%
  group_by(scenario, category, year) %>%
  summarise(Pcal = sum(value)) %>%
  ungroup() %>%
  filter((year == 2015 & scenario == "REF") |
           year == 2050,
         scenario %in% Fig2_scenarios) %>%
  left_join(global_pop, by = c("scenario", "year")) %>%
  mutate(kcal_cap_d = Pcal * 1e9 / pop / 365,
         scenario = factor(paste(year, scenario),
                           levels = c("2015 REF", "2050 REF", "2050 YLD", "2050 FLX", "2050 CO2"))) %>%
  select(scenario, category, year, kcal_cap_d)

# Ancillary calculations for Figure 2c
print(paste0("FLX veg protein increase from REF: ",
             round(diet_2050$kcal_cap_d[diet_2050$scenario == "2050 FLX" & diet_2050$category == "Veg protein"] /
                     diet_2050$kcal_cap_d[diet_2050$scenario == "2050 REF" & diet_2050$category == "Veg protein"] - 1,
                   digits = 3)))
print(paste0("FLX veg protein increase from REF: ",
             round(diet_2050$kcal_cap_d[diet_2050$scenario == "2050 FLX" & diet_2050$category == "Animal"] /
                     diet_2050$kcal_cap_d[diet_2050$scenario == "2050 REF" & diet_2050$category == "Animal"] - 1,
                   digits = 3)))

# Generate Figure 2c
ggplot(diet_2050, aes(x = scenario, y = kcal_cap_d)) +
  geom_bar(stat = "identity", aes(fill = category)) +
  theme_bw() +
  ylab("kcal per capita per day") +
  xlab("") +
  ggtitle("c") +
  theme(plot.title = element_text(vjust = -7, hjust = 0.03)) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle=90))
ggsave("figures/figure2c.png", height = 4.67, width = 4, units = "in")

# Figure 2d: N fertilizer consumption by scenario
Nfert_global <-getQuery(SAMout,"fertilizer consumption by crop type") %>%
  filter(sector != "Exports_fertilizer",
         scenario %in% Fig2_scenarios,
         year %in% modelfuture) %>%
  group_by(scenario,year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# Ancillary calculations for Figure 2d
print(paste0("YLD Nfert increase from REF: ",
             round(Nfert_global$value[Nfert_global$scenario == "YLD" & Nfert_global$year == 2100] /
                     Nfert_global$value[Nfert_global$scenario == "REF" & Nfert_global$year == 2100] - 1,
                   digits = 3)))
print(paste0("FLX Nfert delta from REF: ",
             round(Nfert_global$value[Nfert_global$scenario == "FLX" & Nfert_global$year == 2100] /
                     Nfert_global$value[Nfert_global$scenario == "REF" & Nfert_global$year == 2100] - 1,
                   digits = 3)))

# Generate Figure 2d
ggplot(Nfert_global, aes(x = year, y = value)) +
  geom_line(aes(color = scenario)) +
  scale_color_manual(values = scenario_colors) +
  theme_bw() +
  xlab("") +
  ylab("Mt N") +
  ggtitle("d") +
  theme(plot.title = element_text(vjust = -7, hjust = 0.03)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.84, 0.2))
ggsave("figures/figure2d.png", height = 3.5, width = 3, units = "in")


