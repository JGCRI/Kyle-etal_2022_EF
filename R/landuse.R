
landbycrop<-getQuery(SAMout,"land allocation by crop") %>%
  mutate(landleaf = sub("C4|Tree", "", landleaf))

cropland2015<-landbycrop %>% filter(landleaf %in% allcrops,
                                    year == 2015) %>%
  group_by(scenario, region) %>%
  summarize(cropland=sum(value)) %>%
  ungroup()

forest<-landbycrop %>% filter(grepl("Forest", landleaf)) %>%
  select(-Units, -landleaf) %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, region) %>%
  mutate(forest_base = value[year == 2015]) %>%
  ungroup() %>%
  filter(year %in% modelfuture) %>%
  mutate(nyear = year - 2015,
         forest_change = (forest_base - value) / nyear) %>%
  left_join(cropland2015, by = c("scenario", "region")) %>%
  mutate(forest_frac = forest_change / cropland) ->
  forest_change

forest_change %>% select(scenario,region,year,forest_frac) %>%
  mutate(variable="forest_frac",Unit='pct') %>%
  rename(value=forest_frac) %>%
  select(scenario,region,year,variable,value,Unit) -> forest_plot

rm(forest,forest_change)

