
# value of agriculture products (incl. animal production and forestry) ----

cropprod <- getQuery(SAMout, "ag production by subsector (land use region)")
agprice <- getQuery(SAMout, "ag commodity prices")
anprod <- getQuery(SAMout, "meat and dairy production by tech")
anprice <- getQuery(SAMout, "meat and dairy prices")

cropprod %>% rename("GCAM_commodity" = sector) -> cropprod

agprice %>% select(-Units) %>%
  rename("GCAM_commodity" = sector, "price" = value) %>%
  left_join(cropprod, by = c("scenario", "region", "GCAM_commodity", "year")) %>%
  rename("prod" = value) %>%
  group_by(scenario, region, GCAM_commodity, year) %>%
  summarize(price = mean(price, na.rm = T),
            prod = sum(prod, na.rm = T)) %>%
  ungroup() -> ag_commod

anprice %>% select(-Units) %>%
  rename("output" = sector, "price" = value) %>%
  left_join(anprod %>% select(-Units),
            by = c("scenario", "region", "output", "year")) %>%
  rename("prod" = value) %>%
  group_by(scenario, region, output, year) %>%
  summarize(price = mean(price, na.rm = T),
            prod = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  rename("GCAM_commodity" = output) -> an_commod

ag_commod %>% bind_rows(an_commod) %>%
  mutate(price2010 = gdp_deflator(2010, 1975) * price,
         #price per kg
         value = price2010 * prod * 1e9) %>%
  filter(year %in% 2010:2100) -> ag_an_value


###
# traded fractions ----
###
tradesect <- getQuery(SAMout, "traded outputs by subsector")
#exports from other regions are stored in USA as subsector "[region] traded [commodity]"
tradesect %>% filter(region == "USA") %>%
  filter(str_detect(sector, 'traded')) %>%
  separate(output, sep = " ", into = c("traded", "output"), extra = "merge") %>%
  filter(output %in% c(tolower(allcrops), tolower(animals))) %>%
  select(-traded, -region)  -> all_traded

all_traded %>%
  separate(subsector, sep = " ",
           into = c('first', 'second', 'third', 'fourth')) %>%
  filter(second == "traded") %>%
  rename(region = first) %>%
  select(Units, scenario, region, output, year, value) -> regions1


all_traded %>%
  separate(subsector, sep = " ",
           into = c('first', 'second', 'third', 'fourth')) %>%
  filter(third == "traded") %>%
  mutate(region = paste(first, second, sep = ' ')) %>%
  select(Units, scenario, region, output, year, value) -> regions2


all_traded %>%
  separate(subsector, sep = " ",
    into = c('first', 'second', 'third', 'fourth', 'fifth'),
    extra = "merge") %>%
  filter(!is.na(fifth)) %>%
  mutate(region = paste(first, second, third, fourth, sep = ' ')) %>%
  select(Units, scenario, region, output, year, value) %>%
  bind_rows(regions2) %>%
  bind_rows(regions1) %>%
  rename(exported = value) -> exports_agan


tradesect %>%
  filter(output %in% regional_ag) %>%
  mutate(output = substr(output, 10, nchar(output))) %>%
  group_by(scenario, region, output, year) %>%
  summarize(nontrade = sum(value)) %>%
  ungroup() %>%
  mutate(output = tolower(output)) -> nontrade

nontrade %>%
  left_join(exports_agan, by = c("scenario", "region", "year", "output")) %>%
  mutate(totalprod = exported + nontrade) %>%
  rename("GCAM_commodity" = output) -> prod_trade


ag_an_value %>% select(scenario, region, GCAM_commodity, year, price2010) %>%
  mutate(GCAM_commodity = tolower(GCAM_commodity)) %>%
  right_join(prod_trade, by = c("scenario", "region", "GCAM_commodity", "year")) %>%
  filter(year %in% modelfuture) %>%
  mutate(totval = totalprod * price2010, exportval = exported * price2010) ->
  exports_commod

exports_commod %>%
  group_by(scenario, region, year) %>%
  summarize(exportval = sum(exportval, na.rm = T),
            totalval = sum(totval, na.rm = T)) %>%
  mutate(value = exportval / totalval,
         variable = "Export_pct",
         Unit = "%") %>%
  select(scenario, region, year, variable, value, Unit) -> exportpct

####
# ag labor productivity ----
###

pop_agemploy_pred <- read_csv('./inputs/pop_agemploy_pred.csv')

ag_an_value %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(value)) -> agval_region

agval_region %>%
  left_join(pop_agemploy_pred, by = c("region", "year")) %>%
  drop_na(GCAM_region_ID) %>%
  mutate(value = if_else(is.na(totagemploy_pred), NA_real_, value / (totagemploy_pred)),
    variable = "AgLabProd", Unit = "$ per ag. worker") %>%
  select(scenario, region, year, variable, value, Unit) -> AgLabProd

###
# H-index ----
###
ag_an_value %>%
  filter(GCAM_commodity %in% allcrops) %>%
  group_by(scenario, region, year) %>%
  summarize(totval = sum(value)) %>%
  ungroup() -> totvals

totvals %>%
  left_join(filter(ag_an_value, GCAM_commodity %in% allcrops),
             by = c("scenario", "region", "year")) %>%
  select(scenario, region, year, GCAM_commodity, value, totval) %>%
  mutate(propval = if_else(totval == 0, 0, value / totval)) %>%
  mutate(logprop = log(propval), indexval = propval * logprop) -> proportions

proportions %>%
  filter(year %in% modelfuture) %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(indexval, na.rm = T) * (-1)) %>%
  mutate(variable = "Hindex", Unit = "--") %>%
  select(scenario, region, year, variable, value, Unit) %>%
  ungroup() ->
  Hindex


# top 3 ----
ag_an_value %>%
  filter(GCAM_commodity %in% allcrops) %>%
  group_by(scenario, region, year) %>%
  top_n(n = 3, wt = value) %>%
  group_by(scenario, region, year) %>%
  summarize(topval = sum(value)) %>%
  ungroup() -> top3

totvals %>% left_join(top3, by = c("scenario", "region", "year")) %>%
  mutate(top3frac = topval / totval) -> shareoftop3


shareoftop3 %>%
  mutate(variable = "TOP") %>%
  rename(value = top3frac) %>%
  select(scenario, region, year, variable, value) -> share_top3

# percentiles for green/red
# > summary(Hindex$value)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -4.274   3.033   3.406   3.268   3.707   4.470
# > summary(shareoftop3$top3frac)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.3955  0.5335  0.6058  0.6095  0.6596  0.9223


rm(totvals,proportions, agval_region, exports_commod,
   prod_trade, nontrade, tradesect, exports_agan,
   regions2, all_traded, regions1, ag_commod,
   an_commod, agprice, anprice, anprod)



###
# price volatility ----
###
#
# ag_an_value %>%
#   filter(output!="Forest") %>%
#   group_by(scenario,region,output) %>%
#   summarize(meanprice=mean(price2010,na.rm=T),
#             sdprice=sqrt(var(price2010,na.rm=T)),
#             CVprice=sdprice/meanprice) -> ag_an_cv
#
# agval_region %>%
#   rename(total=value) %>%
#   left_join(ag_an_value, by = c("scenario", "region", "year")) %>%
#   mutate(wt=value/total) %>%
#   select(-c(value,total)) %>%
#   filter(output!="Forest",year %in% modelfuture) %>%
#   left_join(ag_an_cv, by = c("scenario", "region", "output")) %>%
#   mutate(year=if_else(year>2030,2050,2020)) %>%
#   group_by(scenario,region,year) %>%
#   summarize(value=sum(wt*CVprice)) %>%
#   mutate(variable="PVOL", Unit="--") %>%
#   select(scenario,region,year,variable,value,Unit)->pvol
#
# # > summary(pvol$value)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# # 0.03881 0.10594 0.14566 0.16277 0.18922 0.58220
