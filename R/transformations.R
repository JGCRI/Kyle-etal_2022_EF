# loops for radar plots

# v2: only 5 scenarios

library(RColorBrewer)


gcam_home = "/Users/niaz981/Downloads/Modelling/GCAM/gcam_learning/gcam-core/"
metaregions <- read_csv('inputs/metaregions.csv')
#metaregionID <- data.frame(mr2 = unique(metaregions$mr2), GCAM_region_ID = c(1,2,3,6,7,10,11,12))
metaregionID <- data.frame(mr2 = unique(metaregions$mr2), GCAM_region_ID = c(1,2,3,4,5,6,7,8)) %>%
  mutate(mr2_cor=recode(mr2, "North_Am"='North America', 'South_Am'='South America', 'E_Asia'='East Asia','SE_Asia'='South East Asia','CS_Asia'='Central South Asia'))# dummy IDs just for plotting, correcting names
GCAMreg <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/common/GCAM_region_names.csv"),skip=6)
basin_glu <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/water/basin_to_country_mapping.csv"), skip=7)
iso_reg <- read_csv(paste0(gcam_home,"input/gcamdata/inst/extdata/common/iso_GCAM_regID.csv"),skip=6) %>%
  rename(country=country_name)


###
# transformations
###
rgbvals=c('#2f6c20','#f2ef54','#c94711')

#colors_border=c('#4477AA','#66CCEE','#228833','#ccbb44','#ee6677','#aa3377','#bbbbbb','#000000')
#colors_border=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") #Set1
#colors_border=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666") # dark2
colors_border=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00") # set1 5 sceanrios


#scens_main<-c('Reference','Dietary Transition','Pot Yld','Reference 45','PotYld Diet 45')
#scens_main <- c('Reference','Dietary Transition','Potential Yeild','Carbon Tax','YLD_FLX', 'YLD_CO2', 'YLD_FLX_CO2')
#scens_main <- names(SAMout)


# add already written data
region_vals <- read_csv('./outdata/region_vals_5scen.csv')
SAMthresh <- read_csv('outdata/SAM_thresholds.csv')

region_vals %>%  select(-Unit) %>%
  pivot_wider(names_from = variable, values_from = value)  -> varswide


##thresholds

SAMthresh -> thresh_tr

thresh_tr %>%
  filter(variable=="GHG") %>%
  mutate(green=-log10(green),red=-log10(red))->ghgtr

thresh_tr %>%
  filter(variable%in%c('TROP','AGDP')) %>%
  mutate(green=log10(green),red=log10(red))->agtrop

thresh_tr %>%
  filter(variable=="LCC") %>%
  mutate(green=-log10(green+0.23),red=-log10(red+0.23))->LCCthr

thresh_tr %>%
  filter(variable%in% c("Nsur","Psur"))%>%
  mutate(green=-green,red=-red)->NPsurtrans

thresh_tr %>%
  filter(variable%in%c('RSH',"RSE"))->Resil

trans_thresholds<-ghgtr %>%
  bind_rows(agtrop) %>%
  bind_rows(LCCthr) %>%
  bind_rows(NPsurtrans) %>%
  bind_rows(Resil) %>%
  select(-trans)

#write_csv(trans_thresholds, 'outdata/SAM_thresholds_trans.csv')

# minimum negative values
# forest_frac = -0.224


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
  select(scenario,region,year,GCAM_region_ID,SUSI,Psur,Nsur,LCC,GHG,AGDP,TROP,RSH,RSE)%>%
  pivot_longer(cols=c(SUSI,Psur,Nsur,LCC,GHG,AGDP,TROP,RSH,RSE),
               names_to='variable',values_to='value_trans')->varstrans



###
# distributions
###


varstrans %>%
  group_by(variable) %>%
  summarize(min=quantile(value_trans,na.rm=T,probs = 0.1),
            max=quantile(value_trans,na.rm=T,probs = 0.9)) -> varsbounds # 10 and 90 percentiles as bounds for scaling

varstrans %>%
  left_join(varsbounds, by = "variable") %>%
  mutate(valnorm=(value_trans-min)/(max-min)*100) -> norm_vars # normalized values as a percentage

norm_vars %>%
  select(scenario,year,GCAM_region_ID,variable, valnorm)%>%
  mutate(valnorm=if_else(valnorm>0, if_else(valnorm>100,100,valnorm), 0)) %>% # not ideal
  pivot_wider(names_from="variable", values_from="valnorm",values_fn=first) -> vals_spider

scenarios_order <- c("REF","YLD","FLX","CO2","YLD_FLX_CO2")
vals_spider <- vals_spider[ order(match(vals_spider$scenario, scenarios_order)),] #reordering scenarios to look nice in plots

# individual region_year plot

#input year and region here: consider making it a function
vals_spider %>% filter(year==2050, GCAM_region_ID==15) %>%
  select(-year,-GCAM_region_ID) %>%
  as.data.frame()->plotcheck

rownames(plotcheck)<-plotcheck$scenario
plotcheck<-plotcheck[,2:ncol(plotcheck)] # got rid of scenario heading

dataplot<-rbind(rep(100,ncol(plotcheck)),rep(0,ncol(plotcheck)), plotcheck) # included 0-100 as extreme, however note that 0 and 100 are actually 10, 90 percentiles

# plot with default options:
radarchart( dataplot, axistype=1,
            pcol=colors_border,
            plty=1,
            plwd=3)
legend(x=1.2, y=1.3, legend = c("REF","YLD","FLX","CO2","YLD_FLX_CO2"), bty = "n", pch=20 , col=colors_border)
#legend(x="bottom", legend = c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2"), bty = "n", pch=20 ,  horiz = TRUE, col=colors_border) # for panel legend

#color_ref = c("#1B9E77" ,"","","","","","","")
#color_ref = c("#1B9E77" ,"#ffffff","#ffffff","#ffffff","#ffffff","#ffffff","#ffffff","#ffffff")
color_ref = c("red" ,"#ffffff","#ffffff","#ffffff","#ffffff","#ffffff","#ffffff","#ffffff")
# plots in loop for all regions and years
radarSAM<-function(df,yr,reg){
  df %>% filter(year==yr,GCAM_region_ID==reg) %>%
    select(-year,-GCAM_region_ID) %>%
    as.data.frame()->selectdf
  rownames(selectdf)<-selectdf$scenario
  selectdf<-selectdf[,2:ncol(selectdf)]
  plotdf<-rbind(rep(100,ncol(selectdf)),rep(0,ncol(selectdf)),selectdf)
  radarchart(plotdf,
             axistype = 0,
             pcol=colors_border,
             pfcol = scales::alpha(color_ref, 0.06),
             plty=1,
             #title = paste0(GCAMreg$region[reg]," ",yr),  # for regions
             title = paste0(metaregionID$mr2_cor[reg]," ",yr), # for meta regions
             cglcol="gray30",
             plwd=3
             )
  #legend(x=1.5, y=1.3, legend = c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2"), bty = "n", pch=20, col=colors_border)
}
radarSAM(vals_spider, 2050,2)

# all regions: make regions string, for region in regions plot and save in a list and arrange, years 2020, 2050, 2100

plot_years <- c(2020, 2050, 2100)
plot_ids <- c(GCAMreg$GCAM_region_ID)

# confirm the title in function
for (id in plot_ids){ #plot_ids[1:2]
  for (pyr in plot_years){
    mypath <- file.path("figures","spiders", "region_year_all" ,paste(GCAMreg$region[id],"_", pyr, ".png", sep = ""))
    png(file=mypath, width = 600, height = 600, res = 100)
    #pdf(file=mypath) # also change file extension
    radarSAM(vals_spider, pyr, id)
    dev.off()
  }
  }
dev.off()

# region panels
for (id in plot_ids){ #plot_ids[1:2]
  mypath <- file.path("figures","spiders", "region_year_panels" ,paste(GCAMreg$region[id],".png", sep = ""))
  png(file=mypath, width = 1500, height = 600, res = 150)
  par(mfrow=c(1,3), mar=c(2,2,2,0))
  #pdf(file=mypath) # for publication quality, also change file extension
  for (pyr in plot_years){
    radarSAM(vals_spider, pyr, id)
    if (pyr == length(plot_years)){
      dev.off()
    }
  }
  # legend(x=1.5, y=1.3, legend = c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2"), bty = "n", pch=20, col=colors_border) # adjust figure size to bring in legend
  dev.off()
}

# plot for meta regions: transform data to generate new indicators,

vals_spider %>% left_join(metaregions, by = "GCAM_region_ID") %>%
pivot_longer(cols=c(SUSI,Psur,Nsur,LCC,GHG,AGDP,TROP,RSH,RSE),
             names_to='variable',values_to='value_long') %>%
  filter(year >=2020) %>%
  group_by(scenario, year, mr2, variable) %>%
  summarise(value_mr = mean(value_long,na.rm=T)) %>% ungroup() %>%
  pivot_wider(names_from = variable, values_from = value_mr) %>%
  left_join(metaregionID, by = "mr2") %>%
  select(!mr2 & !mr2_cor) -> vals_spider_mr

radarSAM(vals_spider_mr, 2050,1)

# meta region all
plot_ids_mr <- c(metaregionID$GCAM_region_ID)

for (id in plot_ids_mr){ #plot_ids_mr[1:2]
  for (pyr in plot_years){
    mypath <- file.path("figures","spiders", "metaregions_yr_all" ,paste(metaregionID$mr2[id],"_", pyr, ".png", sep = ""))
    png(file=mypath, width = 600, height = 600, res = 100)
    #pdf(file=mypath) # also change file extension
    radarSAM(vals_spider_mr, pyr, id)
    dev.off()
  }
}

# metaregions panels
for (id in plot_ids_mr){ #plot_ids[1:2]
  mypath <- file.path("figures","spiders", "metaregions_yr_panels" ,paste(metaregionID$mr2[id],".png", sep = ""))
  png(file=mypath, width = 1500, height = 500, res = 160)
  #mypath <- file.path("figures","spiders", "metaregions_yr_panels" ,paste(metaregionID$mr2[id],".pdf", sep = ""))
  #pdf(file=mypath, width = 10, height = 3.5) # for publication quality, also change file extension
  par(mfrow=c(1,3), mar=c(2,2,2,0))
  for (pyr in plot_years){
    radarSAM(vals_spider_mr, pyr, id)
    if (pyr == length(plot_years)){
      dev.off()
    }
 }
  # legend(x=1.5, y=1.3, legend = c("REF","YLD","FLX","CO2","FLX_CO2","YLD_CO2","YLD_FLX","YLD_FLX_CO2"), bty = "n", pch=20, col=colors_border) # adjust figure size to bring in legend
  dev.off()
}

# one big panel of all meta regions
mypath <- file.path("figures","spiders", "metaregions_yr_panels",paste("all.png", sep = ""))
png(file=mypath, width = 3000, height = 2000, res = 180)
#mypath <- file.path("figures","spiders", "metaregions_yr_panels",paste("all.pdf", sep = ""))
#pdf(file=mypath, width = 18, height = 12) # for publication quality, also change file extension
par(mfrow=c(4,6), mar=c(2,2,2,0))
for (id in plot_ids_mr){ #plot_ids[1:2]
  for (pyr in plot_years){
    radarSAM(vals_spider_mr, pyr, id)
  }
}
dev.off()

radarSAM(vals_spider, 2050,2)
legend(x=-2.5,y=-1.1, legend = c("REF","YLD","FLX","CO2","YLD_FLX_CO2"), bty = "n", pch=20 ,  horiz = TRUE, col=colors_border)


###
# scores
###

varstrans %>%
  left_join(trans_thresholds)%>%
  mutate(score=if_else(value_trans>green,
                       33.33*(value_trans-green)/(green-red)+66.66,
                       33.33*(value_trans-red)/(green-red)+33.33)) ->score_vars # this could be hard to explain



score_vars %>% filter(score>100) %>%
  mutate(score_norm=100)->scoreover

score_vars %>% filter(score<100) %>%
  mutate(score_norm=0)->scoreunder


score_vars %>% filter(score<100,score>0) %>%
  mutate(score_norm=score)%>%
  bind_rows(scoreover) %>%
  bind_rows(scoreunder)->scores_norm


scores_norm %>%
  select(scenario,year,GCAM_region_ID,variable, score_norm)%>%
  pivot_wider(names_from="variable", values_from="score_norm",values_fn=first)->scores_spider


scores_spider %>% filter(year==2050, GCAM_region_ID==15) %>%
  select(-year,-GCAM_region_ID) %>%
  as.data.frame()->plotcheck

rownames(plotcheck)<-plotcheck$scenario
plotcheck<-plotcheck[,2:ncol(plotcheck)]

dataplot<-rbind(rep(100,ncol(plotcheck)),rep(0,ncol(plotcheck)), plotcheck)

# plot with default options:
radarchart( dataplot  , axistype=1,
            pcol=colors_border,
            plty=1,
            plwd=3)
legend(x=1.25, y=1.4, legend = rownames(dataplot[-c(1,2),]),
       bty = "n", pch=20 , col=colors_border)


######
# water by basin
###

watbal %>%
  mutate(value_trans=-log10(value))->wat_trans

q05=quantile(wat_trans$value_trans,0.05)
q95=quantile(wat_trans$value_trans,0.95)

wat_trans%>%
  mutate(green=0,
         red=0.30103) %>%
  mutate(score=if_else(value_trans>green,
                       33.33*(value_trans-green)/(green-red)+66.66,
                       33.33*(value_trans-red)/(green-red)+33.33),
         valnorm=(value-q05)/(q95-q05)*100,
         variable="SUSI") ->wat_score





varstrans %>%
  left_join(varsbounds, by = "variable") %>%
  mutate(valnorm=(value_trans-min)/(max-min)*100)->norm_vars

varstrans %>%
  group_by(variable) %>%
  summarize(min=quantile(value_trans,na.rm=T,probs = 0.05),
            max=quantile(value_trans,na.rm=T,probs = 0.95))->varsbounds

