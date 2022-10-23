# query model output xml database using the rgcam package
# To get the rgcam package, run:
# install_github('JGCRI/rgcam')
library(rgcam)

gcam_home='/Users/D3P747/Desktop/stash/sam/'
file.path <- paste0(gcam_home,'output')
dbFile <- c('database_basexdb')
conn <- localDBConn(dbPath=file.path, dbFile, migabble = FALSE, maxMemory="8g")

prj.name <- 'outdata/SAM-matrix.dat'
scenario.names <- c('REF', 'YLD','FLX','YLD_FLX','CO2', 'YLD_CO2', 'FLX_CO2', 'YLD_FLX_CO2')
query.fname  <- 'queries/SAM_querylist.xml'

prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

# The nonco2 query is performed separately, by scenario, in order to avoid running into memory issues
query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'REF'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'YLD'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'FLX'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'YLD_FLX'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'CO2'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'YLD_FLX_CO2'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'YLD_CO2'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

query.fname  <- 'queries/nonco2.xml'
scenario.names <- 'FLX_CO2'
prj <- addScenario(conn, prj.name, scenario=scenario.names, clobber=FALSE, queryFile=query.fname)

