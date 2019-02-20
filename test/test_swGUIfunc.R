
rm(list=ls(all=TRUE))
devtools::load_all(pkg = '~/Documents/Git/rSFSW2/')
devtools::load_all(pkg = '~/Documents/Git/rSOILWAT2/')

library(rSFSW2)
setwd("/Users/candrews/Documents/Git/longtermdroughtsimulator/ShinyApp")
source("functions/swGUIfuncv2.R")
source("functions/MiscFunctions.R")

#debug(rSFSW2:::sw_dailyC4_TempVar)
#debug(set_execute_SW)
#debug(set_IM)
#debug(set_future)
#debug(set_soils)
#debug(set_comp)
#debug(rSFSW2::simulate_SOILWAT2_experiment)
#debug(rSFSW2::populate_rSFSW2_project_with_data)
#debug(rSFSW2::make_dbOutput)
#debug(rSFSW2:::recreate_dbWork)

#debug(rSFSW2:::local_weatherDirName)
#debug(rSFSW2:::calc_SiteClimate)
#debug(rSFSW2:::simTiming_ForEachUsedTimeUnit)
#debug(rSFSW2:::sw_dailyC4_TempVar)
#undebug(rSOILWAT2::dbW_getWeatherData)

#debug(rSFSW2::dbOutput_create_Design)
#debug(create_job_df)
#debug(rSFSW2::create_dbWork)

debug(rSOILWAT2::swYears_StartYear)

delete_test_output2(file.path(getwd(), "data/rSFSW2_ProjectFiles"))

#### Inputs
fmetaP <- "data/rSFSW2_ProjectFiles/SFSW2_project_descriptions.rds"
lat <- 35.5567
lng <- -111.6518
futuresim <- 1
soils <- 2
sand <- 0.33
clay <- 0.33
comp <- 1


set_execute_SW(lat, lng, futuresim,
               soils, sand, clay,
               comp, trees, shrubs, grasses, forbs, bg)

#delete_test_output2(file.path(getwd(), "data/rSFSW2_ProjectFiles"))
