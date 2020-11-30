library(tictoc)

# mian function test
source('ShinyApp/functions/weather_functions.R')
lat <- 35
lng <- -112
soils <- 1
sand <- 33
clay <- 33
comp <- 1
trees <- forbs <- bg <- 0
shrubs <- .5
grasses <- .5
futuresim <- 1

curr_year <- lubridate::year(Sys.Date())

set_execute_SW(lat, lng, futuresim,
                           dir = "../../www.northwestknowledge.net/metdata/data/",
                           soils, sand = 33, clay = 33,
                           comp, trees = 0, shrubs = 0.5, 
                           grasses = 0.5, forbs = 0, bg = 0)

# MACA speed test
url_main <- 
  'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_'

GCMs <- c( 'HadGEM2-CC365_r1i1p1_rcp45', 'bcc-csm1-1_r1i1p1_rcp45',
           'CNRM-CM5_r1i1p1_rcp45',  'MIROC5_r1i1p1_rcp45',
           'NorESM1-M_r1i1p1_rcp45', 'GFDL-ESM2M_r1i1p1_rcp45',
           'MRI-CGCM3_r1i1p1_rcp45', 'HadGEM2-ES_r1i1p1_rcp45')
# 'CanESM2_r1i1p1_rcp45', 'CCSM4_r6i1p1_rcp45', 
#'IPSL-CM5A-MR_r1i1p1_rcp45', 'CSIRO-Mk3-6-0_r1i1p1_rcp45',
#'MIROC-ESM_r1i1p1_rcp45')

sc <- GCMs[1]
lat <- 35.1
lng <- -112.2 + 360

for(g in seq_along(GCMs[1:5])) {
  tic("datatest")
  print(g)
  data <- get_MACA_one_scenario(lat, lng, url_main, sc)
  toc()
}

