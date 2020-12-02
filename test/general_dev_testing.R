library(tictoc)

# mian function test
source('ShinyApp/functions/weather_functions.R')
source('ShinyApp/functions/set_execute_SW_functions.R')
source('ShinyApp/functions/getOutputs.R')
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
lng2<- -112.2 + 360
sw_in0 <- rSOILWAT2::sw_exampleData # baseline data
rSOILWAT2::swSite_SoilTemperatureFlag(sw_in0) <- FALSE
rSOILWAT2::swCarbon_Use_Bio(sw_in0) <- FALSE
rSOILWAT2::swCarbon_Use_WUE(sw_in0) <- FALSE
rSOILWAT2::swYears_StartYear(sw_in0) <- 2020
rSOILWAT2::swYears_EndYear(sw_in0) <- 2099

soils_info0 <- data.frame(depth_cm = c(1:200),
                          Depth = c(rep('Shallow', 20), rep('Intermediate', 80),
                                    rep('Deep',100)))

soils_info <- data.frame(sw_in0@soils@Layers)
soils_info <- soils_info[,c('depth_cm', 'sand_frac', 'clay_frac')]
soils_info$width <- diff(c(0, soils_info$depth_cm))
soils_info <- merge(soils_info, soils_info0, by = 'depth_cm')
soils_info$variable <- paste0('Lyr_',1:dim(soils_info)[1])

# get weighted average of sand and clay
soils_info_avg <- data.table::setDT(soils_info)[,.(sand = weighted.mean(sand_frac, width),
                                                   clay = weighted.mean(clay_frac, width)), .(Depth)]
soils_info <- soils_info[,c('Depth', 'variable', 'width')]


get_weath_and_run_future <- function(g, lat, lng2, GCMs, sw_in0, soils_info, soils_info_avg) {
    print(Sys.time(), 'begin weath')
    weath <- get_MACA_one_scenario(lat = lat, lng = lng2,
                                   url_main = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_',
                                   sc = GCMs[g])
    print(Sys.time(), 'end weath')
    
    sw_out2 <- rSOILWAT2::sw_exec(inputData = sw_in0,
                                  weatherList = weath, quiet = FALSE)
    
    output <- get_output(sw_out2, soils_info, soils_info_avg,
                         Scenario = GCMs[g])
    
    return(output)
}




tic('mccollect')
a <- mcparallel(get_weath_and_run_future(1, lat, lng2, GCMs, sw_in0, soils_info, soils_info_avg))
b <- mcparallel(get_weath_and_run_future(2, lat, lng2, GCMs, sw_in0, soils_info, soils_info_avg))
c <- mcparallel(get_weath_and_run_future(3, lat, lng2, GCMs, sw_in0, soils_info, soils_info_avg))
d <- mcparallel(get_weath_and_run_future(4, lat, lng2, GCMs, sw_in0, soils_info, soils_info_avg))
res <- mccollect(list(a,b, c, d), wait = TRUE)
toc()

tic('loop')
results <- data.frame()
for(g in 1:4){
  print(g)
  x <- get_weath_and_run_future(g, lat, lng2, GCMs, sw_in0, soils_info, soils_info_avg)
  results <- rbind(results,x)
}
toc()

toEXPORT <- c('get_weath_and_run_future')

tic('foreach')
AllVars <- foreach::foreach(g = 1:4,
                            .combine = rbind,
                            .export = toEXPORT,
                            .packages = c('lubridate', "data.table")) %dopar%
  
  {
    rbind(do.call(get_weath_and_run_future, 
                  list(g, lat, lng2, GCMs, sw_in0,
                       soils_info, soils_info_avg)))
  }
toc()



for(g in seq_along(GCMs[1:5])) {
  tic("datatest")
  print(g)
  data <- get_MACA_one_scenario(lat, lng, url_main, sc)
  toc()
}

