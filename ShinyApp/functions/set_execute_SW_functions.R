#' @param lat logical. TRUE means future simulations will be executed.
#' @param lng logical. TRUE means future simulations will be executed.
#' @param futuresim logical. TRUE means future simulations will be executed.
#' @param soils A list from the input$clickmap of lat & lng
#' @param sand logical. TRUE means future simulations will be executed.
#' @param clay logical. TRUE means future simulations will be executed.
#' @param comp logical. TRUE means future simulations will be executed.
#' @param trees logical. TRUE means future simulations will be executed.
#' @param shrubs logical. TRUE means future simulations will be executed.
#' @param grasses logical. TRUE means future simulations will be executed.
#' @param forbs logical. TRUE means future simulations will be executed.
#' @param bf logical. TRUE means future simulations will be executed.

set_execute_SW <- function(lat, lng, futuresim,
                           dir ,
                           soils, sand = 33, clay = 33,
                           comp, trees = 0, shrubs = 0.5,
                           grasses = 0.5, forbs = 0, bg = 0,
                           verbose = TRUE){

  lat <- as.numeric(lat)
  lng <- as.numeric(lng)

  sand <- as.numeric(sand)
  clay <- as.numeric(clay)

  curr_year <- lubridate::year(Sys.Date())
  ################### ----------------------------------------------------------
  # Part 1 - Getting and formatting weather data for Current/Historical runs
  ################### ----------------------------------------------------------
  if(verbose) print(paste("Formatting Historical Weather Data", Sys.time()))

  weath <- get_gridMET_data(lat, lng, curr_year,  dir)
  #lastWeatherDate <- wdata[[2]]
  weath <- weath[[1]]

  ################### ----------------------------------------------------------
  # Part 2 - Sets soils and veg and other ....
  ################### ----------------------------------------------------------
  sw_in0 <- rSOILWAT2::sw_exampleData # baseline data

  # set whether soils should be extracted from 250m data or chosen by user
  sw_in0 <- set_soils(sw_in0, soils, sand, clay)
  # ExtractSoilDataFromISRICWISE30secV1a_Global
  # predict composition from climate
  sw_in0 <- set_comp(sw_in0, comp, trees, shrubs, grasses, forbs, bg, weath)

  # set latitude. Used in GISSM calculations
  rSOILWAT2::swSite_IntrinsicSiteParams(sw_in0)[["Latitude"]] <- lat * pi/180

  # set other params and flags
  rSOILWAT2::swSite_SoilTemperatureFlag(sw_in0) <- FALSE
  rSOILWAT2::swCarbon_Use_Bio(sw_in0) <- FALSE
  rSOILWAT2::swCarbon_Use_WUE(sw_in0) <- FALSE

  ################### ----------------------------------------------------------
  # Part 3 - Set up for outputs
  ################### -----------------------------------------------------------

  # Soils info formatting ----------------------------------------------------
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

  ################### ----------------------------------------------------------
  # Part 4 - Run Historical Soilwat and get outputs
  ################### -----------------------------------------------------------

  # --------------------------------------------------------------------------
  # Run 1 - with gridMET  data ------------------------------------
  # --------------------------------------------------------------------------
  if(verbose) print(paste('Running Historical', Sys.time()))

  rSOILWAT2::swYears_StartYear(sw_in0) <- 1980
  rSOILWAT2::swYears_EndYear(sw_in0) <- curr_year

  sw_out1 <- rSOILWAT2::sw_exec(inputData = sw_in0,
                                weatherList = weath, quiet = FALSE)

  if(verbose) print(paste('Getting Output', Sys.time()))
  if(verbose) print(object.size(sw_out1))

  sw_out_hist <- get_output(sw_out = sw_out1, soils_info, soils_info_avg,
                            Scenario = 'Current')

  ################### ----------------------------------------------------------
  # Part 5 - Run Future Soilwat and get outputs
  ################### -----------------------------------------------------------
  if(futuresim == 1) {

    if(verbose) print(paste('Running Futures', Sys.time()))

    rSOILWAT2::swYears_StartYear(sw_in0) <- 2020
    rSOILWAT2::swYears_EndYear(sw_in0) <- 2099

    # --------------------------------------------------------------------------
    # Runs 2: Parallel -  Future scenario data --------------------------------
    # --------------------------------------------------------------------------
    lng2 <- lng + 360
    #source - https://climate.northwestknowledge.net/MACA/data_catalogs.php
    GCMs <- c( 'HadGEM2-CC365_r1i1p1_rcp45', 'bcc-csm1-1_r1i1p1_rcp45',
               'CNRM-CM5_r1i1p1_rcp45',  'MIROC5_r1i1p1_rcp45',
               #'NorESM1-M_r1i1p1_rcp45', 'GFDL-ESM2M_r1i1p1_rcp45',
               #'MRI-CGCM3_r1i1p1_rcp45', 'HadGEM2-ES365_r1i1p1_rcp45',
               #'CanESM2_r1i1p1_rcp45', 'CCSM4_r6i1p1_rcp45',
               'IPSL-CM5A-MR_r1i1p1_rcp45', 'CSIRO-Mk3-6-0_r1i1p1_rcp45',
               'MIROC-ESM_r1i1p1_rcp45',
               'HadGEM2-CC365_r1i1p1_rcp85', 'bcc-csm1-1_r1i1p1_rcp85',
               'CNRM-CM5_r1i1p1_rcp85',  'MIROC5_r1i1p1_rcp85',
               #'NorESM1-M_r1i1p1_rcp45', 'GFDL-ESM2M_r1i1p1_rcp45',
               #'MRI-CGCM3_r1i1p1_rcp45', 'HadGEM2-ES365_r1i1p1_rcp45',
               #'CanESM2_r1i1p1_rcp45', 'CCSM4_r6i1p1_rcp45',
               'IPSL-CM5A-MR_r1i1p1_rcp85', 'CSIRO-Mk3-6-0_r1i1p1_rcp85',
               'MIROC-ESM_r1i1p1_rcp85')


    indexes <- seq_along(GCMs)

    cores <- 14
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)

    toEXPORT <- c('get_weath_and_run_future', 'get_MACA_one_scenario', 
                  'get_MACA_one_variable', 'get_output')

    AllVars <- foreach::foreach(g = indexes,
                                 .combine = rbind,
                                 .export = toEXPORT,
                                 .packages = c('lubridate', "data.table")) %dopar%

       {
        rbind(do.call(get_weath_and_run_future, 
                      list(g, lat, lng2, GCMs, sw_in0,
                           soils_info, soils_info_avg)))
      }

    print(Sys.time())
    parallel::stopCluster(cl)

    ################### ----------------------------------------------------------
    # Part 6 - Format outputs
    ################### -----------------------------------------------------------

    AllVars <- rbind(sw_out_hist, AllVars)

    } else {

      AllVars <- sw_out_hist

    }

  #SeasonDF
  SeasonsDF <- data.frame(Month = c(1:12), Season = c(rep('Winter',2),
                                                      rep('Spring', 3),
                                                      rep('Summer', 3),
                                                      rep('Fall', 3),
                                                      rep('Winter', 1)))
  #Variables
  VarDF <- data.frame(variable = c('Shallow', 'Intermediate', 'Deep',
                                   'max_C', 'min_C', 'avg_C', 'ppt'),
                      variable2 = c('Shallow', 'Soil Moisture (SWP, -MPa)',
                                    'Deep', 'max_C', 'min_C',
                                    'Average Temperature (C)',
                                    'Precipitation (cm)'))

  AllVars2 <- merge(AllVars, SeasonsDF)
  AllVars2 <- merge(AllVars2, VarDF, by = 'variable')
  AllVars2$variable <- NULL
  names(AllVars2)[7] <- 'variable'

  return(list(AllVars2))

}

set_soils <- function(sw_in, soils, sand, clay){

  # always have the layers of ISRIC - 10, 20, 40, 60, 80, 100, 150, 200
  layers <-  c(10, 20, 40, 60, 80, 100, 150, 200)
  soils_swdat <- rSOILWAT2::swSoils_Layers(sw_in)
  soils_swdat <- soils_swdat[1:length(layers), ]
  soils_swdat[,1] <- layers

  if(soils == 1){
    soils_swdat[,9] <- sand/100
    soils_swdat[,10] <- clay/100
  }

  if(soils == 2){
    # to do
  }

  # quick fix to evco and trco
  # also other adjustments to evo and trco
  for (i in 4:8) soils_swdat[,i] <- soils_swdat[,i]/sum(soils_swdat[,i])
  rSOILWAT2::swSoils_Layers(sw_in) <- soils_swdat

  return(sw_in)

}

set_comp <- function(sw_in, comp, trees, shrubs, grasses, forbs, bg, weath){

  if(comp == 1){
    rSOILWAT2::swProd_Composition(sw_in) <- c(grasses, shrubs, trees, forbs,
                                              bg)
  }

  if(comp == 2){

    clim1 <- rSOILWAT2::calc_SiteClimate(weatherList = weath, do_C4vars = TRUE)

    comp <- rSOILWAT2::estimate_PotNatVeg_composition(
      MAP_mm = 10 * clim1[["MAP_cm"]], MAT_C = clim1[["MAT_C"]],
      mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
      mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]]
    )

    rSOILWAT2::swProd_Composition(sw_in) <- c(comp$Rel_Abundance_L1[4],
                                              comp$Rel_Abundance_L1[2],
                                              comp$Rel_Abundance_L1[1],
                                              comp$Rel_Abundance_L1[3],
                                              comp$Rel_Abundance_L1[5])
  }

  return(sw_in)

}

get_weath_and_run_future <- function(g, lat, lng2, GCMs, sw_in0, 
                                     soils_info, soils_info_avg) {
  
  weath <- get_MACA_one_scenario(lat = lat, lng = lng2,
                                 url_main = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_',
                                 sc = GCMs[g])

  sw_out2 <- rSOILWAT2::sw_exec(inputData = sw_in0,
                                weatherList = weath, quiet = FALSE)
  
  output <- get_output(sw_out2, soils_info, soils_info_avg,
                       Scenario = GCMs[g])
  
  return(output)
}
