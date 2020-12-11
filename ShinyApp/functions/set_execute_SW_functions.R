#' @param lat numeric TRUE means future simulations will be executed.
#' @param lng numeric TRUE means future simulations will be executed.
#' @param futuresim logical. TRUE means future simulations will be executed.
#' @param soils numeric. 1 means user set soils. 2 means soils from POLARIS.
#' @param sand numeric. Percent sand.
#' @param clay numeric. Percent clay.
#' @param comp numeric. 1 means user set comp. 2 means comp predicted from climate.
#' @param trees numeric. Fraction of cover that is trees
#' @param shrubs numeric. Fraction of cover that is shrubs
#' @param grasses numeric. Fraction of cover that is grasses.
#' @param forbs numeric. Fraction of cover that is forbs.
#' @param bg numeric. Fraction of cover that is bareground.
#' @param curr_year numeric. Current year
#' @param verbose logical. 

set_execute_SW <- function(lat, lng, futuresim,
                           dir,
                           soils, sand = 33, clay = 33,
                           comp, trees = 0, shrubs = 0.5,
                           grasses = 0.5, forbs = 0, bg = 0,
                           curr_year, verbose = TRUE){

  lat <- as.numeric(lat)
  lng <- as.numeric(lng)

  sand <- as.numeric(sand)
  clay <- as.numeric(clay)
  
  layers <-  c(10, 20, 40, 60, 80, 100, 150, 200)
  tr_layers <- c(1, 1, 2, 2, 3, 3, 3, 3)

  ################### ----------------------------------------------------------
  # Part 1 - Getting and formatting weather data for Current/Historical runs
  ################### ----------------------------------------------------------
  if(verbose) print(paste("Formatting Historical Weather Data", Sys.time()))

  weath <- get_gridMET_data(lat, lng, (curr_year - 1),  
                            dir)

  ################### ----------------------------------------------------------
  # Part 2 - Sets up SW including soils and veg
  ################### ----------------------------------------------------------
  #sw_in0 <- new("swInputData") # baseline data
  sw_in0 <- rSOILWAT2::sw_exampleData
  # set latitude. Used in GISSM calculations
  rSOILWAT2::swSite_IntrinsicSiteParams(sw_in0) <- c(Longitude = lng,
                                                     Latitude = lat,
                                                     Altitude = NA,
                                                     Slope = 0,
                                                     Aspect = NA )

  # set monthly atmospheric conditions used in penman PET - TO FIX
  rH <- rep(50, 12) # relative humidity [%]
  ws <- rep(2, 12) # wind speed [m/s]
  sc <- rep(30, 12) # sky cover [%]
  # Assign monthly climate normals to rSOILWAT2 input object
  rSOILWAT2::swCloud_Humidity(sw_in0) <- rH
  rSOILWAT2::swCloud_WindSpeed(sw_in0) <- ws
  rSOILWAT2::swCloud_SkyCover(sw_in0) <- sc
  
  # set whether soils should be extracted from SSURGO / STATSGO data or chosen by user
  ## bd is always grabbed from grid 
  soils_df <- initialize_soils(sw_in0, soils, sand, clay, layers)
  print(soils_df)
  # set or predict composition from climate
  ## transpiration and monthly biomass is set for both options
  sw_in0 <- set_comp_roots(sw_in0, comp, trees, shrubs, grasses, 
                            forbs, bg, weath, soils_df)

  # Set transpration ------------------------------------------------------
  tr <- rSOILWAT2::prepare_TranspirationRegions(tr_lyrs = tr_layers)
  rSOILWAT2::swSite_TranspirationRegions(sw_in0) <- data.matrix(tr)
  # Make necessary adjustments based on soil depth and rooting profiles
  rSOILWAT2::swSite_TranspirationRegions(sw_in0) <- 
    rSOILWAT2::adjust_TranspirationRegions(sw_in0)
  
  # set other params and flags
  rSOILWAT2::swSite_SoilTemperatureFlag(sw_in0) <- FALSE
  rSOILWAT2::swCarbon_Use_Bio(sw_in0) <- FALSE # could turn this to true in fut.
  rSOILWAT2::swCarbon_Use_WUE(sw_in0) <- FALSE

  ################### ----------------------------------------------------------
  # Part 3 - Set up for outputs
  ################### -----------------------------------------------------------

  # Soils info formatting ----------------------------------------------------
  soils_info0 <- data.frame(depth_cm = c(1:200),
                            Depth = c(rep('Shallow', 20), 
                                      rep('Intermediate', 80),
                                      rep('Deep',100)))

  soils_info <- data.frame(sw_in0@soils@Layers)[,c('depth_cm', 'sand_frac', 'clay_frac')]
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
  rSOILWAT2::swYears_EndYear(sw_in0) <- (curr_year - 1)

  sw_out1 <- rSOILWAT2::sw_exec(inputData = sw_in0,
                                weatherList = weath, quiet = FALSE)

  if(verbose) print(paste('Getting Output', Sys.time()))

  sw_out_hist <- get_output(sw_out = sw_out1, soils_info, soils_info_avg,
                            Scenario = 'Current')

  ################### ----------------------------------------------------------
  # Part 5 - Run Future Soilwat and get outputs
  ################### -----------------------------------------------------------
  if(futuresim == 1) {

    if(verbose) print(paste('Running Futures', Sys.time()))

    rSOILWAT2::swYears_EndYear(sw_in0) <- 2099
    rSOILWAT2::swYears_StartYear(sw_in0) <- 2019
    
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

    cores <- 4
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
                                   'avg_C', 'ppt'),
                      variable2 = c('Shallow', 'Soil Moisture (SWP, -MPa)',
                                    'Deep', 'Average Temperature (C)',
                                    'Precipitation (cm)'))

  AllVars2 <- merge(AllVars, SeasonsDF)
  AllVars2 <- merge(AllVars2, VarDF, by = 'variable')
  AllVars2$variable <- NULL
  names(AllVars2)[7] <- 'variable'

  return(list(AllVars2))

}

#' @param sw_in S4 object. S4 object of class swInputData.
#' @param soils numeric. 1 is soils from STATSGO/SSURGO. 2 is user set soils.
#' @param sand numeric. Percent sand.
#' @param clay numeric. Percent clay.
#' @param layers numeric array. Depth of all soil layers.

initialize_soils <- function(sw_in, soils, sand, clay, layers) {
  
  # grab gridded soils first - will need bd & gravel for user set soils
  
  if (requireNamespace("rSW2exter")) {
    
    tmp <- rSOILWAT2::swSite_IntrinsicSiteParams(sw_in)
    
    suppressWarnings(
      soils_sda <- rSW2exter::extract_soils_NRCS_SDA(
        x = matrix(tmp[c("Longitude", "Latitude")], nrow = 1),
        method = "SSURGO_then_STATSGO", remove_organic_horizons = "at_surface", 
        replace_missing_fragvol_with_zero = "at_surface", 
        estimate_missing_bulkdensity = TRUE, 
        restrict_by_ec_or_ph = FALSE,
        impute = TRUE,
        progress_bar = FALSE,
        verbose = FALSE
    )
    )
  }
  
  # homogenize to soil layers 
  tmp <- grep("depth_L", colnames(soils_sda[["table_depths"]])) 
  
  soils_sda2 <- rSW2data::update_soil_profile(
    soil_layers = soils_sda[["table_depths"]][1, tmp, drop = FALSE], 
    requested_soil_layers = layers,
    soil_data = soils_sda[["table_texture"]],
    variables = c("dbovendry_L", "sandtotal_L", "claytotal_L", "fragvol_L"), 
    vars_exhaust = NULL,
    keep_prev_soildepth = FALSE,
    keep_prev_soillayers = FALSE
  )
  
  soil_new <- data.frame(rSOILWAT2::swSoils_Layers(sw_in)[0, ]) 
  ids <- seq_len(ncol(soils_sda2[["soil_layers"]]))

  if(soils == 1) {

    # format for SW
    # Soil layer depths
    tmp <- grep("depth_L", colnames(soils_sda2[["soil_layers"]])) 
    soil_new[ids, "depth_cm"] <- soils_sda2[["soil_layers"]][1, tmp]
    # Soil density, texture, and gravel
    tmp <- grep("dbovendry_L", colnames(soils_sda2[["soil_data"]]))
    soil_new[ids, "bulkDensity_g.cm.3"] <- soils_sda2[["soil_data"]][1, tmp]
    tmp <- grep("sandtotal_L", colnames(soils_sda2[["soil_data"]])) 
    soil_new[ids, "sand_frac"] <- soils_sda2[["soil_data"]][1, tmp] 
    tmp <- grep("claytotal_L", colnames(soils_sda2[["soil_data"]])) 
    soil_new[ids, "clay_frac"] <- soils_sda2[["soil_data"]][1, tmp]
    tmp <- grep("fragvol_L", colnames(soils_sda2[["soil_data"]]))
    soil_new[ids, "gravel_content"] <- soils_sda2[["soil_data"]][1, tmp]
    
    # if there are no soils that fall into the intermediate depth profile, 
    ## duplicate the deepest soil layer into the inter category (20 - 100)
    # if (soil_new[max(ids), "depth_cm"] <= 20) {
    #   
    #   # determine max layer
    #   ids3 <- ifelse(length(soil_new$depth_cm) == 1, 2, 1)
    #   
    #   if(ids3 == 1){
    #     soil_new[3,'depth_cm'] <- 40
    #   }
    #   
    #   if (ids3 == 2) {
    #     soil_new[2:3, 'depth_cm'] <- c(20, 40)
    #   }
    #   
    #   # repeat last layer
    #   soil_new[c(max(ids)+1):3, "bulkDensity_g.cm.3"] <- 
    #     soil_new[length(ids), "bulkDensity_g.cm.3"]
    #   
    #   soil_new[c(max(ids)+1):3, "gravel_content"] <- 
    #     soil_new[length(ids), "gravel_content"]
    #   
    #   soil_new[c(max(ids)+1):3, "sand_frac"] <- 
    #     soil_new[length(ids), "sand_frac"]
    #   
    #   soil_new[c(max(ids)+1):3, "clay_frac"] <- 
    #     soil_new[length(ids), "clay_frac"]
    # }
    
  }
  
  if(soils == 2) {
    soils_fixed <- data.frame( depth = layers,
                               bulkd = NA,
                               gravel = NA,
                               evco = NA,
                               trco_grass = NA,
                               trco_shrub = NA,
                               trco_tree = NA,
                               trco_forb = NA,
                               sand = sand,
                               clay = clay,
                               impermeability = NA,
                               soil_temp = NA
    )
    
    ids2 <- length(layers)
    
    tmp <- grep("dbovendry_L", colnames(soils_sda2[["soil_data"]]))
    soils_fixed[ids, "bulkd"] <- soils_sda2[["soil_data"]][1, tmp]
    # # repeat last layer
    # soils_fixed[c(max(ids)+1):ids2, "bulkd"] <- 
    #   soils_fixed[length(ids), "bulkd"]
    
    tmp <- grep("fragvol_L", colnames(soils_sda2[["soil_data"]]))
    soils_fixed[ids, "gravel"] <- soils_sda2[["soil_data"]][1, tmp]
    # repeat last layer
    # soils_fixed[c(max(ids)+1):ids2, "gravel"] <- 
    #   soils_fixed[length(ids), "gravel"]
    
    soil_new[seq_len(nrow(soils_fixed)), ] <- soils_fixed
  }
  
  # Set impermeability to zero
  soil_new[, "impermeability_frac"] <- 0
  
  # calc bare soil evap
  if (requireNamespace("rSW2data")) {
    soil_new[, "EvapBareSoil_frac"] <- rSW2data::calc_BareSoilEvapCoefs(
      layers_depth = soil_new[, "depth_cm"],
      sand = soil_new[, "sand_frac"],
      clay = soil_new[, "clay_frac"]
    )[1, ]
  }
  
  return(soil_new[1:8,])
}

#' @param sw_in S4 object. S4 object of class swInputData.
#' @param comp numeric. 1 means user set comp. 2 means comp predicted from climate.
#' @param trees numeric. Fraction of cover that is trees
#' @param shrubs numeric. Fraction of cover that is shrubs
#' @param grasses numeric. Fraction of cover that is grasses.
#' @param forbs numeric. Fraction of cover that is forbs.
#' @param bg numeric. Fraction of cover that is bareground.
#' @param weath S4 object. S4 object of class swWeatherData.
#' @param soil_df data.frame. data.frame with site soils info.

set_comp_roots <- function(sw_in, comp, trees, shrubs, grasses, 
                           forbs, bg, weath, soils_df) {
  
  clim1 <- rSOILWAT2::calc_SiteClimate(weatherList = weath, do_C4vars = TRUE)
  
  # cover
  cover <- rSOILWAT2::estimate_PotNatVeg_composition(
    MAP_mm = 10 * clim1[["MAP_cm"]], 
    MAT_C = clim1[["MAT_C"]],
    mean_monthly_ppt_mm = 10 * clim1[["meanMonthlyPPTcm"]],
    mean_monthly_Temp_C = clim1[["meanMonthlyTempC"]],
    dailyC4vars = clim1[["dailyC4vars"]]
  )
  
  if(comp == 1) {
      rSOILWAT2::swProd_Composition(sw_in) <- 
        c(grasses, shrubs, trees, forbs, bg)
  }
  
  if(comp == 2) {
      rSOILWAT2::swProd_Composition(sw_in) <- c(cover$Rel_Abundance_L1[4],
                                                cover$Rel_Abundance_L1[2],
                                                cover$Rel_Abundance_L1[1],
                                                cover$Rel_Abundance_L1[3],
                                                cover$Rel_Abundance_L1[5])
  }
  
  # Whether you set comp or not, estimate monthly biomass and roots  --------
  
  # Reference biomass values from Bradford et al. 2014 are used
  veg_biom <- rSOILWAT2::estimate_PotNatVeg_biomass(
    target_temp = clim1[["meanMonthlyTempC"]],
    target_MAP_mm = 10 * clim1[["MAP_cm"]], 
    do_adjust_phenology = TRUE, 
    do_adjust_biomass = TRUE,
    fgrass_c3c4ann = cover[["Grasses"]]
  )
  
  # Note: monthly biomass values of forbs, trees, etc. need to be estimated 
  v1 <- c("Litter", "Biomass", "Perc.Live")
  v2 <- c("Litter", "Biomass", "Live_pct") 
  rSOILWAT2::swProd_MonProd_grass(sw_in)[, v2] <- veg_biom[["grass"]][, v1] 
  rSOILWAT2::swProd_MonProd_shrub(sw_in)[, v2] <- veg_biom[["shrub"]][, v1]
  
  # #  rooting profile types
  # Set those to "FILL" where cover == 0 (because of transpiration regions)
  trco_type_by_veg <- list(
    grass_C3 = if (cover[["Rel_Abundance_L0"]][["Grasses_C3"]] > 0) { 
      "SchenkJackson2003_PCdry_grasses"
    } else {
      "FILL"
    },
    grass_C4 = if (cover[["Rel_Abundance_L0"]][["Grasses_C4"]] > 0) {
      "SchenkJackson2003_PCdry_grasses"
    } else {
      "FILL"
    },
    grass_annuals = if (cover[["Rel_Abundance_L0"]][["Grasses_Annuals"]] > 0 ){
      "Jacksonetal1996_crops"
    } else {
      "FILL"
    },
    shrub = if (cover[["Rel_Abundance_L0"]][["Shrubs"]] > 0) {
      "SchenkJackson2003_PCdry_shrubs"
    } else { 
      "FILL"
    },
    forb = if (cover[["Rel_Abundance_L0"]][["Forbs"]] > 0) {
      "SchenkJackson2003_PCdry_forbs"
    } else { 
      "FILL"
    },
    tree = if (cover[["Rel_Abundance_L0"]][["Trees"]] > 0) {
      "Bradfordetal2014_LodgepolePine"
    } else {
      "FILL"
    } 
  )

  veg_roots <- rSOILWAT2::estimate_PotNatVeg_roots( 
    layers_depth = soils_df[, "depth_cm"],
    trco_type_by_veg = trco_type_by_veg, 
    fgrass_c3c4ann = cover[["Grasses"]]
  )
  
  # Add rooting profile to soil
  v1 <- c("Grass", "Shrub", "Tree", "Forb") 
  v2 <- paste0("transp", v1, "_frac") 
  soils_df[, v2] <- veg_roots[, v1]
  
  # Set soils!
  rSOILWAT2::swSoils_Layers(sw_in) <- data.matrix(soils_df)
  
  # done!
  return(sw_in)
  
}

#' @param g numeric. Index to subset GCM array.
#' @param lat numeric. 
#' @param lng2 numeric. 
#' @param GCMs array. Array of GCM names. These names need to correspond 
#' exactly with the file names on the NKN server.
#' @param sw_in0 S4object. swInput object.
#' @param soils_info data.frame.
#' @param soils_info_avg.

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
