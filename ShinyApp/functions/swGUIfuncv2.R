library(dplyr)
library(data.table)


show_js_inputs <- function(lat, long, future, soils, sand, clay,
               comp, trees, shrubs, grasses, forbs, bareground){
  print(paste(lat, long, future, soils, sand, clay,
              comp, trees, shrubs, grasses, forbs, bareground))
}

#' @code{}
#' 
#' @param location A list from the input$clickmap of lat & lng
#' @param futuresim logical. TRUE means future simulations will be executed.
#' 
    
set_execute_SW <- function(lat, lng, futuresim, 
                           soils, sand = 33, clay = 33, 
                           comp, trees = 0, shrubs = 0.5, grasses = 0.5, forbs = 0, bg = 0){

  # delete any potential old or used files
  delete_test_output2("data/rSFSW2_ProjectFiles/")
  
  # load project description & setting environment
  source('data/rSFSW2_ProjectFiles/SFSW2_project_code_Part1.R')
  
  # 1 set location and treatments in Input Master and Treatment files
  set_IM(lat, lng, futuresim)
  
  #2 set whether soils should be extracted from 250m data or chosen by user
  set_soils(SFSW2_prj_meta, soils, sand, clay, futuresim)
  
  #3 set whether composition should be predicted from climate or chosen by user
  set_comp(SFSW2_prj_meta, comp, trees, shrubs, grasses, forbs, bg, futuresim)
  
  #### Save new project description & setting environment
  save_to_rds_with_backup(SFSW2_prj_meta,  file = "data/rSFSW2_ProjectFiles/SFSW2_project_descriptions_Onesite.rds")
  
  source("data/rSFSW2_ProjectFiles/SFSW2_project_code_check_run.R")
}



#' @code{set_IM} Set location (coordinates) in InputMaster

set_IM <- function(lat, lng, futuresim){
  
  # Input input master file
  IMfilepathIn <- "data/rSFSW2_ProjectFiles/1_Input/SWRuns_InputMaster_template_v11.csv"
  
  # Output input master file
  IMfilepathOut <- "data/rSFSW2_ProjectFiles/1_Input/SWRuns_InputMaster_fill_v11.csv"

  ###########################################################
  #------------ find coord match from master ----------------
  ###########################################################
  # Read in file
  IMfile <- read.csv(IMfilepathIn, stringsAsFactors = FALSE)
  
  # get X & Y
  coords <- data.frame(lat, lng)

  # TO DO: checks for X & Y - regular and within range
  
  # Write in lat & longs ----
  IMfile$X_WGS84 <- coords[1,2] # lng
  IMfile$Y_WGS84 <- coords[1,1] # lat

  # Find what state you are in 
  
  state <- 'AZ'
  
  # Find matching weather folder in proper weather db ----
  MasterSites <- fread(file.path("data", paste0(state,"_weatherDB_cells.csv")))
  
  # correct resolution of points (double check that we are finding the right coord in respect to bottom let, top right, etc.)
  coords2 <- apply(coords, 2, conv_res) #Refining coordinates to match database resolution.

  # match -----
  coordstring <- paste(coords2[2], coords2[1], sep="_")

  WeatherFolder <- grep(coordstring, MasterSites$Label, value = TRUE)
  WeatherFolder <- WeatherFolder[1:23]
  ###########################################################
  #------------------ Only Current or All --------------------
  ###########################################################
  
  if(futuresim == 2) WeatherFolder <- grep('Current', WeatherFolder, value = TRUE) # limit weather folders to current
  #if(futuresim == 1) WeatherFolder <- grep('Current|CanESM2', WeatherFolder, value = TRUE) # limit weather folders to current
  
  ###########################################################
  #------------------ check insert write --------------------
  ###########################################################
  
  # checks
  # # has value, length of 1. only western states in this DB
  if (is.na(WeatherFolder) || length(WeatherFolder) == 0) {
    print(paste('Sites not found in weather database'))
  }
  
  # insert into IM - need as many rows as climate scenarios?
  IMfile <- do.call("rbind", replicate(length(WeatherFolder), IMfile, simplify = FALSE))
  IMfile$site_id <- 1:length(WeatherFolder)
  IMfile$WeatherFolder <- WeatherFolder
  IMfile$Label <-  sapply(strsplit( IMfile$WeatherFolder, "_"), "[", 5)
  
  # write - fill file is linked in the descriptions.R file

  write.csv(IMfile, IMfilepathOut, row.names = FALSE)
  
  ######################################################################################################################
  #                           ------------------ Setup Treatment Design-----------------
  ######################################################################################################################
  
  # Input treatment file
  TrtfilepathIn <- "data/rSFSW2_ProjectFiles/1_Input/SWRuns_InputData_TreatmentDesign_v17.csv"
  
  # Output treatment file
  TrtfilepathOut <- "data/rSFSW2_ProjectFiles/1_Input/SWRuns_InputData_TreatmentDesign_v17_fill.csv"
  
  # Read in file
  TrtFile <- read.csv(TrtfilepathIn, stringsAsFactors = FALSE)
  
  ###########################################################
  #------------------ check insert write --------------------
  ###########################################################
  
  if(futuresim == 1) {
    
    TrtFile <- rbind(TrtFile[1,], do.call("rbind", replicate(length(WeatherFolder), TrtFile[2,], simplify = FALSE)))
    indx <- length(WeatherFolder) + 1
    TrtFile$LookupWeatherFolder[2:indx] <- WeatherFolder
    TrtFile$Label[2:indx] <- sapply(strsplit(WeatherFolder, "_"), "[", 5)
    TrtFile$YearStart[2:indx] <- c(1915, rep(2015, 22))
    TrtFile$YearEnd[2:indx] <- c(2015, rep(2099, 22))
    TrtFile[1,c('LookupWeatherFolder', 'YearStart', 'YearEnd')] <- 1
    
  }
  
  # write - fill file is linked in the descriptions.R file
  write.csv(TrtFile, TrtfilepathOut, row.names = FALSE)
}



  
#' @code{set_soils} Set soils options based on user input. 
#'
#' @param soils 1 is extract (default), 2 is select.
#' @param sand %Sand in each layer
#' @param clay %Clay in each layer
#' 
#' @return SoilTexture file
#' @return SoilLayers file
#' 
#' @export

set_soils <- function(environment, soils, sand, clay, futuresim){

  #################################################################################
  if(soils == "2") { # If user chooses to select soils, need to populate files.
    
    #################################################################
    # --------- change environment soil extraction to 0 ------------
    #################################################################
    environment[["exinfo"]][["ExtractSoilDataFromISRICWISE30secV1a_Global"]] <- 0
    
    #################################################################
    # --------- change & write soil depth file ------------
    #################################################################
    x <- read.csv(SFSW2_prj_meta[["fnames_in"]][["fslayers"]], stringsAsFactors = FALSE)
    y <- data.frame(matrix(nrow = 1, ncol = ncol(x)))
    names(y) <- names(x)
    
    y$Label[1] <- "Site01"
    y[1,c(3:9)] <- c(10, 20, 40, 60, 80, 100, 150) # TO DO: change to standard 250 m depths
    y[1,2] <- 150
    
    if(futuresim == 1)   y <- do.call("rbind", replicate(23, y, simplify = FALSE))

    write.csv(y, SFSW2_prj_meta[["fnames_in"]][["fslayers"]], row.names = FALSE)
  
    #################################################################
    # --------- change & write soil texture file ----------
    #################################################################
    x <- read.csv(SFSW2_prj_meta[["fnames_in"]][["fsoils"]], stringsAsFactors = FALSE)
    y <- data.frame(matrix(nrow = 2, ncol = ncol(x)))
    y[1,] <- x[1,]
    names(y) <- names(x)
    
    y$Label[2] <- "Site01"

    # sand
    sandIdx <- grep( 'Sand', names(y))
    y[2,sandIdx[1:7]] <- as.numeric(sand/100) #always 8 
    y[1,sandIdx[1:7]] <- 1
    
    # clay
    clayIdx <- grep( 'Clay', names(y))
    y[2,clayIdx[1:7]] <- as.numeric(clay/100) 
    y[1,clayIdx[1:7]] <- 1
    
    # TO DO - bulk density function where it still pulls
    bdIdx <- grep( 'Matricd', names(x))
    y[2,bdIdx[1:7]] <- 1.66
    y[1,bdIdx[1:7]] <- 1

    if(futuresim == 1)  y <- rbind(y[1,],  do.call("rbind", replicate(23, y[2,], simplify = FALSE)))
    
    write.csv(y, SFSW2_prj_meta[["fnames_in"]][["fsoils"]], row.names = FALSE)

    }

}

#' @code{set_comp} set comp.
#'
#' @param comp 1 is estimate comp from climate (default), 2 is user selection.
#' 
#' @return prod file
#' 
#' @export

set_comp <- function(environment, comp, trees, shrubs, grasses, forbs, bg, futuresim){

  ######################################################################################################################
  #                           ------------------ Setup Experimental Designfile Design-----------------
  ######################################################################################################################
  
  # Input exp file
  ExpfilepathIn <- "data/rSFSW2_ProjectFiles/1_Input/SWRuns_InputData_ExperimentalDesign_v09.csv"
  
  # Output exp file
  ExpfilepathOut <- environment[["fnames_in"]][["fexpDesign"]]
  
  # Read in file
  ExpFile <- read.csv(ExpfilepathIn, stringsAsFactors = FALSE)
  
  if(comp == "2") { # If user chooses to select comp, need to populate exp file
    
    #################################################################
    # --------- change exp design file --------------------------------
    #################################################################
    
    ExpFile$Label[2] <- "VegSet"
    ExpFile[,25:53] <- 0
    
    ExpFile$PotentialNaturalVegetation_CompositionTrees_Fraction <- c(1, trees/100)
    ExpFile$PotentialNaturalVegetation_CompositionShrubs_Fraction <- c(1, shrubs/100)
    ExpFile$PotentialNaturalVegetation_CompositionAnnuals_Fraction <- c(1, grasses/100)
    ExpFile$PotentialNaturalVegetation_CompositionForb_Fraction <- c(1, forbs/100)
    ExpFile$PotentialNaturalVegetation_CompositionBareGround_Fraction <- c(1, bg/100)
    
  }
  
  write.csv(ExpFile, environment[["fnames_in"]][["fexpDesign"]], row.names = FALSE)
  
  
}



