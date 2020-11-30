#' get and organize gridMet data for one site
#'
#'  @param lat latitude. degrees_north
#'  @param lng longitude. degrees_west + 360
#'  @param dir currYear path to where gridMET data is stored.
#'  @param dir path. path to where gridMET data is stored.
#'  
#'  @return data.frame

get_gridMET_data <- function(lat, lng, curr_year, dir) {
  
  
  # Make data.frame of just years and days -------------------------------------------
  wdata <- data.frame(Date = seq(from = as.Date('1979-01-01'),
                                 to = as.Date(paste0(curr_year,'-12-31')),
                                 by="day"))
  wdata$Year <- lubridate::year(wdata$Date)
  wdata$DOY <- lubridate::yday(wdata$Date)
  wdata$Tmax_C <- wdata$Tmin_C <- wdata$PPT_cm <- c()
  
  # Get info from netcdfs ....
  # https://www.northwestknowledge.net/metdata/data/
  files <- list.files(dir, full.names = TRUE)
  
  # Tmax_C
  tmmxfiles <- grep('tmmx_', files, value = TRUE)
  for(f in 1:length(tmmxfiles)){
    year <- as.numeric(substr(tmmxfiles[f],  nchar(tmmxfiles[f]) - 6, nchar(tmmxfiles[f]) -3))
    nc <- suppressWarnings(raster::brick(tmmxfiles[f], varname = 'air_temperature'))
    vals <- raster::extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
    
    # determine "data of last weather" here
    if(year == curr_year){
      lastWeatherDate <- as.Date(length(vals), origin = paste0(curr_year,"-01-01"))
    }
    
    wdata[wdata$Year == year, 'Tmax_C'][1:length(vals)] <- vals
  }
  
  # Tmin_C
  tmmnfiles <- grep('tmmn_', files, value = TRUE)
  for(f in 1:length(tmmnfiles)){
    year <- as.numeric(substr(tmmnfiles[f],  nchar(tmmnfiles[f]) - 6, nchar(tmmnfiles[f]) -3))
    
    nc <- suppressWarnings(raster::brick(tmmnfiles[f], varname = 'air_temperature'))
    vals <- raster::extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
    wdata[wdata$Year == year, 'Tmin_C'][1:length(vals)] <- vals
  }
  
  #PPT_cm
  prfiles <- grep('pr_', files, value = TRUE)
  for(f in 1:length(prfiles)){
    year <- as.numeric(substr(prfiles[f],  nchar(prfiles[f]) - 6, nchar(prfiles[f]) -3))
    
    nc <- suppressWarnings(raster::brick(prfiles[f], varname = 'precipitation_amount'))
    vals <- raster::extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
    wdata[wdata$Year == year, 'PPT_cm'][1:length(vals)] <- vals
  }
  
  # if there isn't 365 days in each year ... na.locf for temp and put 0 for ppt?
  # fill in missing with weather generator when running SOILWAT?
  wdata$Tmax_C <- zoo::na.locf(wdata$Tmax_C)
  wdata$Tmin_C <- zoo::na.locf(wdata$Tmin_C)
  wdata[is.na(wdata$PPT_cm), 'PPT_cm'] <- 0
  
  # convert
  wdata$Tmax_C <- wdata$Tmax_C - 273.15
  wdata$Tmin_C <- wdata$Tmin_C - 273.15
  wdata$PPT_cm <- wdata$PPT_cm /10
  
  wdata <- rSOILWAT2::dbW_dataframe_to_weatherData(
    wdata[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)
  
  return(list(wdata, lastWeatherDate))
}

get_MACA_one_scenario_test <- function(lat, lng, url_main, sc) {
  
  # Make data.frame of just years and days -------------------------------------------
  wdata_fut<- data.frame(Date = seq(from = as.Date('2020-01-01'),
                                    to = as.Date('2099-12-31'), by="day"))
  wdata_fut$Year <- lubridate::year(wdata_fut$Date)
  wdata_fut$DOY <- lubridate::yday(wdata_fut$Date)
  
  #  test ----------------------------------------------------------------
  url_one_scenario <- paste0(url_main, 'tasmax_', sc, '_2006_2099_CONUS_daily.nc')
  nc <- suppressWarnings( raster::brick(url_one_scenario, varname = 'air_temperature'))
  vals <- raster::extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
  
  url_one_scenario <- paste0(url_main, 'tasmin_', sc, '_2006_2099_CONUS_daily.nc')
  nc <- suppressWarnings( raster::brick(url_one_scenario, varname = 'air_temperature'))
  vals <-  raster::extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
  
  url_one_scenario <- paste0(url_main, 'pr_', sc, '_2006_2099_CONUS_daily.nc')
  nc <- suppressWarnings( raster::brick(url_one_scenario, varname = 'precipitation'))
  vals <-  raster::extract(nc, matrix(c(lng, lat), ncol = 2))[1,]

}

#' get and organize MACA data for one site and scenario
#'
#'  @param lat latitude. degrees_north
#'  @param lng longitude. degrees_west + 360
#'  @param url_main character. Url to catalog where MACA data is stored
#'  @param sc character. scenario string
#'
#'  @return data.frame

get_MACA_one_scenario <- function(lat, lng, url_main, sc) {
  
  print(sc)
  lng <- -112.2 + 360
  
  # Make data.frame of just years and days -------------------------------------------
  wdata_fut<- data.frame(Date = seq(from = as.Date('2020-01-01'),
                                 to = as.Date('2099-12-31'), by="day"))
  wdata_fut$Year <- lubridate::year(wdata_fut$Date)
  wdata_fut$DOY <- lubridate::yday(wdata_fut$Date)
  
  # tmax! --------------------------------------------------------------------
  # format url
  url_one_scenario <- paste0(url_main, 'tasmax_', sc, '_2006_2099_CONUS_daily.nc')
  nc <- ncdf4::nc_open(url_one_scenario)
  dat <- get_MACA_one_variable(nc, lat, lng, variable = 'air_temperature')
  names(dat)[2] <- 'Tmax_C'
  wdata_fut <- merge(wdata_fut, dat)
  
  # tmin! --------------------------------------------------------------------
  # format url
  url_one_scenario <- paste0(url_main, 'tasmin_', sc, '_2006_2099_CONUS_daily.nc')
  nc <- ncdf4::nc_open(url_one_scenario)
  dat <- get_MACA_one_variable(nc, lat, lng, variable = 'air_temperature')
  names(dat)[2] <- 'Tmin_C'
  wdata_fut <- merge(wdata_fut, dat)
  
  # precip! --------------------------------------------------------------------
  # format url
  url_one_scenario <- paste0(url_main, 'pr_', sc, '_2006_2099_CONUS_daily.nc')
  nc <- ncdf4::nc_open(url_one_scenario)
  dat <- get_MACA_one_variable(nc, lat, lng, variable = 'precipitation')
  wdata_fut <- merge(wdata_fut, dat)
  
  wdata_fut <- rSOILWAT2::dbW_dataframe_to_weatherData(
    wdata_fut[,c('Year', 'DOY', 'Tmax_C', 'Tmin_C', 'PPT_cm')], round = 4)
  
  return(wdata_fut)
}

#' get and organize MACA data for one variable, site and scenario
#'
#'  @param nc netCDF. netCDF file containing data for main variable and
#'  scenario.
#'  @param lat latitude. degrees_north
#'  @param lng longitude. degrees_west + 360
#'  @param variable character. variable name
#'
#'  @return data.frame
get_MACA_one_variable <- function(nc, lat, lng, variable) {
  
  lat <- as.numeric(lat)
  lng <- as.numeric(lng)
  
  v3 <- nc$var[[1]] ## NOTE: FILE DIMENSIONS ARE lon,lat,time
  endcount <- v3$varsize[3] 
  
  # find the lat and long index -----------------------------------------------
  all_lng <- ncdf4::ncvar_get(nc, "lon")
  lng_idx <- which.min(abs(all_lng - lng))
  
  all_lat <- ncdf4::ncvar_get(nc, "lat")
  lat_idx <- which.min(abs(all_lat - lat))
  
  # get variable data ---------------------------------------------------------
  
  data <- ncdf4::ncvar_get(nc, variable, 
                           start=c(lng_idx, lat_idx, 1),
                           count=c(1, 1, endcount))
  
  # get time data -------------------------------------------------------------
  Date <- ncdf4::ncvar_get(nc, "time", start=c(1),count=c(endcount))
  ##note: assumes leap years!
  ### http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
  Date = as.Date(Date, origin="1900-01-01") 
  
  # format and organize --------------------------------------------------------
  if(variable == 'precipitation') {
    c <- data.frame(Date, PPT_cm = data/10)
  }
  
  if(variable == 'air_temperature') {
    c <- data.frame(Date, T_C = data - 273.15)
  }
  
  ncdf4::nc_close(nc)
  
  return(c)
}
