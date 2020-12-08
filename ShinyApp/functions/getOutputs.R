
get_output <- function(sw_out, soils_info, soils_info_avg,
                       Scenario) {

  # Read in and format data ----------------------------------------------------
  if (object.size(sw_out) > 10000) {

      # Monthly ----------------------------------------------------------------

      # Soil water potential
      ### Get VWC and average
      SWP <- data.table::data.table(sw_out@VWCMATRIC@Month)
      SWP <- data.table::melt(SWP, id.vars = c('Year', 'Month'))
      SWP <- merge(SWP, soils_info)
      SWP <- SWP[,.(value = weighted.mean(value, width)), .(Year, Month, Depth)]
      SWP <- merge(SWP, soils_info_avg, by = 'Depth')
      # convert to SWP
      SWP <- SWP[,.(value =  rSOILWAT2::VWCtoSWP(value, sand, clay)),
                  .(Year, Month, Depth)]

      names(SWP)[3] <- 'variable'

      # Temperature
      Temp <- data.table::data.table(sw_out@TEMP@Month)[,c('Year', 'Month',
                                                           'avg_C')]
      Temp <- data.table::melt(Temp, id.vars = c('Year', 'Month'))

      PPT <- data.table::data.table(sw_out@PRECIP@Month)[,1:3]
      PPT <- data.table::melt(PPT, id.vars = c('Year', 'Month'))
      PPT$value <- PPT$value * 10 # now in mms

      Vars <- rbind(SWP, Temp, PPT)

      # if(Scenario == 'Current') {
      #   Vars2 <- Vars[Year != curr_year && Month != curr_month, ]

      # Details -------------------------------------------------------------
      Vars$GCM <-  if(Scenario == 'Current') 'Current' else substr(Scenario, 1, (nchar(Scenario) - 13))
      Vars$RCP <- if(Scenario == 'Current') 'Current'else substr(Scenario, (nchar(Scenario) - 4), nchar(Scenario))
    } else {
      stop('SW output error')
    }

  return(Vars)
}

format_data_TS <- function(data, variable, time, curr_year) {
  
  if(!inherits(data, 'data.table')) data <- setDT(data)
  
  #first subset just to historical/current!
  data <- data[Year <= curr_year & GCM == 'Current', ]

  if(time == 'Season') {
  # Get proper value - mean or sum - across TPs.
  if(variable == 'Precipitation (cm)'){

    data2 <- data.table::setDT(data)[,.(value = sum(value)/10),
                                     .(Year, Season, variable)]
    
  } else {
    
    data2 <- data.table::setDT(data)[,.(value = mean(value)),
                                     .(Year, Season, variable)]
    }
  }

  if(time == 'Annual') {
    # Get proper value - mean or sum - across TPs.
    if(variable == 'Precipitation (cm)') {
      data2 <- data.table::setDT(data)[,.(value = sum(value)/10),
                                       .(Year, variable)]
    } else {
      data2 <- data.table::setDT(data)[,.(value = mean(value)),.
                                       (Year, variable)]
    }
  }

  return(data2)

}

get_roll <- function(data, time){

  if(time == 'Annual') {
  data.table::setDT(data)[order(Year), 
                          MA := data.table::frollmean(value, 10,
                                          align = 'center')]
  }
  if(time == 'Month') {
    data.table::setDT(data)[order(Month), 
                            MA := data.table::frollmean(value, 10,
                                            align = 'center'), by = .(Month)]
  }
  if(time == 'Season') {
    data.table::setDT(data)[order(Season), 
                            MA := data.table::frollmean(value, 10,
                                            align = 'center'), by = .(Season)]
  }

  return(data)

}

format_data_BP <- function(data, variable, time){

  #Step 1: Create TPs (Near: 2020 - 2059, 2060 - 2099) - Just data formatting, doesn't affect any aggreggations
  data$TP <- ifelse(data$Year <= 2059, 'Near', 'Late' )
  data[data$GCM == 'Current', 'TP']  <-  'Current'

  if(time == 'Season'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'Precipitation (cm)') {
      data2 <- data.table::setDT(data)[,.(value = sum(value)/10), 
                                       .(RCP, GCM, TP, Year, Season, variable)]
    } else {
      data2 <- data.table::setDT(data)[,.(value = mean(value)), 
                                       .(RCP, GCM, TP, Year, Season, variable)]
    }
  }

  if(time == 'Annual'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'Precipitation (cm)'){
      data2 <- data.table::setDT(data)[,.(value = sum(value)/10),
                                       .(RCP, GCM, TP, Year, variable)]
    } else {
      data2 <- data.table::setDT(data)[,.(value = mean(value)), 
                                       .(RCP, GCM, TP, Year, variable)]
    }
  }
  data2$scenario <-  as.factor(paste(data2$RCP, data2$TP, data2$GCM, sep="_"))
  data2$TP <- factor(data2$TP, levels =c ('Current','Near', 'Late'))

  return(data2)
}

format_data_WL <- function(data, future) {

  if(!inherits(data, 'data.table')) data <- setDT(data)
  
  # Sub  extremely low SWP values to lower lim -------------------------------
  lowVal <- -10
  ysub <- min(data[variable %in% c('Soil Moisture (SWP, -MPa)'), value])
  
  if(ysub <= lowVal){
    data.table::set(data, 
        i = which(data[variable %in% c('Soil Moisture (SWP, -MPa)'), 'value']
                  <= lowVal), j = 'value',  value = lowVal)  
  }
  
  # Grab monthly temperature and precipitation values from hist scenario
  DataC <- data[Year %in% c(1980:2019) & GCM == 'Current' &
                  variable  %in% c('Precipitation (cm)', 
                                   'Average Temperature (C)', 
                                   'Soil Moisture (SWP, -MPa)'), ]

  # format for WL calc
  DataC <- data.table::dcast(DataC, Month ~ variable, value.var = "value", 
                             fun.aggregate = mean)
  
  t_idx <- grep('Temperature', names(DataC))
  names(DataC)[t_idx] <- 'Temp'
  
  p_idx <- grep('Precipitation', names(DataC))
  names(DataC)[p_idx] <- 'PPT'
  
  s_idx <- grep('Soil', names(DataC))
  names(DataC)[s_idx] <- 'SWP'
  
  ## need a numeric vector that is 14 rows long - Jan appended to the top and
  ### December to the bottom
  DataC <- rbind(DataC[1,], DataC, DataC[12,])
  DataC$Month[c(1,14)] <- c(.5, 12.5)

  ## IF future is true, format for WL
  if(future == 1) {

    dataFut <- data[Year %in% c(2020:2099) & RCP != 'Current' &
                      variable %in% c('Precipitation (cm)', 
                                      'Average Temperature (C)',
                                      "Soil Moisture (SWP, -MPa)"), ]
    
    dataFut$TP <- ifelse(dataFut$Year %in% 2020:2059, 'Near', 'Late')

    DatGCM <- data.table::setDT(dataFut)[order(Month),
                                         .(mean = mean(value)),
                                         .(TP, RCP, GCM, Month, variable)]
    
    DatEnsemb <- data.table::setDT(DatGCM)[order(Month),
                                           .(mean = mean(mean),
                                             median = median(mean),
                                             min = min(mean), #change ranks
                                             max = max(mean)),
                                           .(TP, RCP, Month, variable)]
    
    return(list(DataC, DatGCM, DatEnsemb))
    
  } else {
    
    return(list(DataC))
    
  }

}
