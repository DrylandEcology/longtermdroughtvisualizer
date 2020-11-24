
get_output <- function(sw_out, soils_info, soils_info_avg, 
                       Scenario) {

  # Read in and format data ----------------------------------------------------
  if (object.size(sw_out) > 10000) {

      # Monthly ----------------------------------------------------------------
     
      # Soil water potential
      ### Get VWC and average
      SWP <- data.table(sw_out@VWCMATRIC@Month)
      SWP <- data.table::melt(SWP, id.vars = c('Year', 'Month'))
      SWP <- merge(SWP, soils_info)
      SWP <- SWP[,.(value = weighted.mean(value, width)), .(Year, Month, Depth)]
      SWP <- merge(SWP, soils_info_avg, by = 'Depth')
      # convert to SWP
      SWP <- SWP[,.(value =  rSOILWAT2::VWCtoSWP(value, sand, clay)), 
                  .(Year, Month, Depth)]
      
      names(SWP)[3] <- 'variable'

      # Temperature
      Temp <- data.table(sw_out@TEMP@Month)
      Temp$surfaceTemp_C <- NULL
      Temp <- data.table::melt(Temp, id.vars = c('Year', 'Month'))

      PPT <- data.table(sw_out@PRECIP@Month)[,1:3]
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

formatDataTS <- function(data, variable, time){
  #first subset just to historical/current!
  data <- data[data$GCM == 'Current', ]
  data <- data[data$Year <= 2013, ]

  if(time == 'Season'){
  # Get proper value - mean or sum - across TPs.
  if(variable == 'Precipitation (cm)'){

    data2 <- setDT(data)[,.(value = sum(value)/10),.(Year, Season, variable)]
  }else{
    data2 <- setDT(data)[,.(value = mean(value)),.(Year, Season, variable)]
  }
  }

  if(time == 'Annual'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'Precipitation (cm)'){
      data2 <- setDT(data)[,.(value = sum(value)/10),.(Year, variable)]
    }else{
      data2 <- setDT(data)[,.(value = mean(value)),.(Year, variable)]
    }
  }

  return(data2)

}

getroll <- function(data, time){

  if(time == 'Annual'){
  data <- plyr::arrange(data, Year)
  setDT(data)[, MA := rollmean(value, 10, na.pad = TRUE)]
  }
  if(time == 'Month'){
    data <-  plyr::arrange(data,Month)
    setDT(data)[, MA := rollmean(value, 10, na.pad = TRUE), by = .(Month)]
  }
  if(time == 'Season'){
    data <-  plyr::arrange(data,Season)
    setDT(data)[, MA := rollmean(value, 10, na.pad = TRUE), by = .(Season)]
  }

  return(data)

}

formatDataBP <- function(data, variable, time){

  #Step 1: Create TPs (Near: 2020 - 2059, 2060 - 2099) - Just data formatting, doesn't affect any aggreggations
  TP_DF <- data.frame(Year = c(1974:2013,2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
  data <- suppressMessages(plyr::join(TP_DF, data))
  #data2 <- data[data$Year == 2015, ]
  #data2 <- data2[data2$GCM == 'Current', ]
  #data <- data[data$Year != 2015, ]
  #data <- rbind(data, data2)

  if(time == 'Season'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'Precipitation (cm)'){
      data2 <- setDT(data)[,.(value = sum(value)/10),.(RCP, GCM, TP, Year, Season, variable)]
    }else{
      data2 <- setDT(data)[,.(value = mean(value)),.(RCP, GCM, TP, Year, Season, variable)]
    }
  }

  if(time == 'Annual'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'Precipitation (cm)'){
      data2 <- setDT(data)[,.(value = sum(value)/10),.(RCP, GCM, TP, Year, variable)]
    }else{
      data2 <- setDT(data)[,.(value = mean(value)),.(RCP, GCM, TP, Year, variable)]
    }
  }
  data2$scenario <-  as.factor(paste(data2$RCP, data2$TP, data2$GCM, sep="_"))
  data2$TP <- factor(data2$TP, levels =c ('Current','Near', 'Late'))

  return(data2)
}

format_data_WL <- function(data, future) {

  # need a numeric vector that 12 cols long and then ppt, min_C, max_C, min_C again
  DataC <- data[data$Year %in% c(1980:2019), ]
  DataC <- DataC[DataC$GCM %in% 'Current', ]
  DataC <- DataC[DataC$variable %in% c('Precipitation (cm)', 'max_C', 'min_C'), ]
  DataC <- reshape2::dcast(DataC, Month ~ variable, value.var = "value", fun.aggregate = mean)
  names(DataC)[4] <- 'PPT'
  DataC$Temp <- rowMeans(DataC[,2:3])

  DataC <- rbind(DataC[1,], DataC, DataC[12,])
  DataC$Month2 <- c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                                     'August', 'September', 'October', 'November', 'December', 'December2')
  DataC$Month2 <- factor(DataC$Month2, levels =c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                                                   'August', 'September', 'October', 'November', 'December', 'December2'))


  if(future == 1) {
    ##### Future data
    dataFut <- data[data$Year %in% c(2020:2099), ]
    TP_DF <- data.frame(Year = c(2020:2099), TP = c(rep('Near',40), rep('Late', 40)))
    dataFut <- suppressMessages(plyr::join(dataFut, TP_DF))

    DatGCM <- setDT(dataFut)[,.(mean = mean(value)),
                          .(TP, RCP, GCM, Month, variable)]

    DatEnsemb <- setDT(DatGCM)[,.(mean = mean(mean),
                                median = median(mean),
                                min = min(mean), #change ranks
                                max = max(mean)),
                             .(TP, RCP, Month, variable)]

    return(list(DataC, DatGCM, DatEnsemb))
  } else {
    return(list(DataC))
  }

}

format_data_SM <- function(data, RCP) {

  data <- data[data$variable %in% 'Soil Moisture (SWP, -MPa)', ]
  MonthDF <- data.frame(Month = 1:12,
                        Month2 =  c( 'January', 'February', 'March',
                                     'April', 'May', 'June', 'July',
                                     'August', 'September', 'October',
                                     'November', 'December' ))
  data <- suppressMessages(plyr::join(data.frame(data), MonthDF))
  
  data$Month2 <- factor(data$Month2, levels =c( 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                                                         'August', 'September', 'October', 'November', 'December'))

  # Subset by RCP
  eval(parse(text = paste0("data <- data[data$RCP %in% c('Current','", RCP, "'), ]")))

  # Get TPs
  TP_DF <- data.frame(Year = c(1980:2019, 2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
  data <- suppressMessages(plyr::join(TP_DF, data))

  DatGCM <- setDT(data)[,.(mean = mean(value)),
                        .(TP, RCP, GCM, Month, Month2, variable)]

  DatEnsemb <- setDT(DatGCM)[,.(mean = mean(mean),
                                median = median(mean),
                                min = min(mean), #change ranks
                                max = max(mean)),
                             .(TP, RCP, Month, Month2, variable)]


  ### ysub for plotting
  lowVal <- -8
  ysub <- ceiling(min(DatEnsemb$median, na.rm = TRUE)) - 1

  if(ysub <= lowVal){
    DatEnsemb$median <- ifelse(DatEnsemb$median <= lowVal,lowVal, DatEnsemb$median)
    ysub <- lowVal
  }

  ### return
  return(list(DatGCM, DatEnsemb, ysub))

}
