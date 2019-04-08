
get_output <- function(){

  # Set up paths
  path_3Runs <- ("data/rSFSW2_ProjectFiles/3_Runs")
  Scenarios <- list.dirs(path_3Runs)

  # Soils
  SoilsDf <- data.frame(variable= paste0('Lyr_', 1:8), Layer= c(rep('Shallow', 2), # 10, 20
                                                            rep('Inter', 3), # 40, 60, 80
                                                            rep('Deep', 3))) # 100, 150, 200
  #SeasonDF
  SeasonsDF <- data.frame(Month = c(1:12), Season = c(rep('Winter',2),
                                                      rep('Spring', 3),
                                                      rep('Summer', 3),
                                                      rep('Fall', 3),
                                                      rep('Winter', 1)))
  #Variables
  VarDF <- data.frame(variable = c('Shallow', 'Inter', 'Deep', 'max_C', 'min_C', 'avg_C', 'ppt'),
                      variable2 = c('Shallow', 'Soil Moisture (SWP, -MPa)', 'Deep', 'max_C', 'min_C', 'Average Temperature (C)', 'Precipitation (cm)'))


  # Read in and format data
  AllVars <- data.frame()

  for(i in 2:length(Scenarios)){
    success <- try(load(file.path(Scenarios[i], 'sw_output_sc1.RData')))

    if (!inherits(success, "try-error")) {

      # Monthly ----------------------------------------------------------------
      SWP <- data.frame(runDataSC@SWPMATRIC@Month)
      SWP <- reshape2::melt(SWP, id.vars = c('Year', 'Month'))
      SWP <- suppressMessages(plyr::join(SWP, SoilsDf))
      SWPMean <- setDT(SWP)[,.(value = mean(value)),.(Year, Month, Layer)] #Probably need to get VWC and convert
      names(SWPMean)[3] <- 'variable'
      SWPMean$value <- SWPMean$value * -0.1

      Temp <- data.frame(runDataSC@TEMP@Month)
      Temp$surfaceTemp_C <- NULL
      Temp <- reshape2::melt(Temp, id.vars = c('Year', 'Month'))

      PPT <- data.frame(runDataSC@PRECIP@Month)[1:3]
      PPT <- reshape2::melt(PPT, id.vars = c('Year', 'Month'))
      PPT$value <- PPT$value * 10 # now in mms

      Vars <- rbind(SWPMean, Temp)
      Vars <- rbind(Vars, PPT)

      # Details -------------------------------------------------------------

      Scenario <- sapply(strsplit(Scenarios[i],'_'), "[", 5)
      Vars$GCM <-  if(Scenario == 'Current') 'Current' else sapply(strsplit(Scenario,'\\.'), "[", 4)
      Vars$RCP <- if(Scenario == 'Current') 'Current'else sapply(strsplit(Scenario,'\\.'), "[", 3)

      AllVars <- rbind(AllVars, Vars)

    } else{
      next
    }
  }

  AllVars <- suppressMessages(plyr::join(AllVars,SeasonsDF))
  AllVars <- suppressMessages(plyr::join(AllVars,VarDF))
  AllVars$variable <- NULL
  names(AllVars)[7] <- 'variable'
  AllVars <- AllVars[AllVars$Year %in% c(1915:2013, 2020:2099), ]

  return(list(AllVars))


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
  data <- arrange(data,Year)
  setDT(data)[, MA := rollmean(value, 10, na.pad = TRUE)]
  }
  if(time == 'Month'){
    data <- arrange(data,Month)
    setDT(data)[, MA := rollmean(value, 10, na.pad = TRUE), by = .(Month)]
  }
  if(time == 'Season'){
    data <- arrange(data,Season)
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



formatDataWL <- function(data, future) {

  # need a numeric vector that 12 columsn long and then ppt, min_C, max_C, min_C again
  DataC <- data[data$Year %in% c(1974:2013), ]
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

    return(list(DataC, DatEnsemb))
  } else {
    return(list(DataC))
  }

}


formatDataSM <- function(data, RCP) {

  data <- data[data$variable %in% 'Soil Moisture (SWP, -MPa)', ]
  data$Month2 <- c( 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                        'August', 'September', 'October', 'November', 'December' )
  data$Month2 <- factor(data$Month2, levels =c( 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                                                         'August', 'September', 'October', 'November', 'December'))

  #data2 <- data[data$Year == 2015, ]
  #data2 <- data2[data2$GCM == 'Current', ]
  #data <- data[data$Year != 2015, ]
  #data <- rbind(data, data2)

  # Subset by RCP
  eval(parse(text = paste0("data <- data[data$RCP %in% c('Current','", RCP, "'), ]")))

  # Get TPs
  TP_DF <- data.frame(Year = c(1974:2013, 2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
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
