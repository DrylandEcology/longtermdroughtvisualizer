
get_output <- function(){
  
  # Set up paths
  path_3Runs <- ("data/rSFSW2_ProjectFiles/3_Runs")
  Scenarios <- list.dirs(path_3Runs)
  
  # Soils
  SoilsDf <- data.frame(variable= paste0('Lyr_', 1:7), Layer= c(rep('Shallow', 2),
                                                            rep('Inter', 3),
                                                            rep('Deep', 2)))
  #SeasonDF
  SeasonsDF <- data.frame(Month = c(1:12), Season = c(rep('Winter',2),
                                                      rep('Spring', 3),
                                                      rep('Summer', 3),
                                                      rep('Fall', 3),
                                                      rep('Winter', 1)))
  # Read in and format data
  AllVars <- data.frame()
  DailySWP <- data.frame()
  
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
      Temp <- reshape2::melt(Temp, id.vars = c('Year', 'Month'))
      
      PPT <- data.frame(runDataSC@PRECIP@Month)[1:3]
      PPT <- reshape2::melt(PPT, id.vars = c('Year', 'Month'))
      PPT$value <- PPT$value * 10 # now in mms
      
      Vars <- rbind(SWPMean, Temp)
      Vars <- rbind(Vars, PPT)
     
      # Daily -----------------------------------------------------------------
      SWPd <-  data.frame(runDataSC@SWPMATRIC@Day)
      SWPd <- reshape2::melt(SWPd, id.vars = c('Year', 'Day'))
      SWPd <- suppressMessages(plyr::join(SWPd, SoilsDf))
      
      DailySWPd <- setDT(SWPd)[,.(value = mean(value)),.(Year, Day, Layer)] #Probably need to get VWC and convert
      names(DailySWPd)[3] <- 'variable'
      DailySWPd <- DailySWPd[DailySWPd$variable == 'Inter', ]
      DailySWPd$value <- DailySWPd$value * -0.1
      
      # Details -------------------------------------------------------------
      
      Scenario <- sapply(strsplit(Scenarios[i],'_'), "[", 5)
      Vars$GCM <- DailySWPd$GCM <- if(Scenario == 'Current') 'Current' else sapply(strsplit(Scenario,'\\.'), "[", 4)
      Vars$RCP <- DailySWPd$RCP <- if(Scenario == 'Current') 'Current'else sapply(strsplit(Scenario,'\\.'), "[", 3)
      
      AllVars <- rbind(AllVars, Vars)
      DailySWP <- rbind(DailySWP, DailySWPd)
      
    } else{
      next
    }
  }
  
  AllVars <- suppressMessages(plyr::join(AllVars,SeasonsDF))
  #write.csv(AllVars, '~/Desktop/CDI_SOILWATGUI/ShinyBegin/runSWwithUIs/test/AllData.csv')
  #write.csv(DailySWP,  '~/Desktop/CDI_SOILWATGUI/ShinyBegin/runSWwithUIs/test/DailySWP.csv')
  return(list(AllVars, DailySWP))

  
}

formatDataTS <- function(data, variable, time){
  #first subset just to historical/current!
  data <- data[data$GCM == 'Current', ]
  
  if(time == 'Season'){
  # Get proper value - mean or sum - across TPs.
  if(variable == 'ppt'){

    data2 <- setDT(data)[,.(value = sum(value)),.(Year, Season, variable)]
  }else{
    data2 <- setDT(data)[,.(value = mean(value)),.(Year, Season, variable)]
  }
  }

  if(time == 'Annual'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'ppt'){
      data2 <- setDT(data)[,.(value = sum(value)),.(Year, variable)]
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
  TP_DF <- data.frame(Year = c(1976:2015,2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
  data <- suppressMessages(plyr::join(TP_DF, data))
  data2 <- data[data$Year == 2015, ]
  data2 <- data2[data2$GCM == 'Current', ]
  data <- data[data$Year != 2015, ]
  data <- rbind(data, data2)
  
  if(time == 'Season'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'ppt'){
      data2 <- setDT(data)[,.(value = sum(value)),.(RCP, GCM, TP, Year, Season, variable)]
    }else{
      data2 <- setDT(data)[,.(value = mean(value)),.(RCP, GCM, TP, Year, Season, variable)]
    }
  }
  
  if(time == 'Annual'){
    # Get proper value - mean or sum - across TPs.
    if(variable == 'ppt'){
      data2 <- setDT(data)[,.(value = sum(value)),.(RCP, GCM, TP, Year, variable)]
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
  DataC <- data[data$Year %in% c(1976:2015), ]
  DataC <- DataC[DataC$GCM %in% 'Current', ]
  DataC <- DataC[DataC$variable %in% c('ppt', 'max_C', 'min_C'), ]
  DataC <- reshape2::dcast(DataC, Month ~ variable, value.var = "value", fun.aggregate = mean)
  DataC <- t(DataC)
  DataC <- DataC[c('ppt', 'min_C', 'max_C', 'min_C'),]
  
  if(future == 1) {
    ##### Future data
    dataFut <- data[data$Year %in% c(2020:2099), ]
    TP_DF <- data.frame(Year = c(2020:2099), TP = c(rep('Near',40), rep('Late', 40)))
    dataFut <- suppressMessages(plyr::join(dataFut,TP_DF))
  
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
  

formatDataDSM <- function(data, RCP) {
  
  data <- data[data$variable %in% 'Inter', ]
  
  data2 <- data[data$Year == 2015, ]
  data2 <- data2[data2$GCM == 'Current', ]
  data <- data[data$Year != 2015, ]
  data <- rbind(data, data2)
  
  # Subset by RCP
  eval(parse(text = paste0("data <- data[data$RCP %in% c('Current','", RCP, "'), ]")))
  
  # Get TPs
  TP_DF <- data.frame(Year = c(1976:2015, 2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
  data <- suppressMessages(plyr::join(TP_DF, data))
  
  DatGCM <- setDT(data)[,.(mean = mean(value)),
                        .(TP, RCP, GCM, Day, variable)]   
  
  DatEnsemb <- setDT(DatGCM)[,.(mean = mean(mean),
                                median = median(mean),
                                min = min(mean), #change ranks
                                max = max(mean)),
                             .(TP, RCP, Day, variable)]   
  
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


# -------------------------------------------------------------------------------

# Time Series settings
legendbkrd <- adjustcolor(col = '#FFFFFF',alpha.f = 0)
colors2 = c( '#FF7F50', '#99d594','#d53e4f','#3288bd')

# Dail Soil Moisture theme
legendbkrd <- adjustcolor(col = '#FFFFFF',alpha.f = 0)

theme_DSM <- theme(legend.position = c(.855,0.125),
                  legend.background = element_rect(fill = legendbkrd),
                  #legend.margin=unit(-0.6,"cm"),
                  legend.key.height=unit(0, "cm"),
                  legend.text=element_text(size=10,face='plain'))

#Set uniform themes
uniformTheme <-     theme(panel.grid.minor = element_blank(),
                          #text
                          text = element_text(family = 'Frutiger LT Pro 45 Light',size = 12,face='bold'),
                          axis.title.x=element_blank(),
                          axis.title = element_text(family='Frutiger LT Pro 45 Light',size= 12,face='plain'),
                          axis.text = element_text(family='Frutiger LT Pro 45 Light',size = 12,face='plain'))
