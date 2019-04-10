setwd('~/Documents/Git/longtermdroughtsimulator/test')
rm(list =ls(all = TRUE))
library(plotly)
library(data.table)

data <- fread('AllData.csv')
head(data)

# For WL actually just need monthly values

# need a numeric vector that 12 columsn long and then ppt, min_C, max_C, min_C again
dataC <- data[data$Year %in% c(1976:2015), ]
dataC <- dataC[dataC$GCM %in% 'Current', ]
dataC <- dataC[dataC$variable %in% c('ppt', 'max_C', 'min_C'), ]
dataC <- reshape2::dcast(dataC, Month ~ variable, value.var = "value", fun.aggregate = mean)
dataC <- t(dataC)
dataC <- dataC[c('ppt', 'min_C', 'max_C', 'min_C'),]


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
legendgroup = 'group1'
