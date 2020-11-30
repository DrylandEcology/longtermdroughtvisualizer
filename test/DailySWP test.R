rm(list = ls(all = TRUE))

library(data.table)
#getwd()
data <- fread('runSWwithUIs/test/DailySWP.csv')
data$V1 <- NULL
head(data)

tail(data)

TP_DF <- data.frame(Year = c(1976:2015, 2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
data <- suppressMessages(plyr::join(TP_DF, data))

DatGCM <- data.table::setDT(data)[,.(mean = mean(value)),
              .(TP, RCP, GCM, Day, variable)]   

DatEnsemb <- data.table::setDT(DatGCM)[,.(mean = mean(mean),
                              median = median(mean),
                              min = min(mean), #change ranks
                              max = max(mean)),
                             .(TP, RCP, Day, variable)]      

DatEnsemb <- DatEnsemb[DatEnsemb$RCP %in% c('RCP85'), ]

# Format and Make Ribbon DF
# Only Near and Late get ribbons
RibbonDF <- DatEnsemb[DatEnsemb$TP %in% c('Near', 'Late'), ]

#### PLOT
lowVal <- -8
ysub <- ceiling(min(DatEnsemb$median)) - 1

if(ysub <= lowVal){ 
  DatEnsemb$mediean <- ifelse(DatEnsemb$median <= lowVal,lowVal, DatEnsemb$median)
  ysub <- lowVal
}

legendbkrd <- adjustcolor(col = '#FFFFFF',alpha.f = 0)

ggplot()+
  geom_line(data = DatEnsemb,aes(Day, median, color=as.factor(TP)),size=1.1)+
  #geom_ribbon(data = RibbonDF, aes(x = Day,ymin = min, ymax = max,fill=as.factor(TP)),
  #            alpha=0.2) + 
  #LEGEND
  scale_color_manual(values=c('black','#b8ae23','#a223b8'),name="",labels=c('Current','Near', 'Long-term'))+
  scale_fill_manual(values=c('#b8ae23','#a223b8'),name="",labels=c('Near', 'Long-term'),guide=FALSE) +
  theme_bw()
+ 
  theme_DSM + 
  #AXES
  labs(
    y = 'soil water potential (-MPa)',
    x = 'Month'
  )+
  scale_x_continuous(expand=c(0,0),breaks=c(1,29,60,91,121,152,182,213,244,274,305,335),
                     labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
  coord_cartesian(ylim = c(ysub,0)) +
  
  #FORMATTING
  uniformTheme

