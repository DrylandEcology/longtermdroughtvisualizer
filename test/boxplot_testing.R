#TP_DF <- data.frame(Year = c(1976:2015,2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
#data <- suppressMessages(plyr::join(TP_DF, data))
#data2 <- data[data$Year == 2015, ]
#data2 <- data2[data2$GCM == 'Current', ]
#data <- data[data$Year != 2015, ]
#data <- rbind(data, data2)
#data2 <- setDT(data)[,.(value = mean(value)),.(RCP, GCM, TP, Year, variable)]
#write.csv(data2, "annualdummydata.csv", row.names = FALSE)
#data2 <- setDT(data)[,.(value = mean(value)),.(RCP, GCM, TP, Year, Season, variable)]
#write.csv(data2, "seasonaldummydata.csv", row.names = FALSE)


rm(list = ls(all=TRUE))
source("functions/getOutputs.R")
library(data.table)
library(dplyr)

# TO do
# Figure out unique legend
# fix facet labels
# hover

annual <- read.csv("annualdummydata.csv")
annual$scenario <-  as.factor(paste(annual$RCP, annual$TP, annual$GCM, sep="_"))
annual$TP <- factor(annual$TP, levels =c ('Current','Near', 'Late'))

length(unique(annual$scenario))

# set factoring
# set colors
#fillGCM <- c('white', rep(RColorBrewer::brewer.pal(11, 'Paired'), 4))
#Scns <- sort(levels(annual$scenario))
#names(fillGCM) <- Scns
#str(fillGCM)

#fillScale <- scale_fill_manual(name = "GCM",values = fillGCM, guide = FALSE)#, labels =  sapply(strsplit(names(colorsGCM),'_'),'[', 3))
#rm(annual, Scns)
#save(list = ls(), file ='data/fillGCM.RData')

load('data/fillGCM.RData')
fillScale <- scale_fill_manual(name = "GCM",values = fillGCM, guide = FALSE)

ggplot(annual, aes(RCP, value, fill = fct_reorder(scenario, value, median))) +
  #bplots
  geom_boxplot(lwd=.8,position=position_dodge(.9)) +
  #shading and coloring
  fillScale + 
  #other
  theme_bw()+
  uniformTheme +   
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size =10)) + 
  facet_grid(. ~ TP, scales = 'free', space = 'free_x')

########################### -----

season <-read.csv("runSWwithUIs/seasonaldummydata.csv")
# set factoring
season$TP <- factor(season$TP, levels =c ('Current','Near', 'Late'))
season$scenario <- as.factor(paste(season$RCP, season$TP, season$GCM, sep="_"))
season$Season <- factor(season$Season, levels = c('Winter', 'Spring', 'Summer', 'Fall'))

ggplot(season, aes(RCP, value,  fill = fct_reorder(scenario, value, median))) +
  #bplots
  geom_boxplot(lwd=.8,position=position_dodge(.9)) +
  #shading and coloring
  fillScale + 
  #other
  theme_bw()+
  uniformTheme +
  facet_grid(Season ~ TP, scales = 'free', space = 'free_x')
                                 
                                              