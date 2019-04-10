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


setwd("/Users/candrews/Documents/Git/longtermdroughtsimulator/")
rm(list = ls(all=TRUE))
source("ShinyApp/functions/getOutputs.R")
library(data.table)
library(dplyr)
#devtools::install_github("ropensci/plotly")
library(plotly)


# Datasetup
annual <- read.csv("test/annualdummydata.csv")
annual$RCP2 <- paste(annual$RCP, annual$TP, sep = '_')
annual$RCP2 <- factor(annual$RCP2, levels = c('Current_Current', "RCP45_Near", "RCP85_Near", "RCP45_Late", "RCP85_Late" ))
levels(annual$RCP2) <- c('Historical',  "RCP45 ", "RCP85 ", "RCP45", "RCP85")

Hist <- annual[annual$RCP2 == 'Historical', ]
annual2 <- annual[!annual$RCP2 == 'Historical', ]
annual2$GCM <- droplevels(annual2$GCM)

annual2 <- annual2 %>% 
  group_by(GCM) %>%
  mutate(med = median(value)) %>%
  arrange(GCM, med)

# Plot things
xaxis <- list(title = "",        zeroline = FALSE,
              showline = FALSE)
yaxis <- list(title =  paste(unique(droplevels(annual$variable))),
              range(min(annual$value), max(annual$value)),
              zeroline = FALSE,
              showline = FALSE)

# Current plot
current <- plot_ly(data = Hist, y = ~value,  x = ~RCP2, showlegend = FALSE, 
                   color = I('white'), type = 'violin') %>% 
  add_trace(box = list(visible = TRUE),line = list( color = 'black')) %>%
 
  layout(yaxis = yaxis,
         xaxis = xaxis)

current

# future plot ----
topText <- list(
  x = c(1.5, 3.5),
  y =  rep(max(annual$value) + .1, 2),
  text = c( '2020-2059',  '2060-2099'),
  showarrow = FALSE,
  font = list(size = 16)
)

colors2 <- c( brewer.pal(11, 'Paired'))

BOXPLOT <-  
  plot_ly(annual2, x = ~RCP2, y = ~value, color = ~GCM, type = 'box',
          colors = 'Paired') %>%
  add_trace(data = Hist, y = ~value,  x = ~RCP2, showlegend = FALSE, 
            color = I('white'), type = 'violin', box = list(visible = TRUE), line = list( color = 'black')) %>%

layout(
       boxmode = 'group',
       boxgap = 0.1,
       title = "Predicted Distribution of Future Values",
       yaxis = yaxis,
       xaxis = xaxis, 
       annotations = topText,
       legend = list(x = 100, y = 0.5),
       margin = list(top = 20),
       shapes = list(
         list(type = 'line', color = 'grey', opacity = .5, y0 = floor(yaxis[[2]][1]), y1 = ceiling(yaxis[[2]][2]), x0 =2.5, x1 = 2.5),
         list(type = 'line', color = 'black',  y0 = median(Hist$value), y1 = median(Hist$value), x0 = .5, x1 = 4.7))
       )
                             

 BOXPLOT






p <- ggplot(annual, aes(RCP2, value, fill = fct_reorder(GCM, value, median)))+
  #bplots
  geom_boxplot(lwd=.8,position=position_dodge(.9)) +
  #other
  theme_bw()+
  uniformTheme +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size =10)) 

p

ggplotly(p) %>% 
  layout(boxmode = "group", 
         title = "Distribution of Future Values",
         yaxis = yaxis,
         xaxis = xaxis, 
         annotations = topText)


# seasonal ---------------------------


season <-read.csv("test/seasonaldummydata.csv")
season$RCP2 <- paste(season$RCP, season$TP, sep = '_')
season$RCP2 <- factor(season$RCP2, levels = c('Current_Current', "RCP45_Near", "RCP85_Near", "RCP45_Late", "RCP85_Late" ))
levels(season$RCP2) <- c('Historical',  "RCP45 ", "RCP85 ", "RCP45", "RCP85")

Hist <- season[season$RCP2 == 'Historical', ]
season2 <- season[!season$RCP2 == 'Historical', ]

season2 <- season2 %>% 
  group_by(GCM, Season) %>%
  mutate(med = median(value)) %>%
  arrange(Season,GCM, med)

summerHist <- Hist[Hist$Season == 'Summer', ]
summerseason2 <-  season2[season2$Season == 'Summer', ]

summer <- 
  season2 %>% 
  filter(Season == 'Summer') %>%
  plot_ly() %>% 
  add_boxplot(data = summerHist, y = ~value,  x = ~RCP2, name = 'Historical', showlegend = FALSE,
              line = list(color = 'black'), marker = list(color = 'black')) %>%
  add_boxplot(data = season2, x = ~RCP2, y = ~value, color = ~GCM, legendgroup = ~GCM) %>%
  layout(boxmode = "group", 
         boxgap = 0.2,
         title = "Distribution of Future Values by Season",
         yaxis = yaxis,
         xaxis = xaxis, 
         annotations = topText)


winterHist <- Hist[Hist$Season == 'Winter', ]
winterseason2 <-  season2[season2$Season == 'Winter', ]
winter <- 
  season2 %>% 
  filter(Season == 'Winter') %>%
  plot_ly() %>% 
  add_boxplot(data = winterHist, y = ~value,  x = ~RCP2, name = 'Historical', showlegend = FALSE,
              line = list(color = 'black'), marker = list(color = 'black')) %>%
  add_boxplot(data = season2, x = ~RCP2, y = ~value, color = ~GCM, showlegend = FALSE,
              legendgroup = ~GCM,  alignmentgroup = ~GCM) %>%
  layout(boxmode = "group", 
         boxgap = 0.2,
         yaxis = yaxis,
         xaxis = xaxis
  )

springHist <- Hist[Hist$Season == 'Spring', ]
springseason2 <-  season2[season2$Season == 'Spring', ]

spring <- plot_ly() %>% 
  add_boxplot(data = springHist, y = ~value,  x = ~RCP2, name = 'Historical', showlegend = FALSE,
              line = list(color = 'black'), marker = list(color = 'black')) %>%
  add_boxplot(data = springseason2, x = ~RCP2, y = ~value, color = ~GCM, showlegend = FALSE,
              legendgroup = ~GCM, alignmentgroup = ~GCM) %>%
  layout(boxmode = "group", 
         boxgap = 0.2,
         yaxis = yaxis,
         xaxis = xaxis
  )

fallHist <- Hist[Hist$Season == 'Fall', ]
fallseason2 <-  season2[season2$Season == 'Fall', ]

fall <- plot_ly() %>% 
  add_boxplot(data = fallHist, y = ~value,  x = ~RCP2, name = 'Historical', showlegend = FALSE,
              line = list(color = 'black'), marker = list(color = 'black')) %>%
  add_boxplot(data = fallseason2, x = ~RCP2, y = ~value, color = ~GCM, showlegend = FALSE,
              legendgroup = ~GCM) %>%
  layout(boxmode = "group", 
         boxgap = 0.2,
         yaxis = yaxis,
         xaxis = xaxis
  )


p <- subplot(winter, summer, nrows =2, shareX = TRUE)
p


ggplot(season, aes(RCP2, value,  fill = fct_reorder(GCM, value, median))) +
  #bplots
  geom_boxplot(lwd=.8,position=position_dodge(.9)) +
  #shading and coloring
  fillScale +
  #other
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill="white"))+#,
  #strip.text = element_text(size =10)) +
  facet_grid(Season ~ TP, scales = 'free', space = 'free_x')

p <- ggplot(season, aes(RCP2, value,  fill = GCM)) +
  #bplots
  geom_boxplot(lwd=.8,position=position_dodge(.9)) +

  #other
  theme_bw()+
  uniformTheme +
  facet_grid(Season ~ ., scales = 'free', space = 'free_x')

p
ggplotly(p) %>% 
  layout(boxmode = "group")






########################### -----


# set factoring
# set colors
#fillGCM <- c('white', rep(RColorBrewer::brewer.pal(11, 'Paired'), 4))
#Scns <- sort(levels(annual$scenario))
#names(fillGCM) <- Scns
#str(fillGCM)

#fillScale <- scale_fill_manual(name = "GCM",values = fillGCM, guide = FALSE)#, labels =  sapply(strsplit(names(colorsGCM),'_'),'[', 3))
#rm(annual, Scns)
#save(list = ls(), file ='data/fillGCM.RData')
rm(list = ls(all = TRUE))
load('ShinyApp/data/fillGCM.RData')

fillScale <- scale_fill_manual(name = "GCM", values = fillGCM, guide = FALSE)

annual <- read.csv("test/annualdummydata.csv")
annual$scenario <-  as.factor(paste(annual$RCP, annual$TP, annual$GCM, sep="_"))
annual$TP <- factor(annual$TP, levels =c ('Current','Near', 'Late'))
annual$RCP2 <- paste(annual$RCP, annual$TP, sep = '_')
annual$RCP2 <- factor(annual$RCP2, levels = c('Current_Current', "RCP45_Near", "RCP85_Near", "RCP45_Late", "RCP85_Late" ))
levels(annual$RCP2) <- c('Historical',  "RCP45 ", "RCP85 ", "RCP45", "RCP85")

length(unique(annual$scenario))

p <- ggplot(annual, aes(RCP2, value, fill = fct_reorder(GCM, value, median))) +
  #bplots
  geom_boxplot(lwd=.8,position=position_dodge(.9)) +
  #shading and coloring
  fillScale + 
  #other
  theme_bw()+
  uniformTheme +   
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size =10)) 
  #facet_grid(. ~ TP, scales = 'free', space = 'free_x')

p

#####################
season <-read.csv("runSWwithUIs/seasonaldummydata.csv")
# set factoring
season$TP <- factor(season$TP, levels =c ('Current','Near', 'Late'))
season$scenario <- as.factor(paste(season$RCP, season$TP, season$GCM, sep="_"))
season$Season <- factor(season$Season, levels = c('Winter', 'Spring', 'Summer', 'Fall'))

                                     