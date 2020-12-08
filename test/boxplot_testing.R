#TP_DF <- data.frame(Year = c(1976:2015,2020:2099), TP = c(rep('Current', 40), rep('Near',40), rep('Late', 40)))
#data <- suppressMessages(plyr::join(TP_DF, data))
#data2 <- data[data$Year == 2015, ]
#data2 <- data2[data2$GCM == 'Current', ]
#data <- data[data$Year != 2015, ]
#data <- rbind(data, data2)
#data2 <- data.table::setDT(data)[,.(value = mean(value)),.(RCP, GCM, TP, Year, variable)]
#write.csv(data2, "annualdummydata.csv", row.names = FALSE)
#data2 <- data.table::setDT(data)[,.(value = mean(value)),.(RCP, GCM, TP, Year, Season, variable)]
#write.csv(data2, "seasonaldummydata.csv", row.names = FALSE)


rm(list = ls(all=TRUE))
source("ShinyApp/functions/getOutputs.R")
library(data.table)
#devtools::install_github("ropensci/plotly")
library(plotly)

# Datasetup
load('ShinyApp/data/fillGCM.RData')
fillScale <- scale_fill_manual(name = "GCM",values = fillGCM, guide = FALSE)
#  data3 <- data[data$GCM %in% c('Current', input$gcms), ]
data <- fread('test/AllData.csv')

# Var value from drop down / select Input - get variable ------------------------------------
data <- data[data$variable %in% 'Average Temperature (C)', ]
data_BP <- format_data_BP(data = data, variable = 'Average Temperature (C)', 
                          time = 'Annual')

data_BP$RCP2 <- paste(data_BP$RCP, data_BP$TP, sep = '_')
data_BP$RCP2 <- factor(data_BP$RCP2, levels = c('Current_Current', 
                                              "rcp45_Near", "rcp85_Near",
                                              "rcp45_Late", "rcp85_Late"))
levels(data_BP$RCP2) <- c('Historical',  "RCP45 ", "RCP85 ", "RCP45", "RCP85")

data_BP_hist <- data_BP[data_BP$RCP2 == 'Historical', ]
data_BP_fut <- data_BP[!data_BP$RCP2 == 'Historical', ]

data_BP_fut <- data_BP_fut %>%
  group_by(GCM) %>%
  mutate(med = median(value)) %>%
  arrange(GCM, med)

MAX <- max(data_BP_fut$value, na.rm = TRUE)
if(input$variables == "Soil Moisture (SWP, -MPa)") Mult <- -1
if(input$variables == "Average Temperature (C)") Mult <- .1
if(input$variables == "Precipitation (cm)") Mult <- .05

topText <- list(
  x = c(1.5, 3.5),
  y =  rep(MAX + (Mult * MAX), 2),
  text = c( '2020-2059',  '2060-2099'),
  showarrow = FALSE,
  font = list(size = 14)
)

y_axis_bp <- list(title =  paste(unique((data_BP_fut$variable))[1]),
              range  =c (min(data_BP_fut$value - 1, na.rm = TRUE), topText$y[1] + .1),
              zeroline = FALSE,
              showline = FALSE)

BOXPLOT <-
  plot_ly(data_BP_fut, x = ~RCP2, y = ~value, color = ~GCM, 
          type = 'box', colors = 'Paired') %>%
  add_trace(data = data_BP_hist, y = ~value, x = ~RCP2, showlegend = FALSE,
            color = I('NA'), type = 'violin', box = list(visible = TRUE),
            line = list( color = 'black')) %>%
  
  layout(boxmode = 'group',
         boxgap = 0.1,
         # axes
         title =  list(text = "Predicted Distribution of Future Values",
                       size = 18),
         yaxis = y_axis_bp,
         #xaxis = xaxis,
         margin = list(top = 100),
         # text and legend
         annotations = topText,
         legend = list(x = 100, y = 0.5),
         # add lines
         shapes = list(
           list(type = 'line', color = 'grey', opacity = .5,
                y0 = floor(y_axis_bp$range[1]), 
                y1 = ceiling(y_axis_bp$range[2]),
                x0 = 2.5, x1 = 2.5),
           list(type = 'line', color = 'black', opacity = .8,
                y0 = median(data_BP_hist$value,  na.rm = TRUE), 
                y1 = median(data_BP_hist$value,  na.rm = TRUE), 
                x0 = .5, x1 = 4.7))
  )
BOXPLOT

}
}) #end of TS and BP Plots / Tab 1 Plots



output$BoxPlotSeasonal <- renderPlot({
  if(input$times == 'Season'){
    
    ## GCMs from checkbox input --------
    fillScale <- scale_fill_manual(name = "GCM",values = fillGCM2, guide = FALSE)
    #  data3 <- data[data$GCM %in% c('Current', input$gcms), ]
    data <- run$outs
    data <- data[[1]]
    
    # Var value from drop down / select Input - get variable ------------------------------------
    data <- data[data$variable %in% c(input$variables), ]
    data <- data[variable %in%  'Average Temperature (C)', ]
    data_BP <- format_data_BP(data = data, 
                              variable = 'Average Temperature (C)', time = 'Season')
    data_BP$scenario <-  as.factor(paste(data_BP$RCP, data_BP$TP,
                                         data_BP$GCM, sep="_"))
    data_BP$TP <- factor(data_BP$TP, levels =c ('Current','Near', 'Late'))
    data_BP$Season <- factor(data_BP$Season, 
                             levels = c('Winter', 'Spring', 'Summer', 'Fall'))
    data_BP <- data_BP[complete.cases(data_BP), ]
    
    BOXPLOT <- ggplot(data_BP, aes(RCP, value,  
                                  fill = fct_reorder(scenario, value, median))) +
      #bplots
      geom_boxplot(lwd=.8,position=position_dodge(.9)) +
      #shading and coloring
      fillScale +
      #other
      theme_bw() +
      theme(legend.position = "bottom",
            strip.background = element_rect(fill="white"),
            plot.title = element_text(hjust = 0.5, size = 20),
            strip.text = element_text(size =12)) +
      facet_grid(Season ~ TP, scales = 'free', space = 'free_x') +
      labs( x= '',
            y = paste(),
            title = "Predicted Distribution of Future Values by Season")
    
    BOXPLOT
    