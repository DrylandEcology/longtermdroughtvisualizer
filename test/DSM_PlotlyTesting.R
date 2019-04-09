data <- fread('AllData.csv')
head(data)
unique(data$variable)
dataDSM <- dataSM <- formatDataDSM(data = data, RCP = 'RCP45')
str(dataSM)

dataDSM2 <- dataDSM[[2]][dataDSM[[2]]$TP == 'Current',]
dataDSM2 <- rbind(dataDSM2[1,], dataDSM2, dataDSM2[12,])
dataDSM2$Month2 <- c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                    'August', 'September', 'October', 'November', 'December', 'December2')

dataDSM2$Month2 <- factor(dataDSM2$Month2, levels =c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                                                  'August', 'September', 'October', 'November', 'December', 'December2'))

y_DSM <- list(
  title = "soil water potential (-MPa)",
  range = c(dataDSM[[3]], 0), 
  showgrid = FALSE,
  linecolor = "black",
  linewidth = 0.5
)
  
DSM_Plot <-  plot_ly() %>%  
  add_lines(data = dataDSM2, x = ~Month2, y = ~median , 
            line = list(color= 'black', width = 2),
            name = 'Current SWP', showlegend = FALSE) %>%
  layout(yaxis = y_DSM,
         xaxis = x_DSMWL,
         showlegend = TRUE,
         legend = list(x = .75, y = .1))

DSM_Plot
p
subplot(p, DSM_Plot, nrows = 2, shareX = TRUE, titleY = TRUE)


RibbonDFNear <-  dataDSM[[2]][dataDSM[[2]]$TP %in% c('Near'), ]
RibbonDFNear <- rbind(RibbonDFNear[1,], RibbonDFNear, RibbonDFNear[12,])
RibbonDFNear$Month2 <- c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                     'August', 'September', 'October', 'November', 'December', 'December2')
RibbonDFNear$Month2 <- factor(RibbonDFNear$Month2, levels =c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                                                     'August', 'September', 'October', 'November', 'December', 'December2'))

RibbonDFLate <-  dataDSM[[2]][dataDSM[[2]]$TP %in% c('Late'), ]
RibbonDFLate <- rbind(RibbonDFLate[1,], RibbonDFLate, RibbonDFLate[12,])
RibbonDFLate$Month2 <- c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                         'August', 'September', 'October', 'November', 'December', 'December2')
RibbonDFLate$Month2 <- factor(RibbonDFLate$Month2, levels =c('January1', 'January', 'February', 'March', 'April', 'May', 'June', 'July',
                                                              'August', 'September', 'October', 'November', 'December', 'December2'))


DSM_Plot %>% 
  add_trace(data = RibbonDFNear, x = ~Month2, y = ~max, type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'High 2020-2059') %>%
  add_trace(data = RibbonDFNear, x = ~Month2, y = ~min, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor = "rgba(184, 174, 35, .4)", line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2020-2059')  %>%
  add_trace(data = RibbonDFLate, x = ~Month2, y = ~max, type = 'scatter', mode = 'lines',
          line = list(color = 'transparent'),
          showlegend = FALSE, name = 'High 2060-2099') %>%
  add_trace(data = RibbonDFLate, x = ~Month2, y = ~min, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor = "rgba(162, 35, 184, .4)", line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2060-2099') %>%
  add_trace(data = RibbonDFNear, x = ~Month2, y = ~median, type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(184, 174, 35)'), showlegend = TRUE, name = 'Near (2020-2059)') %>%
  add_trace(data = RibbonDFLate, x = ~Month2, y = ~median, type = 'scatter', mode = 'lines',
            line = list(color = "rgb(162, 35, 184)"), showlegend = TRUE, name = 'Late (2060-2099)') 
                










