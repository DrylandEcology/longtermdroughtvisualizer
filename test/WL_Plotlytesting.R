rm(list = ls(all = TRUE))
setwd('~/Documents/Git/longtermdroughtsimulator')
source('ShinyApp/functions/getOutputs.R')
source('ShinyApp/functions/themes.R')
#rm(list =ls(all = TRUE))
library(plotly)
library(data.table)

curr_year <- lubridate::year(Sys.Date())
data <- fread('~/Documents/Git/longtermdroughtsimulator/test/AllData.csv')

#debugonce(format_data_WL)
data_WL_SM <- format_data_WL(data = data, future = 1, curr_year = curr_year)
data_WL_SM_curr <- data_WL_SM[[1]]

y_SM <- list( # Need this year because range argument can't be populated until now
    title = "soil water potential (-MPa)",
    range = c(-8, 0),
    showgrid = FALSE,
    linecolor = "black",
    linewidth = 0.5)

# ONLY HISTORICAL PLOTS!

# Walter-Leith Plot ----------------------------------
topTextWL <- list(
  x = c(2.4, 10.5),
  y =  rep(68, 2),
  text = c(
    paste0('Current MAT: ', 
           round(data_WL_SM_curr[2:13, mean(Temp)], 2), '°C'),
    paste0('Current MAP: ', 
           round(data_WL_SM_curr[2:13, sum(PPT)]), " mm")),
  showarrow = FALSE,
  font = list(size = 13)
)
    
WL_Plot <- plot_ly(data = data_WL_SM_curr) %>%
  add_lines(x = ~Month, y = ~Temp , yaxis = 'y1',
            line = list(color='#a50f15'),
            name = 'Average Temp (C)', showlegend = FALSE) %>%
  add_lines(x = ~Month, y = ~PPT, yaxis = "y2",
            line = list(color='#08519c'),
            name = 'Precip (mm)', showlegend = FALSE)  %>%
  ## layout and themes -> primarily set in functions/themes.R
  layout(
    annotations = topTextWL,
    yaxis = y1_WL,
    yaxis2 = y2_WL,
    xaxis = x_SM_WL,
    shapes =
      list(type = 'line', color = 'black',
           y0 = 60, y1 = 60, x0 = 0, x1 = 13)
  )
WL_Plot

# Soil Moisture ----------------------------------
SM_Plot <-  plot_ly() %>%
  add_lines(data = data_WL_SM_curr, x = ~Month, y = ~SWP ,
            line = list(color= 'black', width = 2),
            name = 'Current SWP', showlegend = FALSE) %>%
  layout(yaxis = y_SM,
         xaxis = x_SM_WL)

SM_Plot

# FUTURE PLOT! ---------------------------------------------------------------
# # WL Future ----------------------------------------------------------------------------------------
data_WL_SM_fut_ensemble <- data_WL_SM[[3]]
data_WL_SM_fut_ensemble <- data_WL_SM_fut_ensemble[RCP %in% paste('rcp85'), ]

data_WL_SM_fut_ensemble_format <- format_future_WL_plotly(data_WL_SM_fut_ensemble)

## text for top of climate figure
topTextWL <- list(
  x = rep(c(2.4, 10.5),3),
  y =  c(rep(69, 2), rep(65.5, 2), rep(62, 2)),
  text = c(
    paste0('Historical MAT: ',
           round(data_WL_SM_curr[2:13, mean(Temp)], 1), '°C'),
    paste0('Historical MAP: ',
           round(data_WL_SM_curr[2:13, sum(PPT)]), " mm"),
    paste0('2020-2059 MAT: ', 
           round(data_WL_SM_fut_ensemble_format[[1]][2:13,mean(mean)], 1), '°C'),
    paste0('2020-2059 MAP: ', 
           round(data_WL_SM_fut_ensemble_format[[2]][2:13, sum(mean)]), " mm"),
    paste0('2060-2099 MAT: ', 
           round(data_WL_SM_fut_ensemble_format[[4]][2:13,mean(mean)], 1), '°C'),
    paste0('2060-2099 MAP: ', 
           round(data_WL_SM_fut_ensemble_format[[5]][2:13, sum(mean)]), " mm")
  ),
  showarrow = FALSE,
  font = list(size = 13)
  )

WL_Plot <- plot_ly() %>%
  # near temp ---------------------------------------------------------------------
  add_trace(data = data_WL_SM_fut_ensemble_format[[1]], x = ~Month, y = ~max,
            type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'High 2020-2059') %>%
  add_trace(data = data_WL_SM_fut_ensemble_format[[1]], x = ~Month, y = ~min, 
            type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor = "rgba(239, 59, 44, .4)", 
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2020-2059') %>%
  # late temp
  add_trace(data = data_WL_SM_fut_ensemble_format[[4]], x = ~Month, y = ~max, 
            type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'High 2060-2099') %>%
  add_trace(data = data_WL_SM_fut_ensemble_format[[4]], x = ~Month, y = ~min, 
            type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor = "rgba(252, 146, 114, .4)",
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2060-2099') %>%
  # current temp
  add_lines(data = data_WL_SM_curr, x = ~Month, y = ~Temp , yaxis = 'y1',
            line = list(color='#a50f15'),
            name = 'Average Temp (C)', showlegend = FALSE) %>%
  
  # near ppt -----------------------------------------------------------------
  add_trace(data = data_WL_SM_fut_ensemble_format[[2]], x = ~Month, y = ~max,
            type = 'scatter', mode = 'lines',
          line = list(color = 'transparent'), yaxis = "y2",
          showlegend = FALSE, name = 'High 2020-2059') %>%
  add_trace(data = data_WL_SM_fut_ensemble_format[[2]], x = ~Month, y = ~min,
            type = 'scatter', mode = 'lines', yaxis = "y2",
            fill = 'tonexty', fillcolor = "rgba(44, 53, 232, .4)", line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2020-2059') %>%
  # late ppt
  add_trace(data = data_WL_SM_fut_ensemble_format[[5]], x = ~Month, y = ~max,
            type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'), yaxis = "y2",
            showlegend = FALSE, name = 'High 2060-2099') %>%
  add_trace(data = data_WL_SM_fut_ensemble_format[[5]], x = ~Month, y = ~min, 
            type = 'scatter', mode = 'lines', yaxis = "y2",
            fill = 'tonexty', fillcolor = "rgba(158, 202, 225, .4)", 
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2060-2099') %>%
  # current ppt
  add_lines(data = data_WL_SM_curr, x = ~Month, y = ~PPT, yaxis = "y2",
            line = list(color='#08519c'),
            name = 'Precip (mm)', showlegend = FALSE)  %>%
  # --------------------------------------------------------------------------
  layout(
    annotations = topTextWL,
    yaxis = y1_WL,
    yaxis2 = y2_WL,
    xaxis = x_SM_WL,
    shapes =
      list(type = 'line', color = 'black',
           y0 = 60, y1 = 60, x0 =0, x1 = 12.5)
  )
WL_Plot

# # SM Future ----------------------------------------------------------------------------------------
SM_Plot <- plot_ly() %>%
  # curr median
  add_lines(data = data_WL_SM_curr, x = ~Month, y = ~SWP ,
            line = list(color= 'black', width = 2),
            name = 'Current SWP', showlegend = FALSE) %>%
  # near ribbon
  add_trace(data = data_WL_SM_fut_ensemble_format[[3]], x = ~Month, y = ~max, type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'High 2020-2059') %>%
  add_trace(data = data_WL_SM_fut_ensemble_format[[3]], x = ~Month, y = ~min, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor = "rgba(184, 174, 35, .4)", line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2020-2059')  %>%
  # late ribbon
  add_trace(data = data_WL_SM_fut_ensemble_format[[6]], x = ~Month, y = ~max, type = 'scatter', mode = 'lines',
            line = list(color = 'transparent'),
            showlegend = FALSE, name = 'High 2060-2099') %>%
  add_trace(data = data_WL_SM_fut_ensemble_format[[6]], x = ~Month, y = ~min, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor = "rgba(162, 35, 184, .4)", line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2060-2099') %>%
  # medians
  add_trace(data = data_WL_SM_fut_ensemble_format[[3]], x = ~Month, y = ~median, type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(184, 174, 35)'), showlegend = TRUE, legendgroup = 'group1', name = 'Near (2020-2059)') %>%
  add_trace(data = data_WL_SM_fut_ensemble_format[[6]], x = ~Month, y = ~median, type = 'scatter', mode = 'lines',
            line = list(color = "rgb(162, 35, 184)"), showlegend = TRUE, legendgroup = 'group1', name = 'Late (2060-2099)') %>%
  layout(yaxis = y_SM,
         xaxis = x_SM_WL,
         showlegend = TRUE,
         legend = list(x = .05, y = .05))
SM_Plot

