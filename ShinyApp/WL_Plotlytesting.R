setwd('~/Documents/Git/longtermdroughtsimulator/test')
rm(list =ls(all = TRUE))
library(plotly)
library(data.table)
source('~/Documents/Git/longtermdroughtsimulator/ShinyApp/functions/MiscFunctions.R')
source('~/Documents/Git/longtermdroughtsimulator/ShinyApp/functions/getOutputs.R')
source('~/Documents/Git/longtermdroughtsimulator/ShinyApp/functions/diagWL2.R')

data <- fread('AllData.csv')
head(data)
unique(data$variable)

dataWL <- formatDataWL(data = data, future =1)
str(dataWL)
dataWL2 <- dataWL[[1]]

diagwl2(dataWL[[1]], RCP = 'RCP45',
        Year = 2, YearChoice = NA,
        GCM = 2, GCMc = NA,
        FUTURE50 = 1, FUTURE90 =1,
        data[[1]], dataWL[[3]], dataWL[[2]],
        est='',alt=NA, per='',margen=c(0.1, .5, 0.4, .2))

#

ay1 <- list(
  tickfont = list(color = "#a50f15"),
  side = "left",
  title = "Temperature (C)",
  range = c(-5, 69),
  autotick = FALSE,
  dtick = 10,  linecolor = "black",
  linewidth = 0.5,
  mirror = TRUE,
  showgrid = FALSE
)
  

ay2 <- list(
  tickfont = list(color = "#08519c"),
  overlaying = "y",
  side = "right",
  title = "Precipitation (mm)",
  range = c(-10, 139), 
  autotick = FALSE,
  dtick = 20,
  showgrid = FALSE
  )

ax <- list(
  range = c(.5, 12.5),
  autotic = FALSE,
  dtick = 1,
  showgrid = FALSE,
  ticklen = 5,
  tickwidth = 1,
  tickcolor = toRGB("black"),
  linecolor = "black",
  linewidth = 0.5,
  showticklabels = FALSE,
  title = ''
)

topTextWL <- list(
  x = c(2.4, 10.5),
  y =  rep(68, 2),
  text = c( 
    paste0('Current MAT: ', round(mean(dataWL2[2:13,'Temp']),1), ' C'), 
    paste0('Current MAP: ',round(sum(dataWL2[2:13, 'PPT'])), " mm")),
  showarrow = FALSE,
  font = list(size = 13)
  )


p <- plot_ly(data = dataWL2) %>%  
  add_lines(x = ~Month2, y = ~Temp , yaxis = 'y1',
            line = list(color='#a50f15'),
            name = 'Average Temp (C)', showlegend = FALSE) %>%
    add_lines(x = ~Month2, y = ~PPT, yaxis = "y2",
            line = list(color='#08519c'),
            name = 'Precip (mm)', showlegend = FALSE)  %>%
  layout(
    annotations = topTextWL,
    yaxis = ay1,
    yaxis2 = ay2,
    xaxis = x_DSMWL,
    shapes = 
      list(type = 'line', color = 'black',
           y0 = 60, y1 = 60, x0 =0, x1 = 12.5),
    showlegend = TRUE
  )
p




# --------------------------------------------------------------------------


# --------------------------------------------------------------------------


month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')
high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)
data <- data.frame(month, high_2014, low_2014)
data$average_2014 <- rowMeans(data[,c("high_2014", "low_2014")])
data$month <- factor(data$month, levels = data[["month"]])

plot_ly(data, x = ~month, y = ~high_2014, type = 'scatter', mode = 'lines',
        line = list(color = 'transparent'),
        showlegend = FALSE, name = 'High 2014') %>%
  add_trace(y = ~low_2014, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
            showlegend = FALSE, name = 'Low 2014')
