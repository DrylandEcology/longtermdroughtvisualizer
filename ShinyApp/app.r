library(shiny)
library(shinydashboard)
library(foreach)
library(ggplot2)
library(plotly)


source('functions/set_execute_SW_functions.R')
source('functions/getOutputs.R')
source('functions/MiscFunctions.R')
source('functions/themes.R')
source('functions/weather_functions.R')

my_names <- list('HadGEM2-CC365' = 'HadGEM2-CC',
                 'CNRM-CM5' = 'CNRM-CM5',
                 #'MIROC5' = 'MIROC5',
                 'bcc-csm1-1' = 'bcc-csm1-1',
                 'HadGEM2-ES365' = 'HadGEM2-ES',
                 'CanESM2' = 'CanESM2',
                 'CCSM4' = 'CCSM4',
                 'IPSL-CM5A-MR' = 'IPSL-CM5A-MR'
                 )

my_colors <-  c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
                "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")

my_selected <- c('HadGEM2-CC365', 'CNRM-CM5',
                  'bcc-csm1-1', 'HadGEM2-ES',
                  'CanESM2', 'CCSM4', 'IPSL-CM5A-MR')


ui <- fluidPage(

  #titlePanel("Long-term Drought Simulator"),
  tabsetPanel(id = "mainTabset",               # IDEA: Two tabs - site-by-site & multiple sites (file upload).


              # Sidebar layout with site-by-site definitions ----
              tabPanel("Inputs",
                       htmlTemplate("www/index.html")
                     # Main panel for outputs ----
                     #mainPanel("Welcome to the long-term drought simulator!")
              ) #end of tab 1
  )#, # end of tabset
  # Important! : 'Freshly baked' tabs first enter here.
  #uiOutput("creationPool", style = "display: none;")
  # End Important
)# end of UI

# Define server logic ----
server <- function(input, output, session) {
  # reference to external UI elements
  # leaflet map
  tags$link(href = "leaflet.css")
  tags$script(src = "leaflet.js")
  # in house javascript
  tags$script(src = "master.js")
  # bootstrap
  tags$link(href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
  tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js")
  tags$link(href = "stylesheets/custom.css")
  # Important! : creationPool should be hidden to avoid elements flashing before they are moved.
  #              But hidden elements are ignored by shiny, unless this option below is set.
  output$creationPool <- renderUI({})
  outputOptions(output, "creationPool", suspendWhenHidden = FALSE)
  # End Important
  # Important! : This is the make-easy wrapper for adding new tabPanels.

  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})

    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  # End Important
  #################################################################################################
  ######## ---------------------------- SIMULATION!  ------------------------------------  ########
  #################################################################################################

  run <- reactiveValues() # set up list of values which will store simulation/function output

  observeEvent(input$simulate, {

    curr_year <- lubridate::year(Sys.Date())

    begintime <- proc.time() # start timer clock
    #showModal(modalDialog("calculation running!"))

    run$outs <- set_execute_SW(input$lat, input$lng, input$future,
                                 dir = "Data/WeatherData/www.northwestknowledge.net/metdata/data/",
                                 input$soils, input$sand, input$clay,
                                 input$comp, input$trees, input$shrubs,
                                 input$grasses, input$forbs, input$bg,
                                 curr_year) # the actual calculation

    #print(run$outs) #- What is the point of the run object. Do I need it? will I access
    #run$outs <- get_output()

    endtime <- proc.time() - begintime
    showModal(modalDialog(paste("calculation finished in", round(unname(endtime[3]), 0), "seconds")))
    ##################################################################################################
    ######## --------------------------------- Output UI  ---------------------------------  ########
    #################################################################################################

    ############################################################
    ###### Tab 1 Design
    ############################################################
    insertTab(inputId = "mainTabset",
              tabPanel(title = "Annual mean patterns", value = "outputs1",

                       fluidRow(
                         box(title = h1('Annual mean patterns'),
                                 width = 10, br(),
                            h4(paste0("What do the climate and soil moisture of site ", round(input$lat, 2), ", ", round(input$lng, 2), " look
                            like during an average year? How will these patterns change in the future?"),
                               br(),
                               br(),
                            "Here we present monthly means and sums of daily climate and soil moisture averaged
                            over 40-year periods. Use the plot controls to explore different representation
                            concentration pathways (RCPs), and how individual years through history and
                            climate models (GCMs) compare to the mean. Variables represented here include
                            average temperature (C), total precipitation (mm), and mean soil moisture
                            (measured as soil water potential, -MPa) at intermediate (20 – 80cm) soil depths."
                            )
                       )
                     ), #end of row 1

                       br(),
                     br(),

                       fluidRow(
                         box(title = 'Walter-Leith Climate Diagram and Monthly Mean Soil-Moisture',  width = 3,
                             "These plots depict average monthly fluctuations in climate and soil
                             moisture. The solid lines represent the median values for the
                             current time period (1980 – 2019), while the shaded polygons represent the range of
                             potential values across 11 climate models (GCMs), ensembled on a monthly basis, for each
                             representative concentration pathway (RCP), and for two future time periods (2020 - 2059,
                             2060 - 2099). Individual GCMs and historical years can be overlain.",
                             br(), br(),

                             "This graph allows the user to visualize the inter-play between temperature, precipitation,
                             and soil moisture. Comparing conditions under historical climate with conditions expected in the
                             future enables users to identify the direction and magnitude of robust changes in which all or most climate models agree.
                             Additionally, individual historical years of interest (i.e. known to be wetter or drier than usual)
                             can be compared to expected future conditions."
                             ),
                         box(plotlyOutput("WL_SM_Plots", height = "800px"), width = 6),

                      ############################################################
                      ###### Side bar options
                      ############################################################
                       box(title = 'Plot Controls', width = 3,
                         # If future is turned on
                         conditionalPanel(
                           condition = "input.future == 1",
                             # Select a RCP -----------------
                           selectInput("RCP", "Select a RCP:",
                                       c("RCP 4.5" = "rcp45",
                                         "RCP 8.5" = "rcp85"))
                         ),

                         # DO you want to add a specific year?
                         radioButtons("yearButton", label = h4("Individual year?"),
                                      choices = list('Yes' = 1, 'No' = 2),
                                      inline = TRUE, # side-by-side
                                      selected = 2),

                         conditionalPanel(
                           condition = "input.yearButton == 1",

                           # Select a specific year ----------
                           numericInput("years2", label = "Select a year:",
                                        min = 1980, max = 2020,# fix!
                                        value = c(2012))
                         ),

                         # If future is turned on
                         conditionalPanel(
                           condition = "input.future == 1",

                           # DO you want to add a GCM?
                           radioButtons("gcmsButton", label = h4("Individual GCM?"),
                                        choices = list('Yes' = 1, 'No' = 2),
                                        inline = TRUE, # side-by-side
                                        selected = 2),

                           conditionalPanel(
                             condition = "input.gcmsButton == 1",

                             # Select GCMs --------------------
                             selectInput("gcms2", "Select a GCM:",
                                         c(my_names)))
                         )
                         )# end of sidebar layout boxx
                      ),
                      br()
              ), # end of tab
              target = "Inputs", position = 'after')

    ############################################################
    ###### TAB 2 DESIGN
    ############################################################
    insertTab(inputId = "mainTabset",
              tabPanel(title = "Long-term past and future", value = "outputs2",

                       fluidRow(
                         box(title = h1('Long-term Annual Patterns'),  width = 10,
                        br(),
                        h4('What are the historical patterns of climate and soil moisture? How might they change in the future?', br(), br(),
                            paste0("The plots below depict long-term patterns of climate and soil moisture for
                             site ", round(input$lat, 2), ", ", round(input$lng, 2) ,". Data can be explored either as yearly or seasonal means, by selecting
                             from the time-step menu. Variables include average temperature (C),
                             total precipitation (cm), and mean soil moisture (measured as soil water potential, -MPa)
                             at intermediate (20 – 80cm) soil depths.")))
                       ),
                       br(),

                       fluidRow(
                         box(title = 'Long-term Historical Perspectives',  width = 8,
                             paste0("This plot depicts long-term historical fluctuations. This long-term perspective is useful for
                                    identifying unusual historical years and for recognizing the range of
                                    typical variation among years over the past century. Mean values for each year
                                    from 1980 to 2019 and are shown in the thin line, and the 10-year moving average
                                    is shown in the thick line.")
                             )
                             ), #END OF ROW 3
                       br(),

                       fluidRow(
                        box( plotlyOutput("TimeSeries"), width = 8),

                        box(title = 'Plot Controls',
                             solidHeader = TRUE,
                             width = 4,
                             background = 'black',

                             br(),
                                       #time-step
                                       selectInput("times", "Time-step:",
                                                   c("Annual" = "Annual",
                                                     "Seasonal" = "Season")),
                                       # "Month" = "Month")),
                                       #variable
                                       selectInput("variables", "Variable:",
                                                   c("Soil Moisture (SWP)" = "Soil Moisture (SWP, -MPa)",
                                                     "Temperature (C) " = "Average Temperature (C)",
                                                     "Precipitation (cm)" = "Precipitation (cm)")),
                                       #Years
                                       sliderInput("years", label = "years",
                                                   min = 1980, max = curr_year - 1,
                                                   value = c(1980, curr_year - 1),
                                                   sep = "")
                                     ) # end of box layout
                         ), #end of row 2
                       br(),
                       br(),

                       if(input$future == 1) {# BOX PLOT - only if future == 1
                         fluidRow(
                           box( title = 'Predicted Distribution of Future Values', width = 10,
                                paste0("This plot depicts the distribution of annual values, derived
                                       from observed historical conditions (1980-2019; black boxplot
                                       and violin plot on left) and derived from a suite of climate
                                       models (colored box plots on right) for two future time periods
                                       (2020-2059 and 2060-2099) and two representative concentration
                                       pathways (RCPs). Comparing conditions under historical climate
                                       with conditions expected in the future enables users to identify
                                       the direction and magnitude of robust changes in which all or
                                       most climate models agree. Because the distributions represent
                                       variation among years, users can detect not only changes in typical
                                       conditions (e.g. mean or median years) but also changes in extreme years
                                       (e.g. the tails of the distributions represented in the boxplots).
                                       In addition, users can identify and choose particular climate models
                                       to view, allowing direct comparison with other climate synthesis efforts or
                                       publications that may have focused on climate projections from specific models. ")
                                )
                          )#end of row 4
                       },
                       br(),

                       if(input$future == 1) {# BOX PLOT - only if future == 1

                          fluidRow(
                             box( plotlyOutput("BoxPlotAnnual"), width = 12)
                          )
                       },

                       if(input$future == 1) {# BOX PLOT - only if future == 1

                         fluidRow(
                           box( plotOutput("BoxPlotSeasonal", height = "600px"), width = 12)
                         )
                       }
                       #end of row 5

                       ), # end of tab
              target = "outputs1", position = 'after')


    updateTabsetPanel(session, "mainTabset", selected = "outputs1")

  })

  ##############################################################################
  ##############################################################################
  ######## --------------- Output Reactivate ---------------  ########
  ##############################################################################
  ##############################################################################

  ##############################################################################
  ######## --------------  -  --  Tab 1 ---- - - - - - - - -------------- ######
  ##############################################################################
  output$WL_SM_Plots <- renderPlotly({

    req(input$years) #input$years doesn't initally have values .. need this
    data <- run$outs
    data <- data[[1]]

    # Curr data -------------------------
    data_WL_SM <- format_data_WL(data = data, future = input$future)
    data_WL_SM_curr <- data_WL_SM[[1]]

    y_SM <- list( # Need this year because range argument can't be populated until now
      title = "soil water potential (-MPa)",
      range = c(-4, 0),
      showgrid = FALSE,
      linecolor = "black",
      linewidth = 0.5)

    # PLOTTING    --------------------------------------------------------------
    if(input$future == 2){

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

      # Soil Moisture ----------------------------------
      SM_Plot <-  plot_ly() %>%
        add_lines(data = data_WL_SM_curr, x = ~Month, y = ~SWP ,
                  line = list(color= 'black', width = 2),
                  name = 'Current SWP', showlegend = FALSE) %>%
        layout(yaxis = y_SM,
               xaxis = x_SM_WL)
    }

    # Future ------------------------------------------------------------
    if(input$future == 1) {

      # # WL Future ------------------------------------------------------------
      data_WL_SM_fut_ensemble <- data_WL_SM[[3]]
      data_WL_SM_fut_ensemble <-
        data_WL_SM_fut_ensemble[RCP %in% paste(input$RCP), ]

      data_WL_SM_fut_ensemble_format <-
        format_future_WL_plotly(data_WL_SM_fut_ensemble)

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
    }

    # Individual Years --------------------------------------------------
    if(input$yearButton == 1) {
      #ind_year_dat <- data.table::data[Year == 2015 & GCM == 'Current', ]
      ind_year_dat <- data[Year == input$years2 & GCM == 'Current', ]
      ind_year_dat <- data.table::dcast(ind_year_dat, formula  =
                              Year + Month + GCM + RCP + Season ~ variable)

      ind_year_dat <- rbind(ind_year_dat[1,], ind_year_dat,
                            ind_year_dat[12,])
      ind_year_dat$Month[c(1,14)] <- c(.5, 12.5)


      #plot
      WL_Plot <-  WL_Plot %>%
        add_lines(data = ind_year_dat,
                  x = ~Month, y = ~`Average Temperature (C)` , yaxis = 'y1',
                  line = list(color='#a50f15', dash = 'dash'),
                  name = paste0('Temp ', input$years2),
                  legendgroup = 'group2') %>%
        add_lines(data = ind_year_dat,
                  x = ~Month, y = ~`Precipitation (cm)` , yaxis = 'y2',
                  line = list(color='#08519c', dash = 'dash'),
                  name = paste0('Precip ', input$years2),
                  legendgroup = 'group2')

      SM_Plot <-  SM_Plot %>%
        add_lines(data = ind_year_dat,
                  x = ~Month, y = ~`Soil Moisture (SWP, -MPa)`,
                  line = list(color='black', dash = 'dash'),
                  name = paste0('SWP ', input$years2),
                  legendgroup = 'group2') %>%
        layout(showlegend = TRUE,
               legend = list(x = .05, y = .05))
    }

    # Individual GCMs  -----------------------------------------
    if(input$gcmsButton == 1) {

      #subset
      ind_GCM_dat <- data_WL_SM[[2]]
      ind_GCM_dat <- ind_GCM_dat[RCP == input$RCP & GCM == input$gcms2, ]
      #ind_GCM_dat <- ind_GCM_dat[RCP == 'rcp85' & GCM == 'MIROC-ESM', ]

      # format
      ind_GCM_dat_near <- ind_GCM_dat[ind_GCM_dat$TP == 'Near', ]
      ind_GCM_dat_near <- data.table::dcast(ind_GCM_dat_near,
                                            RCP + TP + GCM + Month ~ variable)
      ind_GCM_dat_near <- rbind(ind_GCM_dat_near[1,], ind_GCM_dat_near,
                                ind_GCM_dat_near[12,])
      ind_GCM_dat_near$Month[c(1,14)] <- c(.5, 12.5)

      # -
      ind_GCM_dat_late <- ind_GCM_dat[ind_GCM_dat$TP == 'Late', ]
      ind_GCM_dat_late <- data.table::dcast(ind_GCM_dat_late,
      RCP + TP + GCM + Month ~ variable)
      ind_GCM_dat_late <- rbind(ind_GCM_dat_late[1,], ind_GCM_dat_late,
                                ind_GCM_dat_late[12,])
      ind_GCM_dat_late$Month[c(1,14)] <- c(.5, 12.5)

      WL_Plot <-  WL_Plot %>%
        add_lines(data = ind_GCM_dat_near,
                  x = ~Month, y = ~`Average Temperature (C)` , yaxis = 'y1',
                  line = list(color='#a50f15', dash = 'dot'),
                  name = paste0(input$gcms2, ' Near Temp'),
                  showlegend = FALSE) %>%
        add_lines(data = ind_GCM_dat_near,
                  x = ~Month, y = ~`Precipitation (cm)` , yaxis = 'y2',
                  line = list(color='#08519c', dash = 'dot'),
                  name = paste0(input$gcms2, ' Near Precip '),
                  showlegend = FALSE) %>%
        # late
        add_lines(data = ind_GCM_dat_late,
                  x = ~Month, y = ~`Average Temperature (C)` , yaxis = 'y1',
                  line = list(color='#a50f15', dash = 'dashdot'),
                  name = paste0(input$gcms2, ' Late Temp'),
                  showlegend = FALSE) %>%
        add_lines(data = ind_GCM_dat_late,
                  x = ~Month, y = ~`Precipitation (cm)` , yaxis = 'y2',
                  line = list(color='#08519c', dash = 'dashdot'),
                  name = paste0(input$gcms2, ' Late Precip '),
                  showlegend = FALSE)

      # ----------------------------------------------------------------
      SM_Plot <-  SM_Plot %>%
        add_lines(data = ind_GCM_dat_near,
                  x = ~Month, y = ~`Soil Moisture (SWP, -MPa)`,
                  line = list(color='#b8ae23', dash = 'dot'),
                  name = paste0(input$gcms2, ' Near'),
                  legendgroup = 'group3') %>%
        add_lines(data = ind_GCM_dat_late,
                  x = ~Month, y = ~`Soil Moisture (SWP, -MPa)`,
                  line = list(color='#a223b8', dash = 'dashdot'),
                  name = paste0(input$gcms2, ' Late'),
                  legendgroup = 'group3') %>%
        layout(showlegend = TRUE,
               legend = list(x = .05, y = .05))
    }

    subplot(WL_Plot, SM_Plot, nrows = 2, shareX = TRUE, titleY = TRUE)

  })

  ##############################################################################
  ######## ---------------- - -- Tab 2  ---- - - - - - - - ---------------######
  ##############################################################################

  output$TimeSeries <- renderPlotly({

    curr_year <- lubridate::year(Sys.Date())

    req(input$years) #input$years doesn't initially have values .. need this
    data <- run$outs
    data <- data.table::setDT(data[[1]])

    # Var value from drop down / select Input - get variable -------------------
    data <- data[variable %in% c(input$variables), ]

    # Time Series controls -----------------------------------------------------
    ## Years from slider input --------
    data2 <- data[Year %in% c(input$years[1]:input$years[2]), ]

    data_time_series <- format_data_TS(data = data2,
                                       variable = input$variables,
                                       time = input$times, c(curr_year - 1))

    dataRM <- get_roll(data_time_series, time = input$times)

    # PLOTTING    --------------------------------------------------------------
    if(input$times == 'Annual'){

      # TIME SERIES PLOT
      TREND <- ggplot(data_time_series, aes(Year, value)) +
        geom_line(data=data_time_series, alpha=.3) +
        geom_line(data=dataRM, aes(Year, MA), size=1.5) +
        labs(
          x = 'years',
          y = paste(unique(input$variables))
        )+
        theme_bw() +
        ggtitle("Long-Term Historical Perspectives") +
        theme(plot.title = element_text(hjust = 0.5))
          }

    if(input$times == 'Season') {
      # TIME SERIES PLOT
      TREND <- ggplot(data_time_series,
                      aes(Year, value, color = Season)) +
        geom_line(data=data_time_series, alpha=.3)+
        geom_line(data=dataRM, aes(Year, MA), size=1.5) +
        labs(
          x = 'years',
          y = paste(unique(input$variables))
        ) +
        theme_bw() +
        scale_color_manual(values = colors_ts) +
        theme(legend.position=c(.5,0.07), legend.title=element_blank()) +
        ggtitle("Long-Term Historical Perspectives") +
        theme(plot.title = element_text(hjust = 0.5))
    }
    ggplotly(TREND)
  })

  # BOX PLOTS
  output$BoxPlotAnnual <- renderPlotly({
    if(input$times == 'Annual') {

      # Boxplot controls ----------------------------------------------------------------------
      # Var value from drop down / select Input - get variable ------------------------------------
      data <- run$outs
      data <- data[[1]]
      data <- data[variable %in% c(input$variables), ]

      data_BP <- format_data_BP(data = data,
                                variable = input$variables, time = input$times)

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

      # plotting specifics ---------------------------------------------------
      MAX <- max(data_BP_fut$value, na.rm = TRUE)
      if(input$variables == "Soil Moisture (SWP, -MPa)") Mult <- -1
      if(input$variables == "Average Temperature (C)") Mult <- .1
      if(input$variables == "Precipitation (cm)") Mult <- .05

      topText <- list(
        x = c(1.5, 3.5),
        y =  rep(MAX + (Mult * MAX), 2),
        text = c( '2020-2059', '2060-2099'),
        showarrow = FALSE,
        font = list(size = 14)
      )

      y_axis_bp <- list(title =  paste(unique(input$variables)),
                    range = c(min(data_BP_fut$value - 1, na.rm = TRUE),
                              topText$y[1] + .1),
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
                xaxis = x_axis_bp,
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
      load('data/fillGCM.RData')
      fillScale <- scale_fill_manual(name = "GCM",
                                     values = fillGCM2, guide = FALSE)

    # Var value from drop down / select Input - get variable ------------------------------------
      data <- run$outs
      data <- data[[1]]
      data <- data[data$variable %in% c(input$variables), ]

      data_BP <- format_data_BP(data = data, variable = input$variables,
                             time = input$times)
      data_BP$scenario <-  as.factor(paste(data_BP$RCP, data_BP$TP,
                                         data_BP$GCM, sep="_"))
      data_BP$TP <- factor(data_BP$TP,
                         levels =c ('Current','Near', 'Late'))
      data_BP$Season <- factor(data_BP$Season,
                            levels = c('Winter', 'Spring', 'Summer', 'Fall'))
      data_BP <- data_BP[complete.cases(data_BP), ]

    # plot
      BOXPLOT <- ggplot(data_BP, aes(RCP, value,
                                   fill = forcats::fct_reorder(scenario, value,
                                                               median))) +
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
          y = paste(unique(data_BP$variable)[1]),
             title = "Predicted Distribution of Future Values by Season")

    BOXPLOT
        }
}) # end of seasonal boxplot

}
# Run the app ----
shinyApp(ui = c(htmlTemplate("www/header.html"), htmlTemplate("www/headerImages.html"), ui, htmlTemplate("www/footer.html")), server = server)
