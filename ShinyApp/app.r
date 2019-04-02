library(shiny)
library(leaflet)
# analysis
library(rSFSW2)
library(rSOILWAT2)
library(zoo)
library(data.table)
library(sp)
library(maps)
library(maptools)
# Plotting
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(forcats)
library(gridGraphics)


source("functions/swGUIfuncv2.R")
source("functions/MiscFunctions.R")
source("functions/getOutputs.R")
source("functions/diagwl2.R")

#devtools::load_all(pkg = '~/Desktop/Dryland Ecology/rSFSW2/')
#devtools::load_all(pkg = '~/Desktop/Dryland Ecology/rSOILWAT2/')

my_names <- list( 'CanESM2' = 'CanESM2',
                'CESM1-CAM5' = 'CESM1-CAM5',  'CNRM-CM5' = 'CNRM-CM5',
                'CSIRO-Mk3-6-0' = 'CSIRO-Mk3-6-0', 'FGOALS-g2' = 'FGOALS-g2',
                'HadGEM2-ES' = 'HadGEM2-ES', 'GISS-E2-R' = 'GISS-E2-R',
                'inmcm4' = 'inmcm4', 'IPSL-CM5A-MR' = 'IPSL-CM5A-MR',
                'MIROC-ESM' = 'MIROC-ESM', 'MRI-CGCM3' = 'MRI-CGCM3')
my_colors <-  c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
                "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99")
my_selected <- c('CanESM2', 'CESM1-CAM5', 'CNRM-CM5',
                 'CSIRO-Mk3-6-0' , 'FGOALS-g2', 'HadGEM2-ES',
                 'GISS-E2-R','inmcm4', 'IPSL-CM5A-MR',
                 'MIROC-ESM', 'MRI-CGCM3' )


ui <- fluidPage(
  
  #titlePanel("Long-term Drought Simulator"),
  tabsetPanel(id = "mainTabset",               # IDEA: Two tabs - site-by-site & multiple sites (file upload).
              
              
              # Sidebar layout with site-by-site definitions ----
              tabPanel("Site-by-site",
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

    begintime <- proc.time() # start timer clock
    #showModal(modalDialog("calculation running!"))
    
    run$SW_out <- set_execute_SW(input$lat, input$lng, input$future, input$soils, input$sand, input$clay,
                                 input$comp, input$trees, input$shrubs, input$grasses, input$forbs, input$bg) # the actual calculation

    #print(run$SWout) - What is the point of the run object. Do I need it? will I access
    run$outs <- get_output()

    endtime <- proc.time() - begintime
    showModal(modalDialog(paste("calculation finished in", round(unname(endtime[3]), 0), "seconds")))
    ##################################################################################################
    ######## --------------------------------- Output UI  ---------------------------------  ########
    #################################################################################################
    insertTab(inputId = "mainTabset",
              tabPanel(title = "Mean annual patterns", value = "outputs1",
                       sidebarLayout(position = "left",
                                     sidebarPanel(

                                       # If future is turned on
                                       conditionalPanel(
                                         condition = "input.future == 1",
                                         # Select a RCP -----------------
                                         selectInput("RCP", "Select a RCP:",
                                                     c("RCP 4.5" = "RCP45",
                                                       "RCP 8.5" = "RCP85"))
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
                                                      min = 1916, max = 2015,
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
                                           selectInput("gcms2", "",
                                                       c(my_names)))
                                       )
                                     ), # end of sidebar layout

                                     # Main panel for outputs ----
                                     mainPanel("",
                                               plotOutput("WL_DSM_Plots", height = 700, width = 700)
                                     )
                       ) # end of sidebarlayout
              ), # end of tab
              target = "Site-by-site", position = 'after')

    insertTab(inputId = "mainTabset",
              tabPanel(title = "Long-term past and future", value = "outputs2",
                       sidebarLayout(position = "left",
                                     sidebarPanel(
                                       #time-step
                                       selectInput("times", "Time-step:",
                                                   c("Annual" = "Annual",
                                                     "Season" = "Season")),
                                       # "Month" = "Month")),
                                       #variable
                                       selectInput("variables", "Variable:",
                                                   c("Soil Moisture" = "Inter",
                                                     "Temp" = "avg_C",
                                                     "Precip" = "ppt")),
                                       #Years
                                       sliderInput("years", label = "years", min = 1916, max = 2015,
                                                   value = c(1916, 2015), sep = ""),

                                       # If future is turned on
                                       conditionalPanel(
                                         condition = "input.future == 1",
                                       # GCMs
                                         my_checkboxGroupInput("gcms", "GCMS:",
                                                               choices = my_names,
                                                               selected= my_selected,
                                                               colors= my_colors)
                                       )
                                     ), # end of sidebar layout


                                     # Main panel for outputs ----
                                     mainPanel("Long-Term Historical Perspectives",
                                               plotOutput("TS_BP_Plots",  height = 700, width = 700)
                                                          #hover = "plot_hover"), # for hovering specific years
                                               #verbatimTextOutput("info")
                                     )
                       ) # end of sidebarlayout
              ), # end of tab
              target = "outputs1", position = 'after')


    updateTabsetPanel(session, "mainTabset", selected = "outputs1")

  })
  # 
  # #################################################################################################
  # #################################################################################################
  # ######## ---------------------------- Output Reactivity  -----------------------------  ########
  # #################################################################################################
  # #################################################################################################
  # 
  # #################################################################################################
  # ######## ---------------- - - - - - - -----  Tab 1  ---- - - - - - - - -----------------  ########
  # #################################################################################################
  # 
  output$TS_BP_Plots <- renderPlot({

    req(input$years) #input$years doesn't initally have values .. need this
    data <- run$outs
    data <- data[[1]]

    # Var value from drop down / select Input - get variable ------------------------------------
    data <- data[data$variable %in% c(input$variables), ]

    # Time Series controls ----------------------------------------------------------------------
    ## Years from slider input --------
    data2 <- data[data$Year %in% c(input$years[1]:input$years[2]), ]
    dataTS <- formatDataTS(data = data2, variable = input$variables, time = input$times)
    dataRM <- getroll(dataTS, input$times)

    # Boxplot controls ----------------------------------------------------------------------
    ## GCMs from checkbox input --------
    load('data/fillGCM.RData')
    fillScale <- scale_fill_manual(name = "GCM",values = fillGCM, guide = FALSE)

    data3 <- data[data$GCM %in% c('Current', input$gcms), ]
    dataBP <- formatDataBP(data = data3, variable = input$variables, time = input$times)

    # PLOTTING    --------------------------------------------------------------------------------
    if(input$times == 'Annual'){
      # TIME SERIES PLOT
      TREND <- ggplot(dataTS, aes(Year, value))+
        geom_line(data=dataTS, alpha=.3)+
        geom_line(data=dataRM, aes(Year, MA), size=1.5) +
        labs(
           x = 'years'
        )+
      theme_bw()+
      uniformTheme

      # BOX PLOT - only if future == 1
      if(input$future == 1){
        BOXPLOT <- ggplot(dataBP, aes(RCP, value, fill = fct_reorder(scenario, value, median))) +
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
      }
    }

    if(input$times == 'Season'){
      # TIME SERIES PLOT
      TREND <- ggplot(dataTS, aes(Year, value, color = Season))+
        geom_line(data=dataTS, alpha=.3)+
        geom_line(data=dataRM, aes(Year, MA), size=1.5) +
        labs(
          x = 'years'
        )+
        theme_bw()+
        uniformTheme +
        scale_color_manual(values=colors2) +
        theme(legend.position=c(.5,0.07), legend.title=element_blank(),legend.direction="horizontal")

      # BOX PLOT - only if future == 1
      if(input$future == 1) {
        dataBP$Season <- factor(dataBP$Season, levels = c('Winter', 'Spring', 'Summer', 'Fall'))

        BOXPLOT <- ggplot(dataBP, aes(RCP, value,  fill = fct_reorder(scenario, value, median))) +
          #bplots
          geom_boxplot(lwd=.8,position=position_dodge(.9)) +
          #shading and coloring
          fillScale +
          #other
          theme_bw()+
          uniformTheme +
          theme(legend.position = "bottom",
                strip.background = element_rect(fill="white"))+#,
                #strip.text = element_text(size =10)) +
          facet_grid(Season ~ TP, scales = 'free', space = 'free_x')
      }
    }

    if(input$future == 1){
      if(input$times == 'Annual'){
        grid.arrange(TREND, BOXPLOT, nrow =2)
      }
      if(input$times == 'Season') {
        grid.arrange(TREND, BOXPLOT, layout_matrix = cbind(c(rep(1,2), rep(2,6))))
      }
    } else {
      grid.arrange(TREND)
    }

  }) #end of TS and BP Plots / Tab 1 Plots

  ######### Hover ----- workup

  output$info <- renderText({

     xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("year =", e$x, 0, ", y =", e$y, "\n")
    }

     paste0(
       "hover: ", xy_str(input$plot_hover)
     )

  }) # end of tab 1 hover

  #################################################################################################
  ######## ---------------- - - - - - - -----  Tab 2  ---- - - - - - - - ----------------  ########
  #################################################################################################
  output$WL_DSM_Plots <- renderPlot({

    req(input$years) #input$years doesn't initally have values .. need this
    data <- run$outs

    # Walter-Leith controls ----------------------------------------------------------------------
    dataWL <- formatDataWL(data = data[[1]], future = input$future)

    # Daily Soil Moisture Plot controls ----------------------------------------------------------
    dataDSM <- formatDataDSM(data = data[[2]], RCP = input$RCP)

    # PLOTTING    --------------------------------------------------------------------------------
    # Walter-Leith Plot ----------------------------------
    diagwl2(dataWL[[1]], RCP = input$RCP,
            Year = input$yearButton, YearChoice = input$years2,
            GCM = input$gcmsButton, GCMc = input$gcms2,
            FUTURE50 = input$future, FUTURE90 = input$future,
            data[[1]], dataWL[[3]], dataWL[[2]],
            est='',alt=NA, per='',margen=c(0.1, .5, 0.4, .2))

    grid.echo()
    WL_Plot <- grid.grab()

    # Daily Soil Moisture Plot ----------------------------------
    if(input$future == 1) {

      RibbonDF <-  dataDSM[[2]][dataDSM[[2]]$TP %in% c('Near', 'Late'), ]

      DSM_Plot <-  ggplot() +
            geom_line(data = dataDSM[[2]], aes(Day, median, color=as.factor(TP)),size=1.1)+
            geom_ribbon(data = RibbonDF, aes(x = Day, ymin = min, ymax = max,fill = as.factor(TP)),
                       alpha=0.2) +
            #LEGEND
            scale_color_manual(values=c('black','#b8ae23','#a223b8'),name="",labels=c('Current','Near', 'Long-term'))+
            scale_fill_manual(values=c('#b8ae23','#a223b8'),name="",labels=c('Near', 'Long-term'),guide=FALSE) +
            theme_bw()+
            theme_DSM +
            #AXES
            labs(
              y = 'soil water potential (-MPa)',
              x = 'Month'
            )+
            scale_x_continuous(expand=c(0,0),breaks=c(1,29,60,91,121,152,182,213,244,274,305,335),
                               labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
            coord_cartesian(ylim = c(dataDSM[[3]],0)) +
        #FORMATTING
        uniformTheme

      # EXTRA Lines (Ind. Years and GCMs.)
      if(input$gcmsButton == 2 && input$yearButton == 1){
        yearLineDat <- data[[2]][data[[2]]$Year %in% input$years2, ]
        DSM_Plot <- DSM_Plot + geom_line(data = yearLineDat, aes(Day, value), color = 'black', size = 1.1, linetype = 'dashed')
      }

      if(input$gcmsButton == 1 & input$yearButton == 2) {
        GCMLineDat <- dataDSM[[1]][dataDSM[[1]]$GCM %in% input$gcms2, ]
        GCMLineDat_near <- GCMLineDat[GCMLineDat$TP == 'Near', ]
        head(GCMLineDat_near)
        str(GCMLineDat_near)
        DSM_Plot <- DSM_Plot + geom_line(data = GCMLineDat_near, aes(Day, mean), color = '#b8ae23', size = 1.1, linetype = 'dashed')
        GCMLineDat_late <- GCMLineDat[GCMLineDat$TP == 'Late', ]
        DSM_Plot <- DSM_Plot + geom_line(data = GCMLineDat_late, aes(Day, mean), color = '#a223b8', size = 1.1, linetype = 'dashed')
      }

    } else {

      dataDSM[[2]] <- dataDSM[[2]][dataDSM[[2]]$TP == 'Current',]

      DSM_Plot <-  ggplot() +
        geom_line(data =  dataDSM[[2]],aes(Day, median, color=as.factor(TP)),size=1.1)+
        #LEGEND
        scale_color_manual(values=c('black'),name="",labels=c('Current'))+
        theme_bw()+
        theme_DSM +
        #AXES
        labs(
          y = 'soil water potential (-MPa)',
          x = 'Month'
        )+
        scale_x_continuous(expand=c(0,0),breaks=c(1,29,60,91,121,152,182,213,244,274,305,335),
                           labels = c('J','F','M','A','M','J','J','A','S','O','N','D')) +
        coord_cartesian(ylim = c(dataDSM[[3]],0)) +
        #FORMATTING
        uniformTheme

      if(input$yearButton == 1){
        yearLineDat <- data[[2]][data[[2]]$Year %in% input$years2, ]
        DSM_Plot <- DSM_Plot + geom_line(data = yearLineDat, aes(Day, value), color = 'black', size = 1.1, linetype = 'dashed')
      }

    }

    grid.arrange(WL_Plot, DSM_Plot)

  }) #end of Tab 2 Plots

}
# Run the app ----
shinyApp(ui = c(htmlTemplate("www/header.html"), ui, htmlTemplate("www/footer.html")), server = server)
