# Formatting for plots using plotly
format_future_WL_plotly <- function(data_WL_SM_fut_ensemble) {
  ##### Near
  #temp
  dat_temp_near_fut <- data_WL_SM_fut_ensemble[TP == 'Near' & 
                                                 variable == "Average Temperature (C)"]
  dat_temp_near_fut <- rbind(dat_temp_near_fut[1,], dat_temp_near_fut, 
                             dat_temp_near_fut[12,])
  dat_temp_near_fut$Month[c(1,14)] <- c(.5, 12.5)
  #ppt
  dat_ppt_near_fut <- data_WL_SM_fut_ensemble[TP == 'Near' & 
                                                variable == "Precipitation (cm)"]
  dat_ppt_near_fut <- rbind(dat_ppt_near_fut[1,], dat_ppt_near_fut, 
                            dat_ppt_near_fut[12,])
  dat_ppt_near_fut$Month[c(1,14)] <- c(.5, 12.5)
  # SWP
  dat_SWP_near_fut <- data_WL_SM_fut_ensemble[TP == 'Near' & 
                                                variable == "Soil Moisture (SWP, -MPa)"]
  dat_SWP_near_fut <- rbind(dat_SWP_near_fut[1,], dat_SWP_near_fut, 
                            dat_SWP_near_fut[12,])
  dat_SWP_near_fut$Month[c(1,14)] <- c(.5, 12.5)
  
  
  #### Late
  # temp
  dat_temp_late_fut <- data_WL_SM_fut_ensemble[TP == 'Late' & 
                                                 variable == "Average Temperature (C)"]
  dat_temp_late_fut <- rbind(dat_temp_late_fut[1,], dat_temp_late_fut, 
                             dat_temp_late_fut[12,])
  dat_temp_late_fut$Month[c(1,14)] <- c(.5, 12.5)
  # ppt
  dat_ppt_late_fut <- data_WL_SM_fut_ensemble[TP == 'Late' & 
                                                variable == "Precipitation (cm)"]
  dat_ppt_late_fut <- rbind(dat_ppt_late_fut[1,], dat_ppt_late_fut, 
                            dat_ppt_late_fut[12,])
  dat_ppt_late_fut$Month[c(1,14)] <- c(.5, 12.5)
  # swp
  dat_SWP_late_fut <- data_WL_SM_fut_ensemble[TP == 'Late' & 
                                                variable == "Soil Moisture (SWP, -MPa)"]
  dat_SWP_late_fut <- rbind(dat_SWP_late_fut[1,], dat_SWP_late_fut, 
                            dat_SWP_late_fut[12,])
  dat_SWP_late_fut$Month[c(1,14)] <- c(.5, 12.5)
  
  # return
  
  return(list(dat_temp_near_fut, dat_ppt_near_fut, dat_SWP_near_fut,
              dat_temp_late_fut, dat_ppt_late_fut, dat_SWP_late_fut))
}

# ----------------- Walter Lieth Plot Formatting -------------------------------

# left y-axis formatting
y1_WL <- list(
  tickfont = list(color = "#a50f15"),
  side = "left",
  title = "Temperature (°C)",
  range = c(-5, 79),
  autotick = FALSE,
  dtick = 10,
  linecolor = "black",
  linewidth = 0.5,
  mirror = TRUE,
  showgrid = FALSE
)

# right y-axis formatting
y2_WL <- list(
  tickfont = list(color = "#08519c"),
  overlaying = "y",
  side = "right",
  title = "Precipitation (mm)",
  range = c(-10, 159),
  autotick = FALSE,
  dtick = 20,
  showgrid = FALSE
)

# xaxis - soil moisture walter-lieth
x_SM_WL <- list(
  range = c(.5, 12.5),
  autotick = FALSE,
  dtick = 1,
  tickmode="array",
  tickvals = 1:12,
  ticktext = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
               'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
  showgrid = FALSE,
  tickangle = 45,
  ticklen = 5,
  tickwidth = 1,
  tickcolor = plotly::toRGB("black"),
  linecolor = "black",
  linewidth = 0.5,
  #showticklabels = FALSE,
  title = ''
)

# -------------------------------------------------------------------------------
# Time Series Seasonal settings
legendbkrd <- adjustcolor(col = '#FFFFFF',alpha.f = 0)
colors_ts = c( '#FF7F50', '#99d594','#d53e4f','#3288bd')

# Dail Soil Moisture theme
legendbkrd <- adjustcolor(col = '#FFFFFF', alpha.f = 0)

theme_DSM <- ggplot2::theme(legend.position = c(.855,0.125),
                   legend.background = element_rect(fill = legendbkrd),
                   #legend.margin=unit(-0.6,"cm"),
                   legend.key.height=unit(0, "cm"),
                   legend.text=element_text(size=10,face='plain'))

uniformTheme <-  ggplot2::theme(panel.grid.minor = element_blank())

# Boxplot settings -----------------------------------------------------------
x_axis_bp <- list(title = "")

