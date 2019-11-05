
# ---------------------- Walter Lieth Settings----------------------------------
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

x_DSMWL <- list(
  range = c(.5, 12.5),
  autotic = FALSE,
  dtick = 1,
  showgrid = FALSE,
  ticklen = 5,
  tickwidth = 1,
  tickcolor = toRGB("black"),
  linecolor = "black",
  linewidth = 0.5,
  #showticklabels = FALSE,
  title = ''
)
# -------------------------------------------------------------------------------
# Dail Soil Moisture theme

# -------------------------------------------------------------------------------
# Time Series Seasonal settings
legendbkrd <- adjustcolor(col = '#FFFFFF',alpha.f = 0)
colors2 = c( '#FF7F50', '#99d594','#d53e4f','#3288bd')

# Dail Soil Moisture theme
legendbkrd <- adjustcolor(col = '#FFFFFF',alpha.f = 0)

theme_DSM <- theme(legend.position = c(.855,0.125),
                   legend.background = element_rect(fill = legendbkrd),
                   #legend.margin=unit(-0.6,"cm"),
                   legend.key.height=unit(0, "cm"),
                   legend.text=element_text(size=10,face='plain'))

uniformTheme <- theme(panel.grid.minor = element_blank())
