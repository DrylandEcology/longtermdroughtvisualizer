Text1 <- textGrob("dry",gp=gpar(fontsize=8,fontfamily = 'Frutiger LT Pro 45 Light'))
Text2 <- textGrob("wet",gp=gpar(fontsize=8,fontfamily = 'Frutiger LT Pro 45 Light'))
DaiLetter <- textGrob("D", gp=gpar(col="black", fontsize=9,fontface='bold',fontfamily='Frutiger LT Pro 45 Light'))

DaiCurgrob = DaiCurPlot + annotation_custom(linesGrob(arrow=(arrow(ends='both',angle=15)),gp=gpar(lwd=1.2)), 
                                            xmin = xval, xmax = xval, ymin = yminvalue, ymax = ymaxvalue)+
  annotation_custom(Text1, xmin = xval-2, xmax = xval+2, ymin = yminvalue-.3, ymax = yminvalue-.3)+
  annotation_custom(Text2, xmin = xval-2, xmax = xval+2, ymin = ymax_text, ymax = ymax_text)+
  annotation_custom(DaiLetter, xmin = xvalLet, xmax = xvalLet, ymin =  .1, ymax = .1)


DaiCurgrob2 <- ggplot_gtable(ggplot_build(DaiCurgrob))
DaiCurgrob2$layout$clip[DaiCurgrob2$layout$name=="panel"] <- "off"

FigureBack <- arrangeGrob(grobs = list(CliDaiPlot,NDVIgrob2, CCPgrob2,DaiCurgrob2),layout_matrix=lay)


lay <-cbind(c(rep(1,18),
              rep(2,8),
              rep(3,4),
              rep(4,18)))
#grid.arrange(CliDaiPlot,NDVIgrob2, CCPgrob2,DaiCurgrob2,layout_matrix=lay)

