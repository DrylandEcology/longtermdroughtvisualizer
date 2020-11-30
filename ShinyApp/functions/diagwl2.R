diagwl2 <- function (dat, RCP, 
                     Year = 2, YearChoice = NA,
                     GCM = 2, GCMc = NA,
                     FUTURE50 = 2, FUTURE90 = 2,
                     DataYear, DatEnsemb, DatGCM,
                     est = "", alt = NA, per = "", 
                     margen = c(4, 4, 5, 4), 
                     pcol = "#08519c", tcol = "#a50f15", 
                     pfcol = "#79e6e8", sfcol = "#09a0d1",
                     pcol50 ="#4292C6C4", tcol50= '#ef3b2c',
                     pcol90 ='#9ecae1', tcol90= '#fc9272',
                    shem = FALSE, p3line = FALSE, ...) 
{
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mai = margen, pty = "m", las = 1, new = FALSE)
  nr <- nrow(dat)
  if (nrow(dat) < 4) {
    cat("Debe haber 4 filas de datos mensuales\n")
    break
  }
  
  ################################################################
  ############## Subset necessary data  ####################
  ################################################################
  if(Year == 1) {
    DataYearSub = data.table::setDT(DataYear)[Year == YearChoice, ]
  }
    
  if(FUTURE50 == 1 || FUTURE90 ==1) {
    
    if(RCP == 'RCP45') {
      DatEnsembSub <- data.table::setDT(DatEnsemb)[RCP == 'RCP45', ]
      DatGCMSub <- data.table::setDT(DatGCM)[RCP == 'RCP45', ]
    }
    
    if(RCP == 'RCP85') {
      DatEnsembSub <- data.table::setDT(DatEnsemb)[RCP == 'RCP85', ]
      DatGCMSub <- data.table::setDT(DatGCM)[RCP == 'RCP85', ]
    }
    
    if(GCM == 1) {
      DatGCMSub <- DatGCMSub[GCM == GCMc, ]
    }
  }
  
  if (FUTURE50 == 1) {
    PPT_d50 <- DatEnsembSub[TP =='Near' & variable == 'ppt', sum(mean)]
    PPT_d50_Min <- t(DatEnsembSub[TP =='Near' & variable =='ppt', 'min'])
    PPT_d50_Max <- t(DatEnsembSub[TP =='Near' & variable =='ppt', 'max'])
    TEMP_d50 <- DatEnsembSub[TP =='Near' & variable == 'avg_C', mean(mean)]
    TEMP_d50_Min <- t(DatEnsembSub[TP =='Near' & variable =='avg_C', 'min'])
    TEMP_d50_Max <- t(DatEnsembSub[TP =='Near' & variable =='avg_C', 'max'])
    }
  
  if (FUTURE90 == 1) {
    PPT_d90 <- DatEnsembSub[TP =='Late' & variable == 'ppt', sum(mean)]
    PPT_d90_Min <- t(DatEnsembSub[TP =='Late' & variable =='ppt', 'min'])
    PPT_d90_Max <- t(DatEnsembSub[TP =='Late' & variable =='ppt', 'max'])
    TEMP_d90 <- DatEnsembSub[TP =='Late' & variable == 'avg_C', mean(mean)]
    TEMP_d90_Min <- t(DatEnsembSub[TP =='Late' & variable =='avg_C', 'min'])
    TEMP_d90_Max <- t(DatEnsembSub[TP =='Late' & variable =='avg_C', 'max'])
  }
  
  ################################################################
  
  mlab = c(1:12)
  dat <- as.matrix(dat)
  if (shem) {
    m1 <- dat[, 1:6]
    m2 <- dat[, 7:12]
    dat <- cbind(m2, m1)
    mlab <- c(mlab[7:12], mlab[1:6])
  }
  p <- dat[1, ]
  if (nr == 2) 
    tm <- dat[2, ]
  else tm <- apply(dat[2:3, ], 2, mean)
  pmax <- max(p)
  ymax <- 60
  if (pmax > 300) 
    ymax <- 50 + 10 * floor((pmax + 100)/200)
  ymin <- min(-1.5, min(tm))
  if (ymin < -1.5) {
    ymin = floor(ymin/10) * 10
    labT <- paste(ymin)
    labP <- ""
    if (ymin < -10) {
      for (i in (ymin/10 + 1):-1) {
        labT <- c(labT, i * 10)
        labP <- c(labP, "")
      }
    }
    labT <- c(labT, "0", "10", "20", "30", "40", "50", "")
    labP <- c(labP, "0", "20", "40", "60", "80", "100", "")
  }
  else {
    labT <- c("0", "10", "20", "30", "40", "50", "")
    labP <- c("0", "20", "40", "60", "80", "100", "")
  }
  if (ymax > 60) {
    for (i in 6:(ymax/10 - 1)) {
      labT <- c(labT, "")
      labP <- c(labP, 100 * (2 * i - 7))
    }
  }
  if(ymin == -10){
    yminv = -2.5
  }else{
    yminv=ymin
  }
    plot(0:13 - 0.5, c(tm[12], tm[1:12], tm[1]), xlim = c(0, 
                                                        12), ylim = c(yminv, ymax), type = "n", xaxs = "i", yaxs = "i", 
       xaxp = c(0, 12, 12), xlab = "", ylab = "", xaxt = "n", 
       yaxt = "n", bty = "n")
  lmin <- ymin
  if (lmin == -1.5) 
    lmin = 0
  axis(2, ((lmin/10):(ymax/10)) * 10, labels = labT, col.axis = tcol,mgp=c(3,.4,0),tck=-.01)
  axis(4, ((lmin/10):(ymax/10)) * 10, labels = labP, col.axis = pcol,mgp=c(3,.4,0),tck=-.01)
  #mtext("C", 2, col = tcol, las = 1, line = 3, adj = -7, at = 60)
  #mtext("mm", 4, col = pcol, las = 1, line = 3, adj = 3.5, at = 60)
  mtext("Temperature (C)", side = 2,las=0, line =1.5)
  mtext("Precipitation (mm)", side = 4, las=0, line =  1.5)
  

  abline(0, 0)
  abline(50, 0)
  if (is.na(alt)) 
    mtext(est, line = 2, adj = 0)
  else mtext(paste(est, " (", alt, " m)", sep = ""), line = 2, 
             adj = 0)
  mtext(per, line = 1, adj = 0)
  
  
 ########### TOP TEXT
  mtext(paste('Current MAT: ',round(mean(tm * 10))/10, " C",sep = ""), line = -.5, adj = .05,col=tcol)
  mtext(paste('Current MAP: ',round(sum(p)), " mm", sep = ""), line = -.5, adj = .95,col=pcol)
  if(FUTURE50 == 1){
  mtext(paste('Near Future MAT: ',round(mean(TEMP_d50 * 10))/10, " C", sep = ""), line = -1.5, adj = .05,col=tcol50)
  mtext(paste('Near Future MAP: ',round(sum(PPT_d50)),  " mm", sep = ""), line = -1.5, adj = .95,col=pcol50)
  }
  if(FUTURE90 == 1){
  mtext(paste('Long Future MAT: ',round(mean(TEMP_d90 * 10))/10, " C", sep = ""), line = -2.5, adj = .05,col=tcol90)
  mtext(paste('Long Future MAP: ',round(sum(PPT_d90)),  " mm", sep = ""), line = -2.5, adj = .95,col=pcol90)
  }
  x <- 0:13 - 0.5
  p2 <- c(p[12], p[1:12], p[1])
  if (p3line) {
    yl3 <- c(p[12], p[1:12], p[1])/3
    yl3[yl3 > 50] <- 50
  }
  if (pmax <= 100) {
    xl <- x
    yl <- c(p[12], p[1:12], p[1])/2
    n2 <- 14
  }
  else {
    xp <- numeric(30)
    yp <- numeric(30)
    xl <- numeric(25)
    yl <- numeric(25)
    n <- 0
    n2 <- 0
    gr <- FALSE
    if (p2[1] > 100) {
      n <- n + 1
      xp[n] <- x[1]
      yp[n] <- 50
      n <- n + 1
      xp[n] <- x[1]
      yp[n] <- 50 + (p2[1] - 100)/20
      n2 <- n2 + 1
      xl[n2] <- x[1]
      yl[n2] <- 50
      gr <- TRUE
    }
    else {
      n2 <- n2 + 1
      xl[n2] <- x[1]
      yl[n2] <- p2[1]/2
    }
    for (i in 2:14) {
      if (gr) {
        n <- n + 1
        if (p2[i] > 100) {
          xp[n] <- x[i]
          yp[n] <- 50 + (p2[i] - 100)/20
        }
        else {
          xp[n] <- x[i - 1] + (100 - p2[i - 1])/(p2[i] - 
                                                   p2[i - 1])
          yp[n] <- 50
          n2 <- n2 + 1
          xl[n2] <- xp[n]
          yl[n2] <- 50
          n <- n + 1
          xp[n] <- NA
          yp[n] <- NA
          n2 <- n2 + 1
          xl[n2] <- x[i]
          yl[n2] <- p2[i]/2
          gr <- FALSE
        }
      }
      else {
        if (p2[i] > 100) {
          n <- n + 1
          xp[n] <- x[i - 1] + (100 - p2[i - 1])/(p2[i] - 
                                                   p2[i - 1])
          yp[n] <- 50
          if (xl[n2] != x[i - 1]) {
            n2 <- n2 + 1
            xl[n2] <- x[i - 1]
            yl[n2] <- p2[i - 1]/2
          }
          n2 <- n2 + 1
          xl[n2] <- xp[n]
          yl[n2] <- 50
          n <- n + 1
          xp[n] <- x[i]
          yp[n] <- 50 + (p2[i] - 100)/20
          gr <- TRUE
        }
        else {
          n2 <- n2 + 1
          xl[n2] <- x[i]
          yl[n2] <- p2[i]/2
        }
      }
    }
    if (!is.na(yp[n])) {
      n <- n + 1
      xp[n] <- xp[n - 1]
      yp[n] <- 50
      n2 <- n2 + 1
      xl[n2] <- 12.5
      yl[n2] <- 50
    }
    polygon(xp[1:n], yp[1:n], col = pcol, border = pcol)
  }
  ####### FUTURE CLIM -------------------------------------------------------------------------------------
  
  # RCPs -------------------------------------------------------------------------------------
  
  if(FUTURE90 == 1 && FUTURE50 == 2){
    polygon(c(xl,rev(xl)),c(PPT_d90_Max[c(12,1:12,1)]/2,rev(PPT_d90_Min[c(12,1:12,1)]/2)),col=pcol90,border=NA)
    polygon(c(xl,rev(xl)),c(TEMP_d90_Max[c(12,1:12,1)],rev(TEMP_d90_Min[c(12,1:12,1)])),col=tcol90,border=NA)
  }
  if(FUTURE50 == 1 && FUTURE90 == 1){
    polygon(c(xl,rev(xl)),c(PPT_d90_Max[c(12,1:12,1)]/2,rev(PPT_d90_Min[c(12,1:12,1)]/2)),col=pcol90,border=NA)
    polygon(c(xl,rev(xl)),c(PPT_d50_Max[c(12,1:12,1)]/2,rev(PPT_d50_Min[c(12,1:12,1)]/2)),col=pcol50,border=NA)
    polygon(c(xl,rev(xl)),c(TEMP_d50_Max[c(12,1:12,1)],rev(TEMP_d50_Min[c(12,1:12,1)])),col=tcol50,border=NA)
    polygon(c(xl,rev(xl)),c(TEMP_d90_Max[c(12,1:12,1)],rev(TEMP_d90_Min[c(12,1:12,1)])),col=tcol90,border=NA)
  }
  
  pi <- approx(xl[1:n2], yl[1:n2], n = 66)$y
  ti <- approx(x, c(tm[12], tm[1:12], tm[1]), n = 66)$y
  ti[ti < 0] <- 0
  d <- pi - ti
  xi <- (1:66)/5 - 0.7
  xw <- subset(xi, d > 0)
  y1 <- subset(pi, d > 0)
  y2 <- subset(ti, d > 0)
  if (length(xw) > 0) 
   # segments(xw, y1, xw, y2, col = pcol, lty = 3, lwd =2)
  xw <- subset(xi, d < 0)
  y1 <- subset(pi, d < 0)
  y2 <- subset(ti, d < 0)
  if (length(xw) > 0) 
    #segments(xw, y1, xw, y2, col = tcol, lty = 3, lwd = 2)
  #for (i in 1:12) if (dat[3, i] <= 0) 
  #  rect(i - 1, -1.5, i, 0, col = sfcol)
  #for (i in 1:12) if (dat[4, i] <= 0 && dat[3, i] > 0) 
  #  rect(i - 1, -1.5, i, 0, col = pfcol)
  ## Current PPT and Temp -----------------------------------------------------------------  
  lines(xl[1:n2], yl[1:n2], col = pcol, lwd = 2.5)
  if (p3line) 
    lines(x, yl3)
  lines(x, c(tm[12], tm[1:12], tm[1]), col = tcol, lwd = 2.5)
  
  ### GCM PPT and Temp ---------------------------------------------------------------------
  if(FUTURE50 == 1 && FUTURE90 == 1 && GCM == 1){
    
    # NEAR
    yl_near <- t(DatGCMSub[TP == 'Near' & variable == 'ppt', 'mean'])/2
    tm_near <- t(DatGCMSub[TP == 'Near' & variable == 'avg_C', 'mean'])   
    lines(xl[1:n2], c(yl_near[12], yl_near[1:12], yl_near[1]), col = "#30769c", lwd = 2.5, lty = 2) # ppt
    lines(x, c(tm_near[12], tm_near[1:12], tm_near[1]), col = "#da2110", lwd = 2.5, lty = 2) # temp
    
    # LATE
    yl_late <- t(DatGCMSub[TP == 'Late' & variable == 'ppt', 'mean'])/2
    tm_late <- t(DatGCMSub[TP == 'Late' & variable == 'avg_C', 'mean'])   
    lines(xl[1:n2], c(yl_late[12], yl_late[1:12], yl_late[1]), col ="#7eb8d7", lwd = 2.5, lty = 2) # ppt
    lines(x, c(tm_late[12], tm_late[1:12], tm_late[1]), col = "#fb744b", lwd = 2.5, lty = 2) # temp
  
  }
  
  ### Individual Year PPT and Temp ---------------------------------------------------------------------
  if(Year == 1){
    
    yl_year <- t(DataYearSub[variable == 'ppt', 'value'])/2
    tm_year <- t(DataYearSub[variable == 'avg_C', 'value'])   
    lines(xl[1:n2], c(yl_year[12], yl_year[1:12], yl_year[1]), col = pcol, lwd = 2.5, lty = 2) # ppt
    lines(x, c(tm_year[12], tm_year[1:12], tm_year[1]), col = tcol, lwd = 2.5, lty = 2) # temp
    
  }
  
  #mtext(formatC(max(as.matrix(dat[2, ])), digits = 1, format = "f"), 
  #     2, las = 1, line = 2, at = 35)
  #mtext(formatC(min(as.matrix(dat[3, ])), digits = 1, format = "f"), 
  #      2, las = 1, line = 2, at = 15)
  
  for (i in 0:13) segments(i, 0, i, -1)
  #mtext(mlab, 1, las = 1, line = 0.5, adj = 0.5, at = x[2:13])
  invisible()

}