library(rbenchmark)

benchmark("raster" = {
  nc <- raster::brick(urltotal, varname = 'precipitation')
  lat <- 35.1
  lng <- -112.2 + 360
  vals <- raster::extract(nc, matrix(c(lng, lat), ncol = 2))[1,]
  vals <- vals[5480:length(vals)]
},
"ncdf4" = {
  nc <-   nc <- ncdf4::nc_open(urltotal)
  var = "precipitation"
  lat2 = 176
  lon = 478
  endcount = 34333
  data <- ncdf4::ncvar_get(nc, var, start=c(lon,lat2,1),count=c(1,1,endcount))
  time <- ncdf4::ncvar_get(nc, "time", start=c(1),count=c(endcount))
  time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
  c <- data.frame(time,data)
},
replications = 10,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))
