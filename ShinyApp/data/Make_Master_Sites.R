library(RSQLite)
library(DBI)
drv <- dbDriver('SQLite')
WeatherDB <- dbConnect(drv, '/Volumes/Samsung_T5/CDI/AZ_dbWeatherData.sqlite3')
#WeatherDB <- dbConnect(drv, '~/Desktop/Projects/4FRI/SOILWAT/CreateWeatherDB/1_Data_SWInput/dbWeatherData_Future.sqlite3')
dbListTables(WeatherDB)
Scenarios <- dbReadTable(WeatherDB, "Scenarios")
head(Scenarios)
Sites <- dbReadTable(WeatherDB, "Sites")
head(Sites)


str(Sites)
write.csv(Sites, row.names = FALSE, 'data/AZ_weatherDB_cells.csv')





break

# Some exploration
extract <- function(e){
  data.frame(DOY=e@data[,1],MAX_C = e@data[,2],MIN_C = e@data[,3],PPT = e@data[,4])
} 

dbListFields(WeatherDB, "WeatherData")
Data <- dbGetQuery(WeatherDB, "SELECT wdid, Site_id, Scenario, StartYear, EndYear FROM WeatherData")

SiteX <- Sites[Sites$Label == 'Cell3386_Livneh2013_-111.65625_34.53125_raw.d45yrs.RCP45.CanESM2', ]
SiteX

DataX <- Data[Data$Site_id == SiteX$Site_id,]
DataX

DataX2 <- dbGetQuery(WeatherDB, "SELECT * FROM WeatherData WHERE Site_id = 17952")
data <- unserialize(memDecompress(DataX2$data[[1]], type = 'gzip'))
data <- plyr::ldply(data,extract)

