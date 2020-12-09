### load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(ggpubr)
Sys.setenv(TZ='UTC')

#downlaod data 
#########################

download_scada <- function(yearmonth){
  external.data.location <- "D:/Data/RAW/AEMO/NEMWEB/UNIT_SCADA" 
  year <- substr(yearmonth, 1, 4)
  month <- substr(yearmonth, 5, 6)
  url <- 0 #initialise
  #check if already downloaded
  csv.name <- paste0(external.data.location,"/PUBLIC_DVD_DISPATCH_UNIT_SCADA_", yearmonth, 
                     "010000.CSV")
  if(!file.exists(csv.name)){
    url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                  year, "_",month, 
                  "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCH_UNIT_SCADA_",
                  yearmonth, "010000.zip")
    temp <- tempfile()
    download.file(url, temp, mode="wb", method = "curl") #download zip
    unzip(temp, paste0("PUBLIC_DVD_DISPATCH_UNIT_SCADA_", yearmonth, "010000.CSV"), 
          exdir = external.data.location) #unzip file and save csv to external storage
  }
  if(url != 0){
    unlink(temp) #delete zip
  }    
}

download_rrp <- function(yearmonth){
  external.data.location <- "D:/Data/RAW/AEMO/NEMWEB/DISPATCHPRICE" 
  year <- substr(yearmonth, 1, 4)
  month <- substr(yearmonth, 5, 6)
  url <- 0 #initialise
  #check if already downloaded
  csv.name <- paste0(external.data.location,"/PUBLIC_DVD_DISPATCHPRICE_", yearmonth, 
                     "010000.CSV")
  if(!file.exists(csv.name)){
    url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                  year, "_",month, 
                  "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHPRICE_",
                  yearmonth, "010000.zip")
    temp <- tempfile()
    download.file(url, temp, mode="wb", method = "curl") #download zip
    unzip(temp, paste0("PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV"), 
          exdir = external.data.location) #unzip file and save csv to external storage
  }
  if(url != 0){
    unlink(temp) #delete zip
  }    
}

nem_year <- function(datetime){
  ifelse(yday(datetime) == 1 & as.ITime(datetime) == as.ITime("00:00:00"),
         year(datetime)-1,
         year(datetime))
}
