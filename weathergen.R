#-----------------------------------------------------#
# Script to format meterological data for use in SWAT
#-----------------------------------------------------#
# Becky Trippier 04/04/2019

## Script to convert meterological station data in an excel workbook into units used by SWAT model and daily text files per variable. Each station should have a separate tab per units and a beginning tab called 'STATIONS' with station location data. 
## acceptable units for variables:
## temperature - fahrenheit or celsius
## precipitation - mm
## windspeed - mph, mps, ftps, kmph
## solar - Wm2, MH m2 d-1
## humidity - % or fraction
## (missing values should be noted as na)

# location of stations excel file
path <- "filepath/stations.xlsx"

#add simulated years of daily data - adds -99 to all stations in order to simulate years at the beginning of the period. used for creating warm up periods where data years is low
simyears <- 0

#run function
swatweather(path,simyears)

# ------------------- Function ----------------------- #
#load dependencies
library(weathermetrics)
library(stringr)
library(openxlsx)
library(tidyr)
library(dplyr)
library(tibble)
library(janitor)
library(stringi)

swatweather <- function(path, simyears = 0) {
  wb <- openxlsx::loadWorkbook(path)
  sheets <- openxlsx::sheets(wb)
  stations <- openxlsx::readWorkbook(wb, sheet = "STATIONS")
  
  #get stations with data
  stationdata <-
    stations$STATION[which(stations$STATION %in% sheets)]
  print(paste("Station data found for:", paste(stationdata, collapse = ",")))
  
  #make out folder
  split <- stringr::str_split(stringi::stri_reverse(path), "/", n = 2)
  newpath <-
    paste(stringi::stri_reverse(split[[1]][2]), "/Out", sep = "")
  dir.create(newpath)
  
  #produce variable txt files
  stfiles <- list()
  strtdate <- list()
  
  for (i in 1:length(stationdata)) {
    stationname <- stationdata[i]
    mysheet <-
      openxlsx::readWorkbook(wb, sheet = as.character(stationname))
    
    #add names to list
    listed <- setNames(list(names(mysheet)), stationname)
    stfiles <- append(stfiles, listed)
    print(paste(stationname, ":", paste(names(mysheet), collapse = ",")))
    
    #get start date
    startdate <-
      janitor::excel_numeric_to_date(as.numeric(as.character(mysheet$date[2])), date_system = "modern")
    date <- format(startdate, format = "%Y%m%d")
    
    if (simyears == 0) {
      datlist <- setNames(list(date), stationname)
      strtdate <- append(strtdate, datlist)
    } else {
      newdate <- as.POSIXlt(date, format = "%Y%m%d")
      newdate$year <- newdate$year - simyears
      simyear <- format(as.Date(newdate), format = "%Y%m%d")
      datlist <- setNames(list(simyear), stationname)
      strtdate <- append(strtdate, datlist)
    }
    
    # temp in celsius
    if ("tempmin" %in% names(mysheet) &
        "tempmax" %in% names(mysheet)) {
      if (mysheet$tempmin[1] == "fahrenheit") {
        mysheet$tempmin[2:length(mysheet$tempmin)] <-
          convert_temperature(as.numeric(mysheet$tempmin[2:length(mysheet$tempmin)]), "f", "c")
      } else if (mysheet$tempmin[1] == "fahrenheit") {
        mysheet$tempmax[2:length(mysheet$tempmax)] <-
          convert_temperature(as.numeric(mysheet$tempmax[2:length(mysheet$tempmax)]), "f", "c")
      }
      
      mysheet$newmin[2:length(mysheet$tempmin)] <-
        mysheet$tempmin[2:length(mysheet$tempmin)] %>%
        as.numeric() %>%
        sprintf(fmt = '%#.2f')
      mysheet$newmin[mysheet$newmin == "NA"] <- "-99.0"
      
      
      mysheet$newmax[2:length(mysheet$tempmax)] <-
        mysheet$tempmax[2:length(mysheet$tempmax)] %>%
        as.numeric() %>%
        sprintf(fmt = '%#.2f')
      mysheet$newmax[mysheet$newmax == "NA"] <- "-99.0"
      
      mysheet$outtemp <-
        paste(mysheet$newmax, ",", mysheet$newmin, sep = "")
      
      tempout <- mysheet %>%
        dplyr::select(outtemp) %>%
        dplyr::slice(2:nrow(mysheet)) %>%
        dplyr::rename(!!date := outtemp)
      
      if (simyears == 0) {
        write.csv(
          tempout,
          file = paste(newpath, "/", stationname, "tmp.txt", sep = ""),
          row.names = F,
          quote = F
        )
      } else{
        simdf <-
          setNames(data.frame(rep("-99.0,-99.0", (
            365 * simyears
          ) + 1)), simyear)
        simout <- rbind(simdf, setNames(tempout, simyear))
        
        ##year check
        #simout$date <- seq(as.Date(simyear, format="%Y%m%d"),by="day",length.out = nrow(simout))
        write.csv(
          simout,
          file = paste(newpath, "/", stationname, "tmp.txt", sep = ""),
          row.names = F,
          quote = F
        )
      }
    }
    
    #precip need mm
    if ("precip" %in% names(mysheet)) {
      if (mysheet$precip[1] == "cm") {
        mysheet$precip[2:length(mysheet$precip)] <-
          convert_precip(as.numeric(mysheet$precip[2:length(mysheet$precip)]), "cm", "mm")
      } else if (mysheet$precip[1] == "inches") {
        mysheet$precip[2:length(mysheet$precip)] <-
          convert_precip(as.numeric(mysheet$precip[2:length(mysheet$precip)]), "inches", "mm")
      }
      
      mysheet$newprecip[2:length(mysheet$precip)] <-
        mysheet$precip[2:length(mysheet$precip)] %>%
        as.numeric() %>%
        sprintf(fmt = '%#.2f')
      mysheet$newprecip[mysheet$newprecip == "NA"] <- "-99.0"
      
      precout <- mysheet %>%
        dplyr::select(newprecip) %>%
        dplyr::slice(2:nrow(mysheet)) %>%
        dplyr::rename(!!date := newprecip)
      
      if (simyears == 0) {
        write.csv(
          precout,
          file = paste(newpath, "/", stationname, "pcp.txt", sep = ""),
          row.names = F,
          quote = F
        )
      } else {
        simdf <- setNames(data.frame(rep("-99.0", (
          365 * simyears
        ) + 1)), simyear)
        simout <- rbind(simdf, setNames(precout, simyear))
        write.csv(
          simout,
          file = paste(newpath, "/", stationname, "pcp.txt", sep = ""),
          row.names = F,
          quote = F
        )
      }
    }
    
    #solar in MJ/m2
    if ("solar" %in% names(mysheet)) {
      if (mysheet$solar[1] == "W/m2") {
        # 1 W/m-2 = 0.0864 MJ/m-2/day-1 http://www.fao.org/docrep/X0490E/x0490e0i.htm
        mysheet$solar[2:length(mysheet$solar)] <-
          as.numeric(mysheet$solar[2:length(mysheet$solar)]) / 11.57407
      }
      
      mysheet$newsolar[2:length(mysheet$solar)] <-
        mysheet$solar[2:length(mysheet$solar)] %>%
        as.numeric() %>%
        sprintf(fmt = '%#.2f')
      mysheet$newsolar[mysheet$newsolar == "NA"] <- "-99.0"
      
      solarout <- mysheet %>%
        dplyr::select(newsolar) %>%
        dplyr::slice(2:nrow(mysheet)) %>%
        dplyr::rename(!!date := newsolar)
      
      if (simyears == 0) {
        write.csv(
          solarout,
          file = paste(newpath, "/", stationname, "sol.txt", sep = ""),
          row.names = F,
          quote = F
        )
      } else {
        simdf <- setNames(data.frame(rep("-99.0", (
          365 * simyears
        ) + 1)), simyear)
        simout <- rbind(simdf, setNames(solarout, simyear))
        write.csv(
          simout,
          file = paste(newpath, "/", stationname, "sol.txt", sep = ""),
          row.names = F,
          quote = F
        )
      }
    }
    
    #humidity as a fraction
    if ("humidity" %in% names(mysheet)) {
      if (mysheet$humidity[1] == "%") {
        mysheet$humidity[2:length(mysheet$humidity)] <-
          (as.numeric(mysheet$humidity[2:length(mysheet$humidity)]) / 100)
      }
      
      mysheet$newhumidity[2:length(mysheet$humidity)] <-
        mysheet$humidity[2:length(mysheet$humidity)] %>%
        as.numeric() %>%
        sprintf(fmt = '%#.2f')
      mysheet$newhumidity[mysheet$newhumidity == "NA"] <- "-99.0"
      
      humidout <- mysheet %>%
        dplyr::select(newhumidity) %>%
        dplyr::slice(2:nrow(mysheet)) %>%
        dplyr::rename(!!date := newhumidity)
      
      if (simyears == 0) {
        write.csv(
          humidout,
          file = paste(newpath, "/", stationname, "rh.txt", sep = ""),
          row.names = F,
          quote = F
        )
      } else {
        simdf <- setNames(data.frame(rep("-99.0", (
          365 * simyears
        ) + 1)), simyear)
        simout <- rbind(simdf, setNames(humidout, simyear))
        write.csv(
          simout,
          file = paste(newpath, "/", stationname, "rh.txt", sep = ""),
          row.names = F,
          quote = F
        )
      }
    }
    
    #windspeed as m/s
    if ("windspeed" %in% names(mysheet)) {
      if (mysheet$windspeed[1] == "kmph") {
        mysheet$windspeed[2:length(mysheet$windspeed)] <-
          convert_wind_speed(as.numeric(mysheet$windspeed[2:length(mysheet$windspeed)]), "kmph", "mps")
      } else if (mysheet$windspeed[1] == "mph") {
        mysheet$windspeed[2:length(mysheet$windspeed)] <-
          convert_wind_speed(as.numeric(mysheet$windspeed[2:length(mysheet$windspeed)]), "mph", "mps")
      } else if (mysheet$windspeed[1] == "ftps") {
        mysheet$windspeed[2:length(mysheet$windspeed)] <-
          convert_wind_speed(as.numeric(mysheet$precip[2:length(mysheet$precip)]), "ftps", "mps")
      }
      
      mysheet$newwind[2:length(mysheet$windspeed)] <-
        mysheet$windspeed[2:length(mysheet$windspeed)] %>%
        as.numeric() %>%
        sprintf(fmt = '%#.2f')
      mysheet$newwind[mysheet$newwind == "NA"] <- "-99.0"
      
      windout <- mysheet %>%
        dplyr::select(newwind) %>%
        dplyr::slice(2:nrow(mysheet)) %>%
        dplyr::rename(!!date := newwind)
      
      if (simyears == 0) {
        write.csv(
          windout,
          file = paste(newpath, "/", stationname, "wind.txt", sep = ""),
          row.names = F,
          quote = F
        )
      } else {
        simdf <- setNames(data.frame(rep("-99.0", (
          365 * simyears
        ) + 1)), simyear)
        simout <- rbind(simdf, setNames(windout, simyear))
        write.csv(
          simout,
          file = paste(newpath, "/", stationname, "wind.txt", sep = ""),
          row.names = F,
          quote = F
        )
      }
    }
    
  }
  
  
  #temp stations txt
  temps <- names(stfiles[which(grepl("tempmin", stfiles))])
  stations %>%
    filter(STATION %in% temps) %>%
    mutate(NAME = paste(STATION, "tmp", sep = "")) %>%
    select(ID, NAME, LONG, LAT, ELEVATION, -STATION) %>%
    write.csv(
      file = paste(newpath, "/tmpStations.txt", sep = ""),
      row.names = F,
      quote = F
    )
  
  #precipitation stations txt
  precips <- names(stfiles[which(grepl("precip", stfiles))])
  stations %>%
    filter(STATION %in% precips) %>%
    mutate(NAME = paste(STATION, "pcp", sep = "")) %>%
    select(ID, NAME, LONG, LAT, ELEVATION, -STATION) %>%
    write.csv(
      file = paste(newpath, "/pcpStations.txt", sep = ""),
      row.names = F,
      quote = F
    )
  
  #solar stations txt
  solars <- names(stfiles[which(grepl("solar", stfiles))])
  stations %>%
    filter(STATION %in% solars) %>%
    mutate(NAME = paste(STATION, "sol", sep = "")) %>%
    select(ID, NAME, LONG, LAT, ELEVATION, -STATION) %>%
    write.csv(
      file = paste(newpath, "/solStations.txt", sep = ""),
      row.names = F,
      quote = F
    )
  
  #humidity stations txt
  humids <- names(stfiles[which(grepl("humidity", stfiles))])
  stations %>%
    filter(STATION %in% humids) %>%
    mutate(NAME = paste(STATION, "rh", sep = "")) %>%
    select(ID, NAME, LONG, LAT, ELEVATION, -STATION) %>%
    write.csv(
      file = paste(newpath, "/rhStations.txt", sep = ""),
      row.names = F,
      quote = F
    )
  
  #windspeed stations txt
  windspeeds <- names(stfiles[which(grepl("windspeed", stfiles))])
  stations %>%
    filter(STATION %in% windspeeds) %>%
    mutate(NAME = paste(STATION, "wind", sep = "")) %>%
    select(ID, NAME, LONG, LAT, ELEVATION, -STATION) %>%
    write.csv(
      file = paste(newpath, "/windStations.txt", sep = ""),
      row.names = F,
      quote = F
    )
  
}
