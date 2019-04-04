#----------------------------------------------#
#script to create SWAT weather generator table
#----------------------------------------------#
# Becky Trippier 04/04/2019

# Script to prepare daily weather station data for variables air temperature, precipitation, solar radiation, relative humidity and wind speed into a SWAT weather generator table which can be inserted into the reference database for use in SWAT modelled.This follows the style of the WGEN Excel macro available from the SWAT website: https://swat.tamu.edu/software/ 
# Inputs: requires a folder of daily data per station per variable and overall station csv file with station locations and references text file names in the folder, in the same style as the SampleFileList.xls in the Excel macro
# Outputs: weather generator table as csv

# path to directory of daily station data
# station files should be formatted with the first row containing the startdate as 'mmddyyyy' then separate rows for each daily record
path <-("filepath/")

# stations file name in the path folder
stations <- "stations.csv"

#run function
swat_wgen(path,stations)

# ------------------- Function ----------------------- #
#load required packages
library(readr)
library(dplyr)
library(lubridate)
library(data.table)
library(e1071)

swat_wgen <- function(path, stations) {
  stationed <- readr::read_csv(paste(path, stations, sep = ""))
  stationed <-
    stationed %>% dplyr::select(-`Max. 1/2 Hour Rainfall File`,-`Output File Title`) %>%
    na.omit()
  
  wgen <- NULL
  
  #precipitation
  for (i in 1:nrow(stationed)) {
    st.info <-
      stationed[i, ] %>% dplyr::select(
        STATION = "Station Name",
        WLONGITUDE = "Longitude",
        WLATITUDE = "Latitude",
        WELEV = "Elevation"
      )
    
    
    ##----------temperature-------------##
    tfile <- stationed$`Temperature File`[i]
    temp <- readr::read_delim(paste(path, tfile, sep = ""), delim = ",")
    startdate <- as.Date(as.character(names(temp)), format = "%Y%m%d")
    print(paste("start date:", startdate))
    
    
    tempdf <-
      temp %>% tidyr::separate(
        col = names(temp),
        into = c("maxtemp", "mintemp"),
        sep = ","
      )
    #dates
    dates <-
      dplyr::data_frame(Date = seq(startdate, by = "day", length.out = nrow(tempdf))) %>%
      dplyr::mutate(
        date = Date %>%
          as.character %>%
          ymd,
        year = year(date),
        month = month(date),
        day = day(date)
      )
    longtemp <-
      cbind(
        dates[, c("year", "month", "day")],
        dailymintmp = as.numeric(tempdf$mintemp),
        dailymaxtmp = as.numeric(tempdf$maxtemp)
      )
    
    #TMPMX - mean daily max air temp for month
    monthlymaxtmp <- longtemp  %>% dplyr::group_by(month) %>%
      dplyr::summarise(mxtmp = mean(dailymaxtmp)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "TMPMX1",
          "TMPMX2",
          "TMPMX3",
          "TMPMX4",
          "TMPMX5",
          "TMPMX6",
          "TMPMX7",
          "TMPMX8",
          "TMPMX9",
          "TMPMX10",
          "TMPMX11",
          "TMPMX12"
        )
      )
    st.info <- cbind(st.info, monthlymaxtmp[2, ])
    
    #TMPMN - mean daily min air temp for month
    monthlymintmp <- longtemp  %>% dplyr::group_by(month) %>%
      dplyr::summarise(mntmp = mean(dailymintmp)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "TMPMN1",
          "TMPMN2",
          "TMPMN3",
          "TMPMN4",
          "TMPMN5",
          "TMPMN6",
          "TMPMN7",
          "TMPMN8",
          "TMPMN9",
          "TMPMN10",
          "TMPMN11",
          "TMPMN12"
        )
      )
    st.info <- cbind(st.info, monthlymintmp[2, ])
    
    #STDMX - standard deviation for daily maximum air temperature in month
    stdmax <- longtemp  %>% dplyr::group_by(month) %>%
      dplyr::summarise(sdmax = sd(dailymaxtmp)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "TMPSTDMX1",
          "TMPSTDMX2",
          "TMPSTDMX3",
          "TMPSTDMX4",
          "TMPSTDMX5",
          "TMPSTDMX6",
          "TMPSTDMX7",
          "TMPSTDMX8",
          "TMPSTDMX9",
          "TMPSTDMX10",
          "TMPSTDMX11",
          "TMPSTDMX12"
        )
      )
    st.info <- cbind(st.info, stdmax[2, ])
    
    #STDMN - standard deviation for daily minimum air temperature in month
    stdmin <- longtemp  %>% dplyr::group_by(month) %>%
      dplyr::summarise(sdmin = sd(dailymintmp)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "TMPSTDMN1",
          "TMPSTDMN2",
          "TMPSTDMN3",
          "TMPSTDMN4",
          "TMPSTDMN5",
          "TMPSTDMN6",
          "TMPSTDMN7",
          "TMPSTDMN8",
          "TMPSTDMN9",
          "TMPSTDMN10",
          "TMPSTDMN11",
          "TMPSTDMN12"
        )
      )
    st.info <- cbind(st.info, stdmin[2, ])
    
    
    ##----------precipitation-------------##
    pfile <- stationed$`Rainfall File`[i]
    prec <- readr::read_delim(paste(path, pfile, sep = ""), delim = ",")
    startdate <- as.Date(as.character(names(prec)), format = "%Y%m%d")
    names(prec) <- "dailyprecip"
    #dates
    dates <-
      dplyr::data_frame(Date = seq(startdate, by = "day", length.out = nrow(prec))) %>%
      dplyr::mutate(
        date = Date %>%
          as.character %>%
          ymd,
        year = year(date),
        month = month(date),
        day = day(date)
      )
    
    st.info$RAIN_YRS <- as.numeric(length(unique(dates$year)))
    longprecip <-
      cbind(dates[, c("year", "month", "day")], dailyprecip = as.numeric(prec$dailyprecip))
    
    #PCPMM - mean total monthly precipitation
    monthlyprec <- longprecip  %>% dplyr::group_by(month, year) %>%
      dplyr::summarise(monthlyprec = sum(dailyprecip))
    meanmonthly <- monthlyprec %>% dplyr::group_by(month) %>%
      dplyr::summarise(mean = mean(monthlyprec)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "PCPMM1",
          "PCPMM2",
          "PCPMM3",
          "PCPMM4",
          "PCPMM5",
          "PCPMM6",
          "PCPMM7",
          "PCPMM8",
          "PCPMM9",
          "PCPMM10",
          "PCPMM11",
          "PCPMM12"
        )
      )
    st.info <- cbind(st.info, meanmonthly[2, ])
    
    #PCPSTD - standard deviation for daily precipitation in month
    stdprec <- longprecip  %>% dplyr::group_by(month) %>%
      dplyr::summarise(sdprec = sd(dailyprecip)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "PCPSTD1",
          "PCPSTD2",
          "PCPSTD3",
          "PCPSTD4",
          "PCPSTD5",
          "PCPSTD6",
          "PCPSTD7",
          "PCPSTD8",
          "PCPSTD9",
          "PCPSTD10",
          "PCPSTD11",
          "PCPSTD12"
        )
      )
    st.info <- cbind(st.info, stdprec[2, ])
    
    #PCPSKW - skew coefficient for daily precipitation in month
    #e1071::skewness function type 2 most similar to excel skew formula methodology
    skwprec <- longprecip  %>% dplyr::group_by(month) %>%
      dplyr::summarise(skwprec = e1071::skewness(dailyprecip, type = 2)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "PCPSKW1",
          "PCPSKW2",
          "PCPSKW3",
          "PCPSKW4",
          "PCPSKW5",
          "PCPSKW6",
          "PCPSKW7",
          "PCPSKW8",
          "PCPSKW9",
          "PCPSKW10",
          "PCPSKW11",
          "PCPSKW12"
        )
      )
    st.info <- cbind(st.info, skwprec[2, ])
    
    #get wet day stats
    precipwet <- longprecip %>%
      dplyr::mutate(wetday = if_else(dailyprecip > 0, "wet", "dry")) %>%
      dplyr::mutate(wet = if_else(wetday == "wet", 1, 0)) %>%
      dplyr::mutate(dry = if_else(wetday == "dry", 1, 0))
    precipwet$PR_W1 <- 0
    precipwet$PR_W2 <- 0
    for (j in 2:nrow(precipwet)) {
      day <- precipwet$wetday[j]
      daybefore <- precipwet$wetday[j - 1]
      if (day == "wet" & daybefore == "dry") {
        precipwet$PR_W1[j] <- 1
      } else if (day == "wet" & daybefore == "wet") {
        precipwet$PR_W2[j] <- 1
      }
    }
    
    #PR_W1 - probability of a wet day following a dry day in a month
    #no.wet-dry days
    wetdry.days <- precipwet  %>% dplyr::group_by(month) %>%
      dplyr::summarise(wetdry = sum(PR_W1))
    #day - wet
    dry.days <- precipwet  %>% dplyr::group_by(month) %>%
      dplyr::summarise(totaldry = sum(dry))
    PR_W1 <- merge(wetdry.days, dry.days, by = "month")
    PR_W1_out <- PR_W1 %>% dplyr::mutate(prob = (wetdry / totaldry)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "PR_W1_1",
          "PR_W1_2",
          "PR_W1_3",
          "PR_W1_4",
          "PR_W1_5",
          "PR_W1_6",
          "PR_W1_7",
          "PR_W1_8",
          "PR_W1_9",
          "PR_W1_10",
          "PR_W1_11",
          "PR_W1_12"
        )
      )
    st.info <- cbind(st.info, PR_W1_out[4, ])
    
    #PR_W2 - probability of a wet day following a wet day in a month
    #no wet-wet days
    wetwet.prec <- precipwet  %>% dplyr::group_by(month) %>%
      dplyr::summarise(wetwet = sum(PR_W2))
    wet.days <- precipwet  %>% dplyr::group_by(month) %>%
      dplyr::summarise(totalwet = sum(wet))
    PR_W2 <- merge(wetwet.prec, wet.days, by = "month")
    PR_W2_out <- PR_W2 %>% dplyr::mutate(prob = (wetwet / totalwet)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "PR_W2_1",
          "PR_W2_2",
          "PR_W2_3",
          "PR_W2_4",
          "PR_W2_5",
          "PR_W2_6",
          "PR_W2_7",
          "PR_W2_8",
          "PR_W2_9",
          "PR_W2_10",
          "PR_W2_11",
          "PR_W2_12"
        )
      )
    st.info <- cbind(st.info, PR_W2_out[4, ])
    
    #PCPD - average number of days of precipitation in month
    mean.wet <- precipwet  %>% dplyr::group_by(month, year) %>%
      dplyr::summarise(meanwet = sum(wet)) %>%
      group_by(month) %>% summarise(monthlywet = mean(meanwet))
    pcpd <- mean.wet %>% data.table::transpose() %>%
      setNames(
        c(
          "PCPD1",
          "PCPD2",
          "PCPD3",
          "PCPD4",
          "PCPD5",
          "PCPD6",
          "PCPD7",
          "PCPD8",
          "PCPD9",
          "PCPD10",
          "PCPD11",
          "PCPD12"
        )
      )
    st.info <- cbind(st.info, pcpd[2, ])
    
    #RAINHHMX - max half hour rainfall for the month
    hhrprecip <-
      longprecip %>% dplyr::mutate(halfhour = dailyprecip / 3)
    maxhhr.prec <- hhrprecip  %>% dplyr::group_by(month, year) %>%
      dplyr::summarise(maxhhr = max(halfhour)) %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(meanmaxhhr = mean(maxhhr)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "RAINHHMX1",
          "RAINHHMX2",
          "RAINHHMX3",
          "RAINHHMX4",
          "RAINHHMX5",
          "RAINHHMX6",
          "RAINHHMX7",
          "RAINHHMX8",
          "RAINHHMX9",
          "RAINHHMX10",
          "RAINHHMX11",
          "RAINHHMX12"
        )
      )
    st.info <- cbind(st.info, maxhhr.prec[2, ])
    
    
    ##----------solar-------------##
    sfile <- stationed$`Solar Radiation File`[i]
    solar <- readr::read_delim(paste(path, sfile, sep = ""), delim = ",")
    
    startdate <- as.Date(as.character(names(solar)), format = "%Y%m%d")
    names(solar) <- "dailysolar"
    dates <-
      dplyr::data_frame(Date = seq(startdate, by = "day", length.out = nrow(solar))) %>%
      dplyr::mutate(
        date = Date %>%
          as.character %>%
          ymd,
        year = year(date),
        month = month(date),
        day = day(date)
      )
    longsolar <-
      cbind(dates[, c("year", "month", "day")], dailysolar = as.numeric(solar$dailysolar))
    
    #SOLARAV - average daily solar radiation for the month
    
    monthlysolar <- longsolar  %>% dplyr::group_by(month) %>%
      dplyr::summarise(monthlysolar = mean(dailysolar)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "SOLARAV1",
          "SOLARAV2",
          "SOLARAV3",
          "SOLARAV4",
          "SOLARAV5",
          "SOLARAV6",
          "SOLARAV7",
          "SOLARAV8",
          "SOLARAV9",
          "SOLARAV10",
          "SOLARAV11",
          "SOLARAV12"
        )
      )
    st.info <- cbind(st.info, monthlysolar[2, ])
    
    ##----------relative humidity-------------##
    rhfile <- stationed$`Dew Point File`[i]
    humidity <- readr::read_delim(paste(path, rhfile, sep = ""), delim = ",")
    
    startdate <-
      as.Date(as.character(names(humidity)), format = "%Y%m%d")
    names(humidity) <- "dailyrh"
    dates <-
      dplyr::data_frame(Date = seq(
        startdate,
        by = "day",
        length.out = nrow(humidity)
      )) %>%
      dplyr::mutate(
        date = Date %>%
          as.character %>%
          ymd,
        year = year(date),
        month = month(date),
        day = day(date)
      )
    longhumid <-
      cbind(dates[, c("year", "month", "day")], dailyrh = as.numeric(humidity$dailyrh))
    
    #DEWPT - average daily relative humidity
    monthlyhumid <- longhumid  %>% dplyr::group_by(month) %>%
      dplyr::summarise(monthlyhumid = mean(dailyrh))
    
    monthlyhumid <- longhumid  %>% dplyr::group_by(month, year) %>%
      dplyr::summarise(monthlyhumid = mean(dailyrh))
    meanhumid <- monthlyhumid %>% dplyr::group_by(month) %>%
      dplyr::summarise(mean = mean(monthlyhumid)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "DEWPT1",
          "DEWPT2",
          "DEWPT3",
          "DEWPT4",
          "DEWPT5",
          "DEWPT6",
          "DEWPT7",
          "DEWPT8",
          "DEWPT9",
          "DEWPT10",
          "DEWPT11",
          "DEWPT12"
        )
      )
    st.info <- cbind(st.info, meanhumid[2, ])
    
    
    
    ##----------windspeed-------------##
    wfile <- stationed$`Wind Speed File`[i]
    windspeed <- readr::read_delim(paste(path, wfile, sep = ""), delim = ",")
    
    startdate <-
      as.Date(as.character(names(windspeed)), format = "%Y%m%d")
    names(windspeed) <- "dailywind"
    dates <-
      dplyr::data_frame(Date = seq(
        startdate,
        by = "day",
        length.out = nrow(windspeed)
      )) %>%
      dplyr::mutate(
        date = Date %>%
          as.character %>%
          ymd,
        year = year(date),
        month = month(date),
        day = day(date)
      )
    longwind <-
      cbind(dates[, c("year", "month", "day")], dailywind = as.numeric(windspeed$dailywind))
    
    #WNDAV - average daily wind speed in month
    monthlywind <- longwind  %>% dplyr::group_by(month) %>%
      dplyr::summarise(monthlywind = mean(dailywind)) %>%
      data.table::transpose() %>%
      setNames(
        c(
          "WNDAV1",
          "WNDAV2",
          "WNDAV3",
          "WNDAV4",
          "WNDAV5",
          "WNDAV6",
          "WNDAV7",
          "WNDAV8",
          "WNDAV9",
          "WNDAV10",
          "WNDAV11",
          "WNDAV12"
        )
      )
    st.info <- cbind(st.info, monthlywind[2, ])
    
    #add st.info to wgen
    wgen <- rbind(wgen, st.info)
    
    stationdat <-
      data.frame(names(st.info), data.table::transpose(st.info))
    stationdat %>% dplyr::mutate_all(funs(as.character)) %>%
      dplyr::rename(variable = names.st.info., value = V1) %>%
      write_csv(path = paste(path, st.info$STATION, "_gen.csv", sep = ""))
  }
  
  wgen$OBJECTID <- 1:nrow(wgen)
  wgen$ID <- 1:nrow(wgen)
  wgen$LSTATION <- wgen$STATION
  wgen$STATE <- "NA"
  wgen_out <-
    dplyr::select(
      wgen,
      OBJECTID,
      STATE,
      STATION,
      LSTATION,
      ID,
      WLATITUDE,
      WLONGITUDE,
      WELEV,
      RAIN_YRS,
      everything()
    )
  
  readr::write_csv(wgen_out, path = paste(path, "swat_wgen.csv", sep = ""))
  
}

