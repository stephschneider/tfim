ImportAQData <- function(NAPS_ID){
  #What you need to import at the beginning of running R studio ----

  library(splitr)
  library(tidyverse)
  library(dplyr)
  library(openair)
  library(withr)

  # Import and treat the FIRMS data ----


  if (exists('FIRMS') == FALSE) {
    print("Import FIRMS first!")
  }

  NAPS_Station <- read.csv("StationsNAPS.csv", skip = 3) %>%
      .[-1,] %>%
      .[-(784:826),]

  TimeZone = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Timezone"])*(-1)
  print(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),c("Station_Name", "Start_Year", "End_Year")])

  AQ_Station <- NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Station_Name"]

  # Import the AQ data ----

  #Imports the Air Quality Data
  #Specific for each air quality monitoring station, needs to be updated anytime you import a new station
  #At the end, you get a table of hourly PM data and its time in UTC

  Station_Data <- read.csv(paste("Former Database/", AQ_Station, ".csv", sep = ""), header = TRUE, skip = 5, blank.lines.skip = FALSE) %>%
    .[-(1:3),] %>%
    mutate(Date = X,
           Hour = X.1) %>%
    select(!starts_with("X")) %>%
    mutate(DateTime = as.POSIXct(paste(Date, Hour), format="%d-%b-%y %H", origin = "1970/01/01")) %>%
    filter(!is.na(DateTime)) %>%
    mutate(date = DateTime + (TimeZone*3600),
           Date = NULL,
           DateTime = NULL,
           Hour = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%H"),
           Hour = as.numeric(Hour),
           month = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%m"),
           month = as.numeric(month))


  for (col in 1:ncol(Station_Data)){
    colnames(Station_Data)[col] <-  sub("Parameter..", "", colnames(Station_Data)[col])
  }

to_delete <- which(Station_Data[,"Hour"] == 18 & Station_Data[, "month"] %in% 5:9 == TRUE|
          Station_Data[,"Hour"] == 00 & Station_Data[, "month"] %in% 5:9 == TRUE|
          Station_Data[,"Hour"] == 06 & Station_Data[, "month"] %in% 5:9 == TRUE|
          Station_Data[,"Hour"] == 12 & Station_Data[, "month"] %in% 5:9 == TRUE)
to_delete <- ifelse(to_delete[1] >= 7, to_delete[1] - 7, to_delete[2] - 7)

if (Station_Data[1,"Hour"] == 18 & Station_Data[1, "month"] %in% 5:9 == TRUE|
        Station_Data[1,"Hour"] == 00 & Station_Data[1, "month"] %in% 5:9 == TRUE|
        Station_Data[1,"Hour"] == 06 & Station_Data[1, "month"] %in% 5:9 == TRUE|
        Station_Data[1,"Hour"] == 12 & Station_Data[1, "month"] %in% 5:9 == TRUE){
  Station_Data <- Station_Data
    } else {
    Station_Data <- Station_Data[-(1:to_delete),]
    }

  Station_Data <- Station_Data %>%
    mutate(Hour = NULL,
           month = NULL)

  Station_Data[ , sapply(Station_Data, is.character)  ] <- as.data.frame(
    apply( Station_Data[ , sapply(Station_Data, is.character) ], 2, as.numeric))

  # Average the AQ data ----

  Station_Data <- Station_Data %>%
    timeAverage(avg.time = "6 hour", data.thresh = 75) %>%
    selectByDate(month = 5:9) %>%
    mutate(Hour = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%H"),
           Hour = as.numeric(Hour),
           day = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%Y-%m-%d"),
           day = as.Date(day,"%Y-%m-%d", tz= "UTC"),
           Number_of_Fires = as.numeric(""),
           FRP = as.numeric(""),
           Fire_Influence = as.character(""))

  Station_Data
}

TFIM_calc <- function(NAPS_ID, duration, Year){

  if (Year <= 2000 | Year >= 2020) {
    return("FIRMS data is only available starting in 2001")
    }

  if (exists('FIRMS') == FALSE) {
  return("Import FIRMS first!")
  }

  if (Year == 2013) {
    pollutants <- c("CO", "NO2", "NO", "O3", "PM10", "PM25", "SO2")
  } else {
    pollutants <- c("CO", "NO2", "NO", "NOX", "O3", "PM10", "PM25", "SO2")
  }


  #Checks to see if the Air Quality data is already downloaded, if not downloads it

  i = 1
  for (i in 1:NROW(pollutants)){

    if(!file.exists( paste(getwd(), "/MonitoringData/",
                           Year, "_",
                           pollutants[i],
                           ".csv",
                           sep = ""))) {
      url <- paste("https://data-donnees.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/",
                   Year,
                   "/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/",
                   pollutants[i],
                   "_",
                   Year, ".csv", sep = "")
      dest_file <- paste(getwd(), "/MonitoringData/",
                         Year, "_",
                         pollutants[i],
                         ".csv",
                         sep = "")
      download.file(url, dest_file)
    }
    i = i+1
  }


  #Reads all the csv files
  temp <- as.character("start")
  for (i in 1:NROW(pollutants)){
    sk <- grep("Pollutant",readLines(paste(getwd(), "/MonitoringData/",
                                      Year, "_",
                                      pollutants[i],
                                      ".csv",
                                      sep = ""))) %>%
      .[-1]


    df <- read.csv(paste(getwd(), "/MonitoringData/",
                                                  Year, "_",
                                                  pollutants[i],
                                                  ".csv",
                                                  sep = ""), skip = sk-1)

    names(df) <- gsub("..", "-", names(df), fixed=TRUE)
    names(df) <- gsub("-.*", "", names(df))

    df <- df %>%
      filter(df[,grep("NAPS", names(df))] == NAPS_ID)

    name <- pollutants[i]

      if (NROW(df) > 0){
        if (NROW(df) >= 367){
          df <- df %>%
            select(Date, contains("H", ignore.case = FALSE))

          df[df == -999] <- NA
          df <- na.omit(df)

          df <- df %>%
            pivot_longer(cols = -Date, names_to = "Hour", values_to = name) %>%
            mutate(Hour = gsub("[a-zA-Z ]", "", Hour),
                 Date = as.character(Date),
                 Date = gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3", Date),
                 DateTime = paste(Date, Hour, sep = " "),
                 Date = NULL,
                 Hour = NULL)

        } else {
          df <- df %>%
            select(Date, contains("H")) %>%
            pivot_longer(cols = -Date, names_to = "Hour", values_to = name) %>%
            mutate(Hour = gsub("[a-zA-Z ]", "", Hour),
                   Date = as.character(Date),
                   Date = gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3", Date),
                   DateTime = paste(Date, Hour, sep = " "),
                   Date = NULL,
                   Hour = NULL)

          df[df == -999] <- NA
        }


        if ("start" %in% temp){
          name <- paste("AP_data_", Year, "_", NAPS_ID)
          temp <- df
        } else {
        temp <- dplyr::full_join(temp, df, by = "DateTime")
      }
      }
    i = i+1
  }

  if ("start" %in% temp) {
    return(paste("No data found for ", Year, sep = ""))
  }


  NAPS_Station <- read.csv("StationsNAPS.csv", skip = 3) %>%
    .[-1,] %>%
    .[-(784:826),]

  TimeZone = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Timezone"])*(-1)

  Station_Data <- temp %>%
    mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H", origin = "1970/01/01")) %>%
    mutate(date = DateTime + (TimeZone*3600),
           Hour = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%H"),
           Hour = as.numeric(Hour),
           month = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%m"),
           month = as.numeric(month),
           DateTime = NULL)

  times <- c(0,6, 12, 18)
  to_delete <- which(Station_Data$Hour %in% times & Station_Data$month %in% 5:9)
  to_delete <- ifelse(to_delete[1] >= 7, to_delete[1] - 7, to_delete[2] - 7)

  if (Station_Data[1,"Hour"] %in% times & Station_Data[1, "month"] %in% 5:9){
    Station_Data <- Station_Data
  } else {
    Station_Data <- Station_Data[-(1:to_delete),]
  }

  Station_Data <- Station_Data %>%
    mutate(Hour = NULL,
           month = NULL)

  Station_Data <- Station_Data %>%
    timeAverage(avg.time = "6 hour", data.thresh = 75) %>%
    selectByDate(month = 5:9) %>%
    mutate(Hour = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%H"),
           Hour = as.numeric(Hour),
           day = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%Y-%m-%d"),
           day = as.Date(day,"%Y-%m-%d", tz= "UTC"),
           Number_of_Fires = as.numeric(""),
           FRP = as.numeric(""),
           Fire_Influence = as.character(""))

  AQ_data <- Station_Data

  lat = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Latitude"])
  lon = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Longitude"])

  print(Year)

  for (row in 1:nrow(AQ_data)) {
    for (start_day in AQ_data[row, "day"]){
      start_day = start_day
      start_day2 = start_day - 1
      start_day3 = start_day - 2
    }
    for (start_time in AQ_data[row, "Hour"]){
      start_time = start_time
    }


    #HYSPLIT section
    with_dir(paste(getwd(), "/HYSPLIT", sep = ""), trajectory_model <-
               create_trajectory_model() %>%
               add_trajectory_params(
                 lat = lat,
                 lon = lon,
                 height = 0,
                 duration = duration,
                 days = start_day,
                 daily_hours = c(start_time, start_time-1, start_time-2, start_time-3, start_time-4, start_time-5),
                 direction = "backward",
                 met_type = "narr",
               ) %>%
               run_model()
          )
    trajectory_tbl <- trajectory_model %>% get_output_tbl()

    for (start_daytime in AQ_data[row, "day"]){
      start_daytime = as.POSIXct(start_daytime)
      start_daytime3 = as.POSIXct(start_daytime - 172800)
    }

    #Then I need to get the FIRMS data for the correct date bracket
    #Fixed on January 11th so it now includes time constraints to better match FireSmoke data
    FIRMS_date <- FIRMS %>%
      filter(datetime > start_daytime3) %>%
      filter(datetime < start_daytime) %>%
      mutate(Fire_Influence = as.character(""))


    latmin = min(trajectory_tbl$lat, na.rm = TRUE) - 1
    latmax = max(trajectory_tbl$lat, na.rm = TRUE) +1
    lonmin = min(trajectory_tbl$lon, na.rm = TRUE) - 1
    lonmax = max(trajectory_tbl$lon, na.rm = TRUE) + 1

    FIRMS_date <- subset(FIRMS_date, FIRMS_date$latitude < latmax &
                           FIRMS_date$latitude > latmin &
                           FIRMS_date$longitude < lonmax &
                           FIRMS_date$longitude > lonmin)


    #FIRMS/HYSPLIT interception section
    if (nrow(FIRMS_date)>0){
      for (fire in 1:NROW(FIRMS_date)) {
        latmin = as.numeric(FIRMS_date[fire, "latitude"] - 0.5)
        latmax = as.numeric(FIRMS_date[fire, "latitude"] + 0.5)
        lonmin = as.numeric(FIRMS_date[fire, "longitude"] - 0.5)
        lonmax = as.numeric(FIRMS_date[fire, "longitude"] + 0.5)

        traj_test <- trajectory_tbl %>%
          filter(between(trajectory_tbl$lat,latmin, latmax),
                 between(trajectory_tbl$lon ,lonmin,lonmax))

        if(nrow(traj_test)>0) {
            FIRMS_date[fire, "Fire_Influence"] <- "Yes"
          } else {
            FIRMS_date[fire, "Fire_Influence"] <- "No"
          }

        if (fire == nrow(FIRMS_date)){
          AQ_data[row, "Number_of_Fires"] <- length(which(FIRMS_date$Fire_Influence == 'Yes'))
          AQ_data[row, "FRP"] <- mean(FIRMS_date$frp[FIRMS_date$Fire_Influence=="Yes"])
        }
        fire = fire+1
      }
    } else {
      AQ_data[row, "Number_of_Fires"] <- 0
      AQ_data[row, "FRP"] <- 0
    }
    AQ_data[row, "Fire_Influence"] <- ifelse(AQ_data[row, "Number_of_Fires"] >= 20, "Yes", "No")
    print(row)
    row = row + 1
  }

  AQ_data$FRP <- round(AQ_data$FRP, digits = 1)
  name <- paste(NAPS_ID, duration, Year,  sep = "_")
  write.csv(AQ_data, paste(name, ".csv", sep = ""))
  return(AQ_data)
}

TFIM_plot <- function(NAPS_ID, day, hour){


  if (exists('FIRMS') == FALSE) {
    return("Import FIRMS first!")
  }

   NAPS_Station <- function(){
    NAPS_Station <- read.csv("StationsNAPS.csv", skip = 3) %>%
      .[-1,] %>%
      .[-(784:826),]
  }

  lat = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Latitude"])
  lon = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Longitude"])

plot <- which(AQ_data$day == day & AQ_data$Hour == hour)

  for (row in plot) {
    for (start_day in AQ_data[row, "day"]){
      start_day = start_day
      start_day2 = start_day - 1
      start_day3 = start_day - 2
    }
    for (start_time in AQ_data[row, "Hour"]){
      start_time = start_time
    }


    #HYSPLIT section
    with_dir(tempdir(paste(getwd(), "/HYSPLIT", sep = "")), trajectory_model <-
      create_trajectory_model() %>%
      add_trajectory_params(
        lat = 51.9469,
        lon = -114.6974,
        height = 0,
        duration = 72,
        days = start_day,
        daily_hours = c(start_time, start_time-1, start_time-2, start_time-3, start_time-4, start_time-5),
        direction = "backward",
        met_type = "narr",
      ) %>%
      run_model()
    )
    trajectory_tbl <- trajectory_model %>% get_output_tbl()

    for (start_daytime in AQ_data[row, "day"]){
      start_daytime = as.POSIXct(start_daytime)
      start_daytime3 = as.POSIXct(start_daytime - 172800)
    }

    #Then I need to get the FIRMS data for the correct date bracket
    #Fixed on January 11th so it now includes time constraints to better match FireSmoke data
    FIRMS_date <- FIRMS
    FIRMS_date <- FIRMS_date %>% filter(FIRMS_date$datetime > start_daytime3)
    FIRMS_date <- FIRMS_date %>% filter(FIRMS_date$datetime < start_daytime)

    latmin = min(trajectory_tbl$lat, na.rm = TRUE) - 1
    latmax = max(trajectory_tbl$lat, na.rm = TRUE) +1
    lonmin = min(trajectory_tbl$lon, na.rm = TRUE) - 1
    lonmax = max(trajectory_tbl$lon, na.rm = TRUE) + 1

    FIRMS_date <- subset(FIRMS_date, FIRMS_date$latitude < latmax)
    FIRMS_date <- subset(FIRMS_date, FIRMS_date$latitude > latmin)
    FIRMS_date <- subset(FIRMS_date, FIRMS_date$longitude < lonmax)
    FIRMS_date <- subset(FIRMS_date, FIRMS_date$longitude > lonmin)


    #FIRMS/HYSPLIT interception section
    for (fire in 1:NROW(FIRMS_date)) {
      if (nrow(FIRMS_date)>0){
        latmin = FIRMS_date[fire, "latitude"] - 0.5
        latmax = FIRMS_date[fire, "latitude"] + 0.5
        lonmin = FIRMS_date[fire, "longitude"] - 0.5
        lonmax = FIRMS_date[fire, "longitude"] + 0.5
        for(point in 1:NROW(trajectory_tbl)){
          if(trajectory_tbl[point, "lat"] < latmax && trajectory_tbl[point, "lat"] > latmin && trajectory_tbl[point, "lon"] < lonmax && trajectory_tbl[point, "lon"] > lonmin) {
            trajectory_tbl[point, "Fire_Influence"] <- "Yes"
          } else if (trajectory_tbl[point, "lat"] < latmax + 0.5 && trajectory_tbl[point, "lat"] > latmin - 0.5 && trajectory_tbl[point, "lon"] < lonmax + 0.5  && trajectory_tbl[point, "lon"] > lonmin - 0.5) {
            trajectory_tbl[point, "Fire_Influence"] <- "Maybe"
          } else {
            trajectory_tbl[point, "Fire_Influence"] <- "No"
          }
          point = point + 1
        }

        ifelse (trajectory_tbl$Fire_Influence  %in% "Maybe" == TRUE, FIRMS_date[fire, "Fire_Influence"] <- "Maybe", FIRMS_date[fire, "Fire_Influence"] <- "No")
        ifelse (trajectory_tbl$Fire_Influence  %in% "Yes" == TRUE, FIRMS_date[fire, "Fire_Influence"] <- "Yes", FIRMS_date[fire, "Fire_Influence"] <- FIRMS_date[fire, "Fire_Influence"])

        fire = fire+1

        if ("Yes" %in% FIRMS_date$Fire_Influence == TRUE) {
          AQ_data[row, "Fire_Influence"] <- "Yes"
        } else {
          AQ_data[row, "Fire_Influence"] <- "No"
        }
      } else {
        AQ_data[row, "Fire_Influence"] <- "No"
      }

      if (nrow(FIRMS_date)>0){
        AQ_data[row, "Number_of_Fires"] <- length(which(FIRMS_date$Fire_Influence == 'Yes'))
        if ("Yes" %in% AQ_data$Fire_Influence  == TRUE) {
          AQ_data[row, "FRP"] <- mean(FIRMS_date$frp[FIRMS_date$Fire_Influence=="Yes"])
        } else {
          AQ_data[row, "FRP"] <- 0
        }
      } else {
        AQ_data[row, "Number of Fires"] <- 0
      }
    }
  }

  print(ggplot(data = trajectory_tbl, aes(x=lon, y=lat)) +
    geom_point(color='black') +
    geom_point(data=FIRMS_date, color='red', size=2, aes(x=longitude, y=latitude)))
}

Import_FIRMS <- function(NameofFile) {

  NameofFile <- as.character(NameofFile)

    if (exists('FIRMS') == FALSE) {
    FIRMS <- read_csv(paste(NameofFile, ".csv", sep="")) %>%
      filter(confidence >= 30) %>%
      mutate(acq_date = as.Date(acq_date),
             datetime = format(strptime(acq_time, format="%H%M"), format = "%H:%M"),
             datetime = as.POSIXct(paste(acq_date, datetime), format = "%Y-%m-%d %H:%M"))
    }
  FIRMS <<- FIRMS
}

NAPS_info <- function(){
  NAPS_Station <- read.csv("StationsNAPS.csv", skip = 3) %>%
    .[-1,]
  NAPS_info <- NAPS_Station[785:826,c(1,4)]
}

NAPS_Station_info <- function(){
  NAPS_Station <- read.csv("StationsNAPS.csv", skip = 3) %>%
    .[-1,] %>%
    .[-(784:826),]
  NAPS_Station<<- NAPS_Station
}

NAPS_Station_plot <- function(province,
                              site_type = "default",
                              urbanization = "default",
                              status = "default",
                              pollutants = "default") {

  NAPS_Station_info()
  NAPS_Station <- NAPS_Station %>%
    filter(NAPS_Station$P.T == province)
  if (site_type != "default") {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$Site_Type == site_type)
    }
  if (status != "default") {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$Status == as.numeric(status))
    }
  if (urbanization != "default") {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$Urbanization == urbanization)
  }
  if ("SO2" %in% pollutants) {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$SO2 == "X")
  }
  if ("NO2" %in% pollutants) {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$NO2 == "X")
  }
  if ("NO" %in% pollutants) {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$NO == "X")
  }
  if ("CO" %in% pollutants) {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$CO == "X")
  }
  if ("O3" %in% pollutants) {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$O3 == "X")
  }
  if ("NOX" %in% pollutants) {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$NOX == "X")
  }
  if ("PM25" %in% pollutants) {
    NAPS_Station <- NAPS_Station %>%
      filter(NAPS_Station$PM_25_Continuous == "X")
  }


  NAPS_box <- make_bbox(lon = as.numeric(NAPS_Station$Longitude), lat = as.numeric(NAPS_Station$Latitude), f= 0.01)
  NAPS_map <- get_stamenmap(bbox = NAPS_box, zoom = 6, maptype = "terrain", crop = FALSE)
  transectMap <- ggmap(NAPS_map)
  transectMap <- transectMap +
    geom_point(data = NAPS_Station,
               aes(x = as.numeric(Longitude),
                   y = as.numeric(Latitude)))+
    geom_label_repel(data = NAPS_Station,
              aes(x = as.numeric(Longitude),
                             y = as.numeric(Latitude),
                             label = Station_Name),
              size = 3,
              force_pull = 0.2,
              force = 10,
              max.overlaps = Inf)


  return(list(transectMap, NAPS_Station[, c("NAPS_ID", "Station_Name", "Start_Year", "End_Year")]))

}
