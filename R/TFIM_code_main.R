TFIM_NAPS <- function(NAPS_ID, duration, year, met = NULL){

  #Defining default meteorology, which is based on year
  if(is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") <= 2019) {
    met <- "narr"
  } else if (is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") > 2019) {
    met <- "nam12"
  }

  #Reminder to run import_FIRMS() first
  if (exists('FIRMS') == FALSE) {
  return("Import FIRMS first!")
  }

  if (year == 2013) {
    pollutants <- c("CO", "NO2", "NO", "O3", "PM10", "PM25", "SO2")
  } else {
    pollutants <- c("CO", "NO2", "NO", "NOX", "O3", "PM10", "PM25", "SO2")
  }


  #Checks to see if the Air Quality data is already downloaded, if not downloads it

  i = 1
  for (i in 1:NROW(pollutants)){

    if(!file.exists( paste(getwd(), "/MonitoringData/",
                           year, "_",
                           pollutants[i],
                           ".csv",
                           sep = ""))) {
      url <- paste("https://data-donnees.az.ec.gc.ca/api/file?path=/air%2Fmonitor%2Fnational-air-pollution-surveillance-naps-program%2FData-Donnees%2F",
                   year,
                   "%2FContinuousData-DonneesContinu%2FHourlyData-DonneesHoraires%2F",
                   pollutants[i],
                   "_",
                   year, ".csv", sep = "")
      dest_file <- paste(getwd(), "/MonitoringData/",
                         year, "_",
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
                                      year, "_",
                                      pollutants[i],
                                      ".csv",
                                      sep = ""))) %>%
      .[-1]


    df <- read.csv(paste(getwd(), "/MonitoringData/",
                                                  year, "_",
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
          name <- paste("AP_data_", year, "_", NAPS_ID)
          temp <- df
        } else {
        temp <- dplyr::full_join(temp, df, by = "DateTime")
      }
      }
    i = i+1
  }

  if ("start" %in% temp) {
    return(paste("No data found for ", year, sep = ""))
  }


  NAPS_Station <- NAPS()

  TimeZone = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Timezone"])*(-1)

  Station_Data <- temp %>%
    mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H", origin = "1970/01/01")) %>%
    mutate(date = DateTime + (TimeZone*3600),
           Hour = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%H"),
           Hour = as.numeric(Hour),
           month = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%m"),
           month = as.numeric(month),
           DateTime = NULL)%>%
    selectByDate(month = 5:9)

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
    timeAverage(avg.time = "6 hour") %>%
    mutate(Hour = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%H"),
           Hour = as.numeric(Hour),
           day = format(as.POSIXct(strptime(date,"%Y-%m-%d %H",tz="")) ,format = "%Y-%m-%d"),
           day = as.Date(day,"%Y-%m-%d", tz= "UTC"),
           Number_of_Fires = as.numeric(""),
           FRP = as.numeric(""),
           Fire_Influence = as.character(""),
           intercept_time = as.numeric(""))

  AQ_data <- Station_Data

  lat = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Latitude"])
  lon = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Longitude"])

  mainDir <- getwd()
  metDir <- "HYSPLIT"
  airDir <- "MonitoringData"

  dir.create(file.path(mainDir, metDir), showWarnings = FALSE)
  dir.create(file.path(mainDir, airDir), showWarnings = FALSE)

  print(year)

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
                 met_type = met,
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
          AQ_data[row, "intercept_time"] <- mean(FIRMS_date$interception[FIRMS_date$Fire_Influence=="Yes"])
        }
        fire = fire+1
      }
    } else {
      AQ_data[row, "Number_of_Fires"] <- 0
      AQ_data[row, "FRP"] <- 0
      AQ_data[row, "intercept_time"] <- NA
    }
    AQ_data[row, "Fire_Influence"] <- ifelse(AQ_data[row, "Number_of_Fires"] >= 20, "Yes", "No")
    if (row %% 10 == 0) {print(round(row/NROW(AQ_data)*100, 2), "%", sep = " ")}
    row = row + 1
  }

  AQ_data$FRP <- round(AQ_data$FRP, digits = 1)
  name <- paste(NAPS_ID, duration, year,  sep = "_")
  write.csv(AQ_data, paste(name, ".csv", sep = ""))
  return(AQ_data)
}

TFIM <- function(lat, lon, duration, start_date, end_date, met = NULL, satellite = NULL, location = NULL){

  #Defining default meteorology, which is based on year
  if(is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") <= 2019) {
    met <- "narr"
  } else if (is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") > 2019) {
    met <- "nam12"
  }

  #Creating output to promt FIRMS update
  if (exists('FIRMS') == FALSE) {
    return("Import FIRMS first!")
  }

  #creating dataframe which can be accepted by the HYSPLIT model; includes pre-set 6-hour periods (in UTC)
  day <- seq(as.Date(start_date), as.Date(end_date), by = "days")
  AQ_data <- data.frame(day, "0")
  AQ_data <- AQ_data %>%
    mutate("00" = 0,
           "06" =0,
           "12" = 0,
           "18" = 0,
           "X.00." = NULL) %>%
    pivot_longer(cols = c("00", "06", "12", "18"), names_to = "Hour", values_to = "bop") %>%
    mutate(date = paste(as.character(day), Hour, sep = " "),
           date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
    select(-`X.0.`) %>%
    timeAverage(avg.time = "6 hour") %>%
    mutate(Hour = format(as.POSIXct(strptime(date,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H"),
           Hour = as.numeric(Hour),
           day = format(as.POSIXct(strptime(date,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d"),
           day = as.Date(day,"%Y-%m-%d", tz= "UTC"),
           Number_of_Fires = as.numeric(""),
           FRP = as.numeric(""),
           Fire_Influence = as.character(""),
           intercept_time = as.numeric(""),
           Hour = case_when(Hour == 24~0,
                            Hour == 23~0,
                            Hour == 5~6,
                            Hour == 17~18,
                            Hour == 11~12,
                            Hour == 23~0,
                            Hour %in% c(0, 6, 12, 18)~Hour))

  #defining variables for HYSPLIT
  for (row in 1:NROW(AQ_data)){
    for (start_day in AQ_data[row, "day"]){
      start_day = start_day
      start_day2 = start_day - 1
      start_day3 = start_day - 2
    }
    for (start_time in AQ_data[row, "Hour"]){
      start_time = start_time
    }

    mainDir <- getwd()
    metDir <- "HYSPLIT"

    dir.create(file.path(mainDir, metDir), showWarnings = FALSE)

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
                 met_type = met,
               ) %>%
               run_model()
    )
    trajectory_tbl <- trajectory_model %>% get_output_tbl()

    trajectory_tbl <- trajectory_tbl

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


    latmin = min(trajectory_tbl$lat, na.rm = TRUE) - 0.5
    latmax = max(trajectory_tbl$lat, na.rm = TRUE) + 0.5
    lonmin = min(trajectory_tbl$lon, na.rm = TRUE) - 0.5
    lonmax = max(trajectory_tbl$lon, na.rm = TRUE) + 0.5

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
          FIRMS_date[fire, "Fire_Influence"] = "Yes"
          FIRMS_date[fire, "interception"] = mean(traj_test$hour_along)
        } else {
          FIRMS_date[fire, "Fire_Influence"] <- "No"
        }

        if (fire == nrow(FIRMS_date)){
          AQ_data[row, "Number_of_Fires"] <- length(which(FIRMS_date$Fire_Influence == 'Yes'))
          AQ_data[row, "FRP"] <- mean(FIRMS_date$frp[FIRMS_date$Fire_Influence=="Yes"])
          AQ_data[row, "intercept_time"] <- mean(FIRMS_date$interception[FIRMS_date$Fire_Influence=="Yes"])
        }
        fire = fire+1
      }
    } else {
      AQ_data[row, "Number_of_Fires"] <- 0
      AQ_data[row, "FRP"] <- NA
      AQ_data[row, "intercept_time"] <- NA
    }
    AQ_data[row, "Fire_Influence"] <- ifelse(AQ_data[row, "Number_of_Fires"] >= 20, "Yes", "No")
    if (row %% 10 == 0) {print(paste(round(row/nrow(AQ_data)*100, 2), "%", sep = " "))}
    row = row + 1
  }

  AQ_data$FRP <- round(AQ_data$FRP, digits = 1)

  name <- paste(AQ_data[1, "day"], ifelse(is.null(location), "", location), duration, ifelse(is.null(satellite), "", satellite),  sep = "_")

  write.csv(AQ_data, paste(name, ".csv", sep = ""))
  return(AQ_data)
}


TFIM_plot <- function(NAPS_ID, date, hour, duration, met = NULL){

  #Defining default meteorology, which is based on year
  if(is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") <= 2019) {
    met <- "narr"
  } else if (is.null(met) & format(as.POSIXct(strptime(end_date,"%Y/%m/%d",tz="")) ,format = "%Y") > 2019) {
    met <- "nam12"
  }

  NAPS_Station <- NAPS()

  date <- as.Date(date)

  lat = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Latitude"])
  lon = as.numeric(NAPS_Station[which(NAPS_Station$NAPS_ID == NAPS_ID),"Longitude"])

    #HYSPLIT section
  with_dir(paste(getwd(), "/HYSPLIT", sep = ""), trajectory_model <-
               create_trajectory_model() %>%
               add_trajectory_params(
                 lat = lat,
                 lon = lon,
                 height = 0,
                 duration = duration,
                 days = date,
                 daily_hours = c(hour, hour-1, hour-2, hour-3, hour-4, hour-5),
                 direction = "backward",
                 met_type = met,
               ) %>%
               run_model()
    )
    trajectory_tbl <- trajectory_model %>% get_output_tbl()

    start_daytime = as.POSIXct(date, tz = "UTC")
    start_daytime3 = as.POSIXct(start_daytime - 172800)

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
        fire = fire+1
      }
    }

  print(ggplot(data = trajectory_tbl, aes(x=lon, y=lat)) +
    geom_point(color='black') +
    geom_point(data=FIRMS_date,size=2, aes(x=longitude, y=latitude, color= Fire_Influence)))+
    theme_bw()+
    annotate("point", x = lon, y = lat, colour = "purple", size = 5)
}

Import_FIRMS <- function(instrument, NameofFile = NULL) {

  if(is.null(NameofFile)){
    NameofFile <- "FIRMS_data"
  }

  if(instrument != "MODIS" & instrument != "VIIRS"){
    print("Please indicate either MODIS or VIIRS")
  }

  NameofFile <- as.character(NameofFile)

  if (exists('FIRMS') == FALSE) {
    FIRMS <- readr::read_csv(paste(NameofFile, ".csv", sep="")) %>%
      mutate(acq_date = as.Date(acq_date),
             datetime = format(strptime(acq_time, format="%H%M"), format = "%H:%M"),
             datetime = as.POSIXct(paste(acq_date, datetime), format = "%Y-%m-%d %H:%M"))
  }

  if (instrument == "MODIS"){
    FIRMS <- FIRMS %>%
      filter(confidence >= 30)
  }

  if (instrument == "VIIRS"){
    FIRMS <- FIRMS %>%
      filter(confidence != "L")
  }

  FIRMS <<- FIRMS
}

NAPS_info <- function(){

  NAPS_Station <- read.csv("StationsNAPS.csv", skip = 3) %>%
    .[-1,] %>%
    .[785:826,c(1,4)]

  return(NAPS_Station)
}

NAPS <- function(){
  NAPS_Station <- read.csv("StationsNAPS.csv", skip = 3) %>%
    .[-1,] %>%
    .[-(784:826),]

  return(NAPS_Station)
}


