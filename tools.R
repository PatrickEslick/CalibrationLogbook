library(xml2)
library(purrr)
library(lubridate)
library(DBI)
library(RSQLite)
library(dplyr)

read_sv_xml <- function(xml_path) {
  
  sv_xml <- read_xml(xml_path)
  sv_xml <- xml_ns_strip(sv_xml)
  return(sv_xml)
  
}

get_SENSOR <- function(sv_xml) {
  
  sensors <- xml_find_all(sv_xml, ".//Sensor")
  
  if(length(sensors) > 0) {
    sns <- xml_find_first(sensors, ".//SerialNumber") %>%
      xml_text() 
    parameters <- xml_find_first(sensors, ".//Name") %>%
      xml_text()
    manu <- xml_find_first(sensors, ".//ManufacturerName") %>%
      xml_text()
    model <- xml_find_first(sensors, ".//ModelNumber") %>%
      xml_text()
    
    sensor_df <- data.frame(SENSOR_SN = sns, PARAMETER = parameters, MANUFACTURER = manu,
                            MODEL = model,
                            stringsAsFactors = FALSE) %>%
      unique()
  } else {
    sensor_df <- data.frame(SENSOR_SN = vector(), PARAMETER = vector(), MANUFACTURER = vector(),
                            MODEL = vector())
  }
 
  return(sensor_df) 
}

get_SC_CHECK <- function(sv_xml) {
  
  sc_drift <- xml_find_all(sv_xml, ".//DriftCheckConductance")
  
  if(length(sc_drift) != 0) {
    
    SENSOR_ID <- xml_find_first(sc_drift, ".//Sensor//SerialNumber") %>%
      xml_text()
    CELL_CONSTANT <- xml_find_first(sc_drift, ".//CellRangeMeasurePreCal") %>%
      xml_text()
    AIR_READING <- xml_find_first(sc_drift, ".//InAirMeasure") %>%
      xml_text()
    COMMENT <- xml_find_first(sc_drift, ".//Comment") %>%
      xml_text()
    
    sc_check_df <- data.frame(SENSOR_ID, CELL_CONSTANT, AIR_READING, COMMENT,
                              stringsAsFactors = FALSE)
    
  } else {
    
    sc_check_df <- data.frame(SENSOR_ID = vector(), CELL_CONSTANT = vector(),
                            AIR_READING = vector(), COMMENT = vector())
    
  }
  
  return(sc_check_df)
  
}

get_SC_READING <- function(sv_xml) {
  
  sc_readings <- xml_find_all(sv_xml, ".//DriftCheckConductanceReading")
  
  if(length(sc_readings) > 0) {
    
    STD_VALUE <- xml_find_first(sc_readings, ".//StandardMeasure") %>%
      xml_text()
    STD_EXPIRATION <- xml_find_first(sc_readings, ".//ExpirationDate") %>%
      xml_text() %>%
      ymd() %>%
      as.character(format = "%Y-%m-%d")
    STD_TYPE <- xml_find_first(sc_readings, ".//StandardCode") %>%
      xml_text()
    STD_LOT <- xml_find_first(sc_readings, ".//LotNumber") %>%
      xml_text()
    READING <- xml_find_first(sc_readings, ".//ConductanceMeasure") %>%
      xml_text()
    TEMPERATURE <- xml_find_first(sc_readings, ".//TemperatureMeasure") %>%
      xml_text()
    DATETIME <- xml_find_first(sc_readings, ".//ReadingDateTime") %>%
      xml_text() %>%
      ymd_hms() %>%
      as.character(format = "%Y-%m-%d %H:%M:%S")
    TYPE <- xml_find_first(sc_readings, ".//CalibrationCode") %>%
      xml_text()
    
    sc_reading_df <- data.frame(STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING,
                                TEMPERATURE, DATETIME, TYPE,
                                stringsAsFactors = FALSE)
    
  } else {
    
    sc_reading_df <- data.frame(STD_VALUE = vector(), STD_EXPIRATION = vector(), STD_TYPE = vector(),
                                STD_LOT = vector(), READING = vector(), TEMPERATURE = vector(),
                                DATETIME = vector(), TYPE = vector())
    
  }
  
  return(sc_reading_df)
  
}

get_TBY_CHECK <- function(sv_xml) {
  
  tby_drift <- xml_find_all(sv_xml, ".//DriftCheckTurbidity")
  
  if(length(tby_drift) != 0) {
    
    SENSOR_ID <- xml_find_first(tby_drift, ".//Sensor//SerialNumber") %>%
      xml_text()
    SENSOR_LIMIT <- xml_find_first(tby_drift, ".//MaximumMeasure") %>%
      xml_text()
    COMMENT <- xml_find_first(tby_drift, ".//Comment") %>%
      xml_text()
    tby_check_df <- data.frame(SENSOR_ID, SENSOR_LIMIT, COMMENT,
                               stringsAsFactors = FALSE)
    
  } else {
    
    tby_check_df <- data.frame(SENSOR_ID = vector(), SENSOR_LIMIT = vector(), 
                               COMMENT = vector())
    
  } 
  
  return(tby_check_df)
  
}

get_TBY_READING <- function(sv_xml) {
  
  tby_readings <- xml_find_all(sv_xml, ".//DriftCheckTurbidityReading")
  
  if(length(tby_readings) > 0) {
    
    STD_VALUE <- xml_find_first(tby_readings, ".//StandardMeasure") %>%
      xml_text()
    STD_EXPIRATION <- xml_find_first(tby_readings, ".//ExpirationDate") %>%
      xml_text() %>%
      ymd() %>%
      as.character("%Y-%m-%d")
    STD_TYPE <- xml_find_first(tby_readings, ".//StandardCode") %>%
      xml_text()
    STD_LOT <- xml_find_first(tby_readings, ".//LotNumber") %>%
      xml_text()
    READING <- xml_find_first(tby_readings, ".//TurbidityMeasure") %>%
      xml_text()
    TEMPERATURE <- xml_find_first(tby_readings, ".//TemperatureMeasure") %>%
      xml_text()
    DATETIME <- xml_find_first(tby_readings, ".//ReadingDateTime") %>%
      xml_text() %>%
      ymd_hms() %>%
      as.character(format= "%Y-%m-%d %H:%M:%S")
    TYPE <- xml_find_first(tby_readings, ".//CalibrationCode") %>%
      xml_text()
    
    tby_reading_df <- data.frame(STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING,
                                 TEMPERATURE, DATETIME, TYPE,
                                 stringsAsFactors = FALSE)
    
  } else {
    tby_reading_df <- data.frame(STD_VALUE = vector(), STD_EXPIRATION = vector(),
                                 STD_TYPE = vector(), STD_LOT = vector(), 
                                 READING = vector(), TEMPERATURE =  vector(),
                                 DATETIME = vector(), TYPE = vector())
  }
  
  return(tby_reading_df)
  
}

get_DO_CHECK <- function(sv_xml) {
  
  do_drift <- xml_find_all(sv_xml, ".//DriftCheckDissolvedOxygen")
  
  if(length(do_drift) != 0) {
    
    SENSOR_ID <- xml_find_first(do_drift, ".//Sensor//SerialNumber") %>%
      xml_text()
    SC_AIR_SATURATED_WATER <- xml_find_first(do_drift, ".//ConductanceOfWater") %>%
      xml_text()
    TEMP_AIR_SATURATED_WATER <- xml_find_first(do_drift, ".//TemperatureOfWater") %>%
      xml_text()
    SALINITY <- xml_find_first(do_drift, ".//SalinityMeasure") %>%
      xml_text()
    DATE_BAROMETER_CALIBRATED <- xml_find_first(do_drift, ".//BarometerCalibrationDate") %>%
      xml_text() %>%
      ymd_hms() %>%
      as.character(format = "%Y-%m-%d")
    ODO_GAIN_PRE <- xml_find_first(do_drift, ".//PreCalDoGainMeasure") %>%
      xml_text()
    ODO_CAP_CHANGED <- xml_find_first(do_drift, ".//OpticalDOCapChanged") %>%
      xml_text()
    ODO_CAP_SN   <- xml_find_first(do_drift, ".//OpticalDOCapSerialNumber") %>%
      xml_text()
    ODO_GAIN_POST <- xml_find_first(do_drift, ".//PostCalDoGainMeasure") %>%
      xml_text()
    COMMENT <- xml_find_first(do_drift, ".//Comment") %>%
      xml_text()
    
    do_check_df <- data.frame(SENSOR_ID, SC_AIR_SATURATED_WATER, TEMP_AIR_SATURATED_WATER, SALINITY,
                              DATE_BAROMETER_CALIBRATED, ODO_GAIN_PRE, ODO_CAP_CHANGED, ODO_CAP_SN,
                              ODO_GAIN_POST, COMMENT,
                              stringsAsFactors = FALSE)
    
  } else {
    
    do_check_df <- data.frame(SENSOR_ID = vector(), SC_AIR_SATURATED_WATER = vector(),
                              TEMP_AIR_SATURATED_WATER = vector(), SALINITY = vector(),
                              DATE_BAROMETER_CALIBRATED = vector(), ODO_GAIN_PRE = vector(),
                              ODO_CAP_CHANGED = vector(), ODO_CAP_SN = vector(),
                              ODO_GAIN_POST = vector(), COMMENT = vector())
    
  }
  
  return(do_check_df)
  
}

get_DO_READING <- function(sv_xml) {
  
  do_readings <- xml_find_all(sv_xml, ".//DriftCheckDissolvedOxygenReading")
  
  if(length(do_readings) > 0) {
    
    TEMPERATURE <- xml_find_first(do_readings, ".//TemperatureMeasure") %>%
      xml_text()
    PRESSURE <- xml_find_first(do_readings, ".//BarometricMeasure") %>%
      xml_text()
    SALINITY_CORRECTION <- xml_find_first(do_readings, ".//SalinityCorrectionFactor") %>%
      xml_text()
    DO_TABLE_VALUE <- xml_find_first(do_readings, ".//DissolvedOxygenTableMeasure") %>%
      xml_text()
    READING <- xml_find_first(do_readings, ".//DissolvedOxygenMeasure") %>%
      xml_text()
    DATETIME <- xml_find_first(do_readings, ".//ReadingDateTime") %>%
      xml_text() %>%
      ymd_hms() %>%
      as.character(format = "%Y-%m-%d %H:%M:%S")
    ZERO_READING <- xml_find_first(do_readings, ".//DissolvedOxygenZeroMeasure") %>%
      xml_text()
    TYPE <- xml_find_first(do_readings, ".//CalibrationCode") %>%
      xml_text()
    
    do_reading_df <- data.frame(TEMPERATURE, PRESSURE, SALINITY_CORRECTION, DO_TABLE_VALUE,
                                READING, DATETIME, ZERO_READING, TYPE,
                                stringsAsFactors = FALSE)
    
  } else {
    
    do_reading_df <- data.frame(TEMPERATURE = vector(), PRESSURE = vector(), SALINITY_CORRECTION = vector(),
                                DO_TABLE_VALUE = vector(), READING = vector(), DATETIME = vector(),
                                ZERO_READING = vector(), TYPE = vector())
    
  } 
  
  return(do_reading_df)
  
}

get_PH_CHECK <- function(sv_xml) {
  
  ph_drift <- xml_find_all(sv_xml, ".//DriftCheckPh")
  
  if(length(ph_drift) != 0) {
    
    SENSOR_ID <- xml_find_first(ph_drift, ".//Sensor//SerialNumber") %>%
      xml_text()
    COMMENT <- xml_find_first(ph_drift, ".//Comment") %>%
      xml_text()
    
    ph_check_df <- data.frame(SENSOR_ID, COMMENT, 
                              stringsAsFactors = FALSE)
    
  } else {
    
    ph_check_df <- data.frame(SENSOR_ID = vector(), COMMENT = vector())
    
  } 
  
  return(ph_check_df)
  
}

get_PH_READING <- function(sv_xml) {
  
  ph_readings <- xml_find_all(sv_xml, ".//DriftCheckPhReading")
  
  if(length(ph_readings) > 0) {
    
    STD_UNCORRECTED <- xml_find_first(ph_readings, ".//StandardMeasureBeforeTemperatureAdjustment") %>%
      xml_text()
    STD_EXPIRATION <- xml_find_first(ph_readings, ".//ExpirationDate") %>%
      xml_text() %>%
      ymd() %>%
      as.character(format = "%Y-%m-%d")
    STD_TYPE <- xml_find_first(ph_readings, ".//StandardCode") %>%
      xml_text()
    STD_LOT <- xml_find_first(ph_readings, ".//LotNumber") %>%
      xml_text()
    TEMPERATURE <- xml_find_first(ph_readings, ".//TemperatureMeasure") %>%
      xml_text()
    STD_VALUE <- xml_find_first(ph_readings, ".//StandardMeasure") %>%
      xml_text()
    READING <- xml_find_first(ph_readings, ".//PhMeasure") %>%
      xml_text()
    DATETIME <- xml_find_first(ph_readings, ".//ReadingDateTime") %>%
      xml_text() %>%
      ymd_hms() %>%
      as.character(format="%Y-%m-%d %H:%M:%S")
    MILLIVOLTS <- xml_find_first(ph_readings, ".//VoltsMeasure") %>%
      xml_text()
    TYPE <- xml_find_first(ph_readings, ".//CalibrationCode") %>%
      xml_text()
    
    ph_reading_df <- data.frame(STD_UNCORRECTED, STD_EXPIRATION, STD_TYPE, STD_LOT, TEMPERATURE, 
                                STD_VALUE, READING, DATETIME, MILLIVOLTS, TYPE,
                                stringsAsFactors = FALSE)
    
  } else {
    
    ph_reading_df <- data.frame(STD_UNCORRECTED = vector(), STD_EXPIRATION = vector(), 
                                STD_TYPE = vector(), STD_LOT = vector(),
                                TEMPERATURE = vector(), STD_VALUE = vector(), READING = vector(),
                                DATETIME = vector(), MILLIVOLTS = vector(), TYPE = vector())
    
  }
  
  return(ph_reading_df)
  
}

get_WT_MULTIPOINT_CHECK <- function(sv_xml) {
  
  wt_drift <- xml_find_all(sv_xml, ".//DriftCheckWaterTemperature")
  
  if(length(wt_drift) != 0) {
    
    SENSOR_ID <- xml_find_first(wt_drift, ".//Sensor//SerialNumber") %>%
      xml_text()
    NIST_CERT_DATE <- xml_find_first(wt_drift, ".//NistAstmCertificationDate") %>%
      xml_text()
    NIST_SN <- xml_find_first(wt_drift, ".//NistSerialNumber") %>%
      xml_text()
    COMMENT <- xml_find_first(wt_drift, ".//Comment") %>%
      xml_text()
    
    wt_check_df <- data.frame(SENSOR_ID, NIST_CERT_DATE, NIST_SN, COMMENT, 
                              stringsAsFactors = FALSE)
    
  } else {
    
    wt_check_df <- data.frame(SENSOR_ID = vector(), NIST_CERT_DATE = vector(),
                              NIST_SN = vector(), COMMENT = vector())
    
  } 
  
  return(wt_check_df)
  
  
}

get_WT_MULTIPOINT_READING <- function(sv_xml) {
  
  wt_readings <- xml_find_all(sv_xml, ".//MultiPointTempCheck")
  
  if(length(wt_readings) > 0) {
    
    NIST_READING <- xml_find_first(wt_readings, ".//WaterTemperatureNistMeasure") %>%
      xml_text()
    MONITOR_READING <- xml_find_first(wt_readings, ".//WaterTemperatureMeasure") %>%
      xml_text()
    DATETIME <- xml_find_first(wt_readings, ".//ReadingDateTime") %>%
      xml_text() %>%
      ymd_hms() %>%
      as.character(format="%Y-%m-%d %H:%M:%S")
    
    wt_reading_df <- data.frame(NIST_READING, MONITOR_READING, DATETIME,
                                stringsAsFactors = FALSE)
    
  } else {
    
    wt_reading_df <- data.frame(NIST_READING = vector(), MONITOR_READING = vector(),
                                DATETIME = vector())
    
  }
  
  return(wt_reading_df)
  
}

get_WT_COMPARISON <- function(sv_xml) {
  
  wt_drift <- xml_find_all(sv_xml, ".//DriftCheckWaterTemperature")
  
  if(length(wt_drift) != 0) {
    
    SENSOR_ID <- xml_find_first(wt_drift, ".//Sensor//SerialNumber") %>%
      xml_text()
    FIELD_SENSOR_SN <- xml_find_first(wt_drift, ".//FieldSensor//SerialNumber") %>%
      xml_text()
    DATETIME <- xml_find_first(wt_drift, ".//ReadingDateTime") %>%
      xml_text() %>%
      ymd_hms() %>%
      as.character(format="%Y-%m-%d %H:%M:%S")
    CHECK_MEASURE <- xml_find_first(wt_drift, ".//WaterTemperatureMeterMeasure") %>%
      xml_text()
    SENSOR_MEASURE <- xml_find_first(wt_drift, ".//WaterTemperatureMeasure") %>%
      xml_text()
    TWO_POINT_CHECK_DATE <- xml_find_first(wt_drift, ".//TwoPointCheckDate") %>%
      xml_text()
    FIVE_POINT_CHECK_DATE <- xml_find_first(wt_drift, ".//FivePointCheckDate") %>%
      xml_text()
    COMMENT <- xml_find_first(wt_drift, ".//Comment") %>%
      xml_text()
    
    wt_check_df <- data.frame(SENSOR_ID, FIELD_SENSOR_SN, DATETIME, CHECK_MEASURE,
                              SENSOR_MEASURE, TWO_POINT_CHECK_DATE, FIVE_POINT_CHECK_DATE,
                              COMMENT,
                              stringsAsFactors = FALSE)
    
  } else {
    
    wt_check_df <- data.frame(SENSOR_ID = vector(), FIELD_SENSOR_SN = vector(),
                              DATETIME = vector(), CHECK_MEASURE = vector(), 
                              SENSOR_MEASURE = vector(), TWO_POINT_CHECK_DATE = vector(),
                              FIVE_POINT_CHECK_DATE = vector(), COMMENT = vector(),
                              stringsAsFactors = FALSE)
    
  } 
  
  return(wt_check_df)
  
}

get_CALIBRATION <- function(sv_xml) {
  
  cal_type <- "SITEVISIT"
  monitor_sn <- xml_find_first(sv_xml, ".//MonitorInspection//SerialNumber") %>%
    xml_text()
  
  cal <- data.frame(MONITOR_SN = monitor_sn, CAL_TYPE = cal_type)
  
  return(cal)
  
}

get_ALL <- function(sv_xml) {
  
  all_sv_data <- list()
  all_sv_data[["SENSOR"]] <- get_SENSOR(sv_xml)
  all_sv_data[["SC_CHECK"]] <- get_SC_CHECK(sv_xml)
  all_sv_data[["SC_READING"]] <- get_SC_READING(sv_xml)
  all_sv_data[["TBY_CHECK"]] <- get_TBY_CHECK(sv_xml)
  all_sv_data[["TBY_READING"]] <- get_TBY_READING(sv_xml)
  all_sv_data[["PH_CHECK"]] <- get_PH_CHECK(sv_xml)
  all_sv_data[["PH_READING"]] <- get_PH_READING(sv_xml)
  all_sv_data[["DO_CHECK"]] <- get_DO_CHECK(sv_xml)
  all_sv_data[["DO_READING"]] <- get_DO_READING(sv_xml)
  all_sv_data[["WT_COMPARISON_CHECK"]] <- get_WT_COMPARISON(sv_xml)
  all_sv_data[["WT_MULTIPOINT_CHECK"]] <- get_WT_MULTIPOINT_CHECK(sv_xml)
  all_sv_data[["WT_MULTIPOINT_READING"]] <- get_WT_MULTIPOINT_READING(sv_xml)
  all_sv_data[["CALIBRATION"]] <- get_CALIBRATION(sv_xml)
  
  return(all_sv_data)
  
}

add_keys <- function(all_sv_data, max_keys, source_file) {
  
  source <- data.frame(SOURCE_ID = 1 + max_keys["SOURCE_ID"], SOURCE_NAME = basename(source_file),
                     DATE_LOADED = as.character(Sys.time(), format="%Y-%m-%d %H:%M:%S"))
  all_sv_data[["SOURCE"]] <- source
  
  #Find the date of the first reading
  reading_dates <- c(all_sv_data[["SC_READING"]]$DATETIME, all_sv_data[["TBY_READING"]]$DATETIME,
                     all_sv_data[["PH_READING"]]$DATETIME, all_sv_data[["DO_READING"]]$DATETIME,
                     all_sv_data[["WT_COMPARISON_CHECK"]]$DATETIME,
                     all_sv_data[["WT_MULTIPOINT_READING"]]$DATETIME) %>%
    as.POSIXct()
  if(length(reading_dates) > 0) {
    cal_date <- min(reading_dates) %>% as.character(format="%Y-%m-%d")
  } else {
    cal_date <- "1900-01-01"
  }
  
  all_sv_data[["CALIBRATION"]]$CAL_ID <- 1 + max_keys["CAL_ID"]
  all_sv_data[["CALIBRATION"]]$DATE <- cal_date
  all_sv_data[["CALIBRATION"]]$SOURCE_ID <- source$SOURCE_ID
  calibration <- all_sv_data[["CALIBRATION"]]
  
  #Add keys to the sc check and readings
  if(nrow(all_sv_data[["SC_CHECK"]]) > 0) {
    
    all_sv_data[["SC_CHECK"]]$SC_ID <- 1 + max_keys["SC_ID"]
    all_sv_data[["SC_CHECK"]]$CAL_ID <- calibration$CAL_ID
      
    if(nrow(all_sv_data[["SC_READING"]]) > 0) {
      
      all_sv_data[["SC_READING"]]$SC_ID <- 1 + max_keys["SC_ID"]
      all_sv_data[["SC_READING"]]$SCR_ID <- 1:nrow(all_sv_data[["SC_READING"]])
      all_sv_data[["SC_READING"]]$SCR_ID <- all_sv_data[["SC_READING"]]$SCR_ID + max_keys["SCR_ID"]
      
    } 
    
  } else {
    
    all_sv_data[["SC_CHECK"]]$SC_ID <- vector()
    all_sv_data[["SC_CHECK"]]$CAL_ID <- vector()
    all_sv_data[["SC_READING"]]$SC_ID <- vector()
    all_sv_data[["SC_READING"]]$SCR_ID <- vector()
    
  }
  
  if(nrow(all_sv_data[["TBY_CHECK"]]) > 0) {
    
    all_sv_data[["TBY_CHECK"]]$TBY_ID <- 1 + max_keys["TBY_ID"]
    all_sv_data[["TBY_CHECK"]]$CAL_ID <- calibration$CAL_ID
    
    if(nrow(all_sv_data[["TBY_READING"]]) > 0) {
      
      all_sv_data[["TBY_READING"]]$TBY_ID <- 1 + max_keys["TBY_ID"]
      all_sv_data[["TBY_READING"]]$TBYR_ID <- 1:nrow(all_sv_data[["TBY_READING"]])
      all_sv_data[["TBY_READING"]]$TBYR_ID <- all_sv_data[["TBY_READING"]]$TBYR_ID + max_keys["TBYR_ID"]
      
    }
    
  } else {
    
    all_sv_data[["TBY_CHECK"]]$TBY_ID <- vector()
    all_sv_data[["TBY_CHECK"]]$CAL_ID <- vector()
    all_sv_data[["TBY_READING"]]$TBY_ID <- vector()
    all_sv_data[["TBY_READING"]]$TBYR_ID <- vector()
    
  }
  
  if(nrow(all_sv_data[["DO_CHECK"]]) > 0) {
    
    all_sv_data[["DO_CHECK"]]$DO_ID <- 1 + max_keys["DO_ID"]
    all_sv_data[["DO_CHECK"]]$CAL_ID <- calibration$CAL_ID
    
    if(nrow(all_sv_data[["DO_READING"]]) > 0) {
      
      all_sv_data[["DO_READING"]]$DO_ID <- 1 + max_keys["DO_ID"]
      all_sv_data[["DO_READING"]]$DOR_ID <- 1:nrow(all_sv_data[["DO_READING"]])
      all_sv_data[["DO_READING"]]$DOR_ID <- all_sv_data[["DO_READING"]]$DOR_ID + max_keys["DOR_ID"]
      
    }
    
  } else {
    
    all_sv_data[["DO_CHECK"]]$DO_ID <- vector()
    all_sv_data[["DO_CHECK"]]$CAL_ID <- vector()
    all_sv_data[["DO_READING"]]$DO_ID <- vector()
    all_sv_data[["DO_READING"]]$DOR_ID <- vector()
    
  }
  
  if(nrow(all_sv_data[["PH_CHECK"]]) > 0) {
    
    all_sv_data[["PH_CHECK"]]$PH_ID <- 1 + max_keys["PH_ID"]
    all_sv_data[["PH_CHECK"]]$CAL_ID <- calibration$CAL_ID
    
    if(nrow(all_sv_data[["PH_READING"]]) > 0) {
      
      all_sv_data[["PH_READING"]]$PH_ID <- 1 + max_keys["PH_ID"]
      all_sv_data[["PH_READING"]]$PHR_ID <- 1:nrow(all_sv_data[["PH_READING"]])
      all_sv_data[["PH_READING"]]$PHR_ID <- all_sv_data[["PH_READING"]]$PHR_ID + max_keys["PHR_ID"]
      
    }
    
  } else {
    
    all_sv_data[["PH_CHECK"]]$PH_ID <- vector()
    all_sv_data[["PH_CHECK"]]$CAL_ID <- vector()
    all_sv_data[["PH_READING"]]$PH_ID <- vector()
    all_sv_data[["PH_READING"]]$PHR_ID <- vector()
    
  }
  
  if(nrow(all_sv_data[["WT_MULTIPOINT_CHECK"]]) > 0) {
    all_sv_data[["WT_MULTIPOINT_CHECK"]]$WT_MULTIPOINT_ID <- 
      1 + max_keys["WT_MULTIPOINT_ID"]
    all_sv_data[["WT_MULTIPOINT_CHECK"]]$CAL_ID <- calibration$CAL_ID
    
    if(nrow(all_sv_data[["WT_MULTIPOINT_READING"]]) > 0) {
      
      all_sv_data[["WT_MULTIPOINT_READING"]]$WT_MULTIPOINT_ID <- 
        1 + max_keys["WT_MULTIPOINT_ID"]
      all_sv_data[["WT_MULTIPOINT_READING"]]$WTR_MULTIPOINT_ID <- 
        1:nrow(all_sv_data[["WT_MULTIPOINT_READING"]])
      all_sv_data[["WT_MULTIPOINT_READING"]]$WTR_MULTIPOINT_ID <- 
        all_sv_data[["WT_MULTIPOINT_READING"]]$WTR_MULTIPOINT_ID + max_keys["WTR_MULTIPOINT_ID"]
    } else {
      all_sv_data[["WT_MULTIPOINT_READING"]]$WT_MULTIPOINT_ID <- vector()
      all_sv_data[["WT_MULTIPOINT_READING"]]$WTR_MULTIPOINT_ID <- vector()
    }
    
  } else {
    
    all_sv_data[["WT_MULTIPOINT_CHECK"]]$WT_MULTIPOINT_ID <- vector()
    all_sv_data[["WT_MULTIPOINT_CHECK"]]$CAL_ID <- vector()
    all_sv_data[["WT_MULTIPOINT_READING"]]$WT_MULTIPOINT_ID <- vector()
    all_sv_data[["WT_MULTIPOINT_READING"]]$WTR_MULTIPOINT_ID <- vector()
    
  }
  
  if(nrow(all_sv_data[["WT_COMPARISON_CHECK"]]) > 0) {
    
    all_sv_data[["WT_COMPARISON_CHECK"]]$WT_COMP_ID <- 
      1 + max_keys["WT_COMP_ID"]
    all_sv_data[["WT_COMPARISON_CHECK"]]$CAL_ID <- calibration$CAL_ID
    
  } else {
    
    all_sv_data[["WT_COMPARISON_CHECK"]]$WT_COMP_ID <- vector()
    all_sv_data[["WT_COMPARISON_CHECK"]]$CAL_ID <- vector()
    
  }
  
  return(all_sv_data)
  
}

cal_book_connect <- function(path) {
  
  return(dbConnect(SQLite(), dbname = path))
  
}

get_max_keys <- function(dbcon) {

  source_id <- tbl(dbcon, "SOURCE") %>%
    pull(SOURCE_ID)
  if(length(source_id) > 0) {
    max_source_id <- max(source_id)
  } else {
    max_source_id <- 0
  }
  
  cal_id <- tbl(dbcon, "CALIBRATION") %>%
    pull(CAL_ID) 
  if(length(cal_id) > 0) {
    max_cal_id <- max(cal_id)
  } else {
    max_cal_id <- 0
  }
  
  sc_id <- tbl(dbcon, "SC_CHECK") %>%
    pull(SC_ID)
  if(length(sc_id) > 0) {
    max_sc_id <- max(sc_id)
  } else {
    max_sc_id <- 0
  }
  
  scr_id <- tbl(dbcon, "SC_READING") %>%
    pull(SCR_ID)
  if(length(scr_id) > 0) {
    max_scr_id <- max(scr_id)
  } else {
    max_scr_id <- 0
  }
  
  tby_id <- tbl(dbcon, "TBY_CHECK") %>%
    pull(TBY_ID)
  if(length(tby_id) > 0) {
    max_tby_id <- max(tby_id)
  } else {
    max_tby_id <- 0
  }
  
  tbyr_id <- tbl(dbcon, "TBY_READING") %>%
    pull(TBYR_ID)
  if(length(tby_id) > 0) {
    max_tbyr_id <- max(tbyr_id)
  } else {
    max_tbyr_id <- 0
  }
  
  do_id <- tbl(dbcon, "DO_CHECK") %>%
    pull(DO_ID)
  if(length(do_id) > 0) {
    max_do_id <- max(do_id)
  } else {
    max_do_id <- 0
  }
  
  dor_id <- tbl(dbcon, "DO_READING") %>%
    pull(DOR_ID)
  if(length(dor_id) > 0) {
    max_dor_id <- max(dor_id) 
  } else {
    max_dor_id <- 0
  }

  ph_id <- tbl(dbcon, "PH_CHECK") %>%
    pull(PH_ID)
  if(length(ph_id) > 0) {
    max_ph_id <- max(ph_id)
  } else {
    max_ph_id <- 0
  }
  
  phr_id <- tbl(dbcon, "PH_READING") %>%
    pull(PHR_ID)
  if(length(phr_id) > 0) {
    max_phr_id <- max(phr_id)
  } else {
    max_phr_id <- 0
  }
  
  wt_comp_id <- tbl(dbcon, "WT_COMPARISON_CHECK") %>%
    pull(WT_COMP_ID)
  if(length(wt_comp_id) > 0) {
    max_wt_comp_id <- max(wt_comp_id)
  } else {
    max_wt_comp_id <- 0
  }
  
  wt_multipoint_check_id <- tbl(dbcon, "WT_MULTIPOINT_CHECK") %>%
    pull(WT_MULTIPOINT_ID)
  if(length(wt_comp_id) > 0) {
    max_wt_multipoint_id <- max(wt_multipoint_check_id)
  } else {
    max_wt_multipoint_id <- 0
  }
  
  wt_multipoint_reading_id <- tbl(dbcon, "WT_MULTIPOINT_READING") %>%
    pull(WTR_MULTIPOINT_ID)
  if(length(wt_multipoint_reading_id) > 0) {
    max_wtr_multipoint_id <- max(wt_multipoint_reading_id)
  } else {
    max_wtr_multipoint_id <- 0
  }

  max_keys <- c("CAL_ID" = max_cal_id, "SOURCE_ID" = max_source_id,
                "SC_ID" = max_sc_id, "SCR_ID" = max_scr_id,
                "TBY_ID" = max_tby_id, "TBYR_ID" = max_tbyr_id,
                "DO_ID" = max_do_id, "DOR_ID" = max_dor_id,
                "PH_ID" = max_ph_id, "PHR_ID" = max_phr_id,
                "WT_COMP_ID" = max_wt_comp_id, 
                "WT_MULTIPOINT_ID" = max_wt_multipoint_id,
                "WTR_MULTIPOINT_ID" = max_wtr_multipoint_id)
  
  return(max_keys)
  
}

lookup_sensors <- function(all_sv_data, dbcon) {
  
  sv_sensors <- all_sv_data[["SENSOR"]]
  
  db_sensors <- tbl(dbcon, "SENSOR") %>%
    select(SENSOR_ID, SENSOR_SN, PARAMETER) %>%
    data.frame()
  
  if(nrow(db_sensors) > 0) {
    max_sensor_id <- max(db_sensors$SENSOR_ID)
  } else {
    max_sensor_id <- 0
  }
  
  sensors <- left_join(sv_sensors, db_sensors, by = c("SENSOR_SN", "PARAMETER")) %>%
    mutate(new = is.na(SENSOR_ID))
  
  count_new_sensors <- sum(sensors$new)
  
  if(count_new_sensors > 0)
    sensors$SENSOR_ID[sensors$new] <- 1:count_new_sensors + max_sensor_id
  
  all_sv_data[["SENSOR"]] <- select(sensors, SENSOR_ID, SENSOR_SN, PARAMETER, MANUFACTURER, MODEL, new)
  
  if(nrow(all_sv_data[["SC_CHECK"]]) > 0) {
    
    sc_sensor_sn <- all_sv_data[["SC_CHECK"]]$SENSOR_ID
    sc_sensor_id <- sensors %>%
      filter(SENSOR_SN == sc_sensor_sn, PARAMETER == "Specific cond at 25C") %>%
      pull(SENSOR_ID)
    all_sv_data[["SC_CHECK"]]$SENSOR_ID <- sc_sensor_id
    
  }
  
  if(nrow(all_sv_data[["TBY_CHECK"]]) > 0) {
    
    tby_sensor_sn <- all_sv_data[["TBY_CHECK"]]$SENSOR_ID
    tby_sensor_id <- sensors %>%
      filter(SENSOR_SN == tby_sensor_sn, PARAMETER == "Turbidity, FNU") %>%
      pull(SENSOR_ID)
    all_sv_data[["TBY_CHECK"]]$SENSOR_ID <- tby_sensor_id
    
  }
  
  if(nrow(all_sv_data[["DO_CHECK"]]) > 0) {
    
    do_sensor_sn <- all_sv_data[["DO_CHECK"]]$SENSOR_ID
    do_sensor_id <- sensors %>%
      filter(SENSOR_SN == do_sensor_sn, PARAMETER == "Dissolved oxygen") %>%
      pull(SENSOR_ID)
    all_sv_data[["DO_CHECK"]]$SENSOR_ID <- do_sensor_id
    
  }
  
  if(nrow(all_sv_data[["PH_CHECK"]]) > 0) {
    
    ph_sensor_sn <- all_sv_data[["PH_CHECK"]]$SENSOR_ID
    ph_sensor_id <- sensors %>%
      filter(SENSOR_SN == ph_sensor_sn, PARAMETER == "pH") %>%
      pull(SENSOR_ID)
    all_sv_data[["PH_CHECK"]]$SENSOR_ID <- ph_sensor_id
    
  }
  
  if(nrow(all_sv_data[["WT_MULTIPOINT_CHECK"]]) > 0) {
    
    wt_sensor_sn <- all_sv_data[["WT_MULTIPOINT_CHECK"]]$SENSOR_ID
    wt_sensor_id <- sensors %>%
      filter(SENSOR_SN == wt_sensor_sn, PARAMETER == "Temperature, water") %>%
      pull(SENSOR_ID)
    all_sv_data[["WT_MULTIPOINT_CHECK"]]$SENSOR_ID <- wt_sensor_id
    
  }
  
  if(nrow(all_sv_data[["WT_COMPARISON_CHECK"]]) > 0) {
    
    wt_sensor_sn <- all_sv_data[["WT_COMPARISON_CHECK"]]$SENSOR_ID
    wt_sensor_id <- sensors %>%
      filter(SENSOR_SN == wt_sensor_sn, PARAMETER == "Temperature, water") %>%
      pull(SENSOR_ID)
    all_sv_data[["WT_COMPARISON_CHECK"]]$SENSOR_ID <- wt_sensor_id
    
  }
  
  return(all_sv_data)
  
}

write_sv_data <- function(all_sv_data, dbcon) {
  
  print("writing")
  print(all_sv_data)
  write <- list()
  
  write[["SENSOR"]] <- all_sv_data[["SENSOR"]] %>%
    filter(new) %>%
    select(SENSOR_ID, SENSOR_SN, PARAMETER, MANUFACTURER, MODEL)

  write[["SOURCE"]] <- all_sv_data[["SOURCE"]] %>%
    select(SOURCE_ID, SOURCE_NAME, DATE_LOADED)

  write[["CALIBRATION"]] <- all_sv_data[["CALIBRATION"]] %>%
    select(CAL_ID, DATE, MONITOR_SN, CAL_TYPE, SOURCE_ID)

  write[["SC_CHECK"]] <- all_sv_data[["SC_CHECK"]] %>%
    select(SC_ID, CAL_ID, SENSOR_ID, CELL_CONSTANT, AIR_READING, COMMENT)

  write[["SC_READING"]] <- all_sv_data[["SC_READING"]] %>%
    select(SCR_ID, SC_ID, STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING,
           TEMPERATURE, DATETIME, TYPE)

  write[["TBY_CHECK"]] <- all_sv_data[["TBY_CHECK"]] %>%
    select(TBY_ID, CAL_ID, SENSOR_ID, SENSOR_LIMIT, COMMENT)

  write[["TBY_READING"]] <- all_sv_data[["TBY_READING"]] %>%
    select(TBYR_ID, TBY_ID, STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING,
           TEMPERATURE, DATETIME, TYPE)

  write[["DO_CHECK"]] <- all_sv_data[["DO_CHECK"]] %>%
    select(DO_ID, CAL_ID, SENSOR_ID, SC_AIR_SATURATED_WATER, TEMP_AIR_SATURATED_WATER,
           SALINITY, DATE_BAROMETER_CALIBRATED, ODO_GAIN_PRE, ODO_CAP_CHANGED, ODO_CAP_SN,
           ODO_GAIN_POST, COMMENT)

  write[["DO_READING"]] <- all_sv_data[["DO_READING"]] %>%
    select(DOR_ID, DO_ID, TEMPERATURE, PRESSURE, SALINITY_CORRECTION, DO_TABLE_VALUE, READING,
           DATETIME, ZERO_READING, TYPE)

  write[["PH_CHECK"]] <- all_sv_data[["PH_CHECK"]] %>%
    select(PH_ID, CAL_ID, SENSOR_ID, COMMENT)

  write[["PH_READING"]] <- all_sv_data[["PH_READING"]] %>%
    select(PHR_ID, PH_ID, STD_UNCORRECTED, STD_TYPE, STD_LOT, TEMPERATURE, STD_VALUE, READING,
           DATETIME, MILLIVOLTS, TYPE)
  
  write[["WT_COMPARISON_CHECK"]] <- all_sv_data[["WT_COMPARISON_CHECK"]] %>%
    select(WT_COMP_ID, CAL_ID, SENSOR_ID, FIELD_SENSOR_SN, DATETIME, CHECK_MEASURE,
           SENSOR_MEASURE, TWO_POINT_CHECK_DATE, FIVE_POINT_CHECK_DATE, COMMENT)
  
  write[["WT_MULTIPOINT_CHECK"]] <- all_sv_data[["WT_MULTIPOINT_CHECK"]] %>%
    select(WT_MULTIPOINT_ID, SENSOR_ID, CAL_ID, NIST_CERT_DATE, NIST_SN, COMMENT)
  
  write[["WT_MULTIPOINT_READING"]] <- all_sv_data[["WT_MULTIPOINT_READING"]] %>%
    select(WTR_MULTIPOINT_ID, WT_MULTIPOINT_ID, NIST_READING, MONITOR_READING, DATETIME)

  tables <- c("SENSOR", "SOURCE","CALIBRATION", "SC_CHECK", "SC_READING", "TBY_CHECK", "TBY_READING",
              "DO_CHECK", "DO_READING", "PH_CHECK", "PH_READING", "WT_COMPARISON_CHECK",
              "WT_MULTIPOINT_CHECK", "WT_MULTIPOINT_READING")
  
  for(i in tables) {

    if(nrow(write[[i]]) > 0) {
      
      dbWriteTable(dbcon, i, write[[i]], append=TRUE)
      
    }
    
  }
  
}

combine_manual <- function(monitor_sn, sc_list, tby_list, do_list, ph_list, wt_list) {
  
  #Make a sensor data frame
  sc_check <- sc_list[["SC_CHECK"]]
  tby_check <- tby_list[["TBY_CHECK"]]
  do_check <- do_list[["DO_CHECK"]]
  ph_check <- ph_list[["PH_CHECK"]]
  wt_comp_check <- wt_list[["WT_COMPARISON_CHECK"]]
  wt_multi_check <- wt_list[["WT_MULTIPOINT_CHECK"]]
  
  SENSOR_SN <- vector()
  PARAMETER <- vector()
  MANUFACTURER <- vector()
  MODEL <- vector()
  if(nrow(sc_check) != 0) {
    SENSOR_SN[length(SENSOR_SN) + 1] <- sc_check$SENSOR_ID
    PARAMETER[length(PARAMETER) + 1] <- "Specific cond at 25C"
    MANUFACTURER[length(MANUFACTURER) + 1] <- ""
    MODEL[length(MODEL) + 1] <- ""
  }
  if(nrow(tby_check) != 0) {
    SENSOR_SN[length(SENSOR_SN) + 1] <- tby_check$SENSOR_ID
    PARAMETER[length(PARAMETER) + 1] <- "Turbidity, FNU"
    MANUFACTURER[length(MANUFACTURER) + 1] <- ""
    MODEL[length(MODEL) + 1] <- ""
  }
  if(nrow(ph_check) != 0) {
    SENSOR_SN[length(SENSOR_SN) + 1] <- ph_check$SENSOR_ID
    PARAMETER[length(PARAMETER) + 1] <- "pH"
    MANUFACTURER[length(MANUFACTURER) + 1] <- ""
    MODEL[length(MODEL) + 1] <- ""
  }
  if(nrow(do_check) != 0) {
    SENSOR_SN[length(SENSOR_SN) + 1] <- do_check$SENSOR_ID
    PARAMETER[length(PARAMETER) + 1] <- "Dissolved oxygen"
    MANUFACTURER[length(MANUFACTURER) + 1] <- ""
    MODEL[length(MODEL) + 1] <- ""
  }
  if(nrow(wt_multi_check) != 0) {
    SENSOR_SN[length(SENSOR_SN) + 1] <- wt_multi_check$SENSOR_ID
    PARAMETER[length(PARAMETER) + 1] <- "Temperature, water"
    MANUFACTURER[length(MANUFACTURER) + 1] <- ""
    MODEL[length(MODEL) + 1] <- ""
  }
  if(nrow(wt_comp_check) != 0) {
    SENSOR_SN[length(SENSOR_SN) + 1] <- wt_comp_check$SENSOR_ID
    PARAMETER[length(PARAMETER) + 1] <- "Temperature, water"
    MANUFACTURER[length(MANUFACTURER) + 1] <- ""
    MODEL[length(MODEL) + 1] <- ""
  }

  sensor_df <- data.frame(SENSOR_SN, PARAMETER, MANUFACTURER, MODEL)
  sensor_df <- unique(sensor_df)

  all_sv_data <- list()
  all_sv_data[["SENSOR"]] <- sensor_df
  all_sv_data[["SC_CHECK"]] <- sc_list[["SC_CHECK"]]
  all_sv_data[["SC_READING"]] <- sc_list[["SC_READING"]]
  all_sv_data[["TBY_CHECK"]] <- tby_list[["TBY_CHECK"]]
  all_sv_data[["TBY_READING"]] <- tby_list[["TBY_READING"]]
  all_sv_data[["PH_CHECK"]] <- ph_list[["PH_CHECK"]]
  all_sv_data[["PH_READING"]] <- ph_list[["PH_READING"]]
  all_sv_data[["DO_CHECK"]] <- do_list[["DO_CHECK"]]
  all_sv_data[["DO_READING"]] <- do_list[["DO_READING"]]
  all_sv_data[["WT_COMPARISON_CHECK"]] <- wt_list[["WT_COMPARISON_CHECK"]]
  all_sv_data[["WT_MULTIPOINT_CHECK"]] <- wt_list[["WT_MULTIPOINT_CHECK"]]
  all_sv_data[["WT_MULTIPOINT_READING"]] <- wt_list[["WT_MULTIPOINT_READING"]]
  all_sv_data[["CALIBRATION"]] <- data.frame(MONITOR_SN = monitor_sn,
                                          CAL_TYPE = "MANUAL")
  
  return(all_sv_data)
  
  
}

delete_everything <- function(dbcon) {
  
  tables <- c("PH_READING", "PH_CHECK", "DO_READING", "DO_CHECK", "TBY_READING", "TBY_CHECK", "SC_READING", "SC_CHECK",
              "WT_MULTIPOINT_READING", "WT_MULTIPOINT_CHECK", "WT_COMPARISON_CHECK","CALIBRATION", "SOURCE", "SENSOR")
  
  for(i in tables) {
    statement <- paste("DELETE FROM", i)
    print(statement)
    dbExecute(dbcon, statement)
  }
  
}

empty_if_null <- function(x) {
  
  if(is.null(x)) {
    x <- ""
  }
  
  return(x)
  
}

get_cal_list <- function(parameter, serial_number, dbcon) {
  #Find the basetable
  if(parameter == "Specific cond at 25C") {
    basetables <- c("SC_CHECK", "SC_READING", "SC_ID")
  } else if (parameter == "Turbidity, FNU") {
    basetables <- c("TBY_CHECK", "TBY_READING", "TBY_ID")
  } else if (parameter == "Dissolved oxygen") {
    basetables <- c("DO_CHECK", "DO_READING", "DO_ID")
  } else if(parameter == "pH") {
    basetables <- c("PH_CHECK", "PH_READING", "PH_ID")
  } else if(parameter == "Temperature, water (comparison)") {
    basetables <- c("WT_COMPARISON_CHECK", "")
  } else if(parameter == "Temperature, water (multi-point)") {
    basetables <- c("WT_MULTIPOINT_CHECK", "WT_MULTIPOINT_READING", "WT_MULTIPOINT_ID")
  } else {
    basetables <- c("GEN_CHECK", "GEN_READING")
  }
  
  basetable <- basetables[1]
  
  sensor_id <- tbl(dbcon, "SENSOR") %>%
    filter(SENSOR_SN == serial_number & PARAMETER == parameter) %>%
    pull(SENSOR_ID)
  
  cal <- tbl(dbcon, "CALIBRATION") %>%
    select(CAL_ID, DATE)
  check <- tbl(dbcon, basetable) %>%
    inner_join(cal) %>%
    filter(SENSOR_ID == sensor_id) %>%
    arrange(DATE) %>%
    collect()
  reading <- tbl(dbcon, basetables[2]) %>%
    collect()
  
  cal_list <- list()
  
  for(i in check[,basetables[3],drop=TRUE]) {
    
    this_check <- list()
    this_check[[1]] <- check %>%
      filter(.data[[basetables[3]]] == i) %>%
      select(-ends_with("_ID"))
    this_check[[2]] <- reading %>%
      filter(.data[[basetables[3]]] == i) %>%
      select(-ends_with("_ID"))
    
    cal_list[[length(cal_list) + 1]] <- this_check
    
  }
  
  return(cal_list)
}

