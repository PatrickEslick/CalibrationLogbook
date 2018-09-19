##########################################################################################################################################################
#
# SC
# Module for getting specific conductance calibration data
#
##########################################################################################################################################################


manualScInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textInput(ns("sc_sensor_sn"), "Sensor serial number", placeholder = "12A34567"),
    fluidRow(
      column(2,
             textInput(ns("sc_cell_constant"), "Cell constant")
      ),
      column(2,
             textInput(ns("sc_air_reading"), "Reading in air")
      )
    ),
    textInput(ns("sc_comment"), "Comment", width = "65%"),
    tabsetPanel(
      tabPanel("Calibration",
        sliderInput(ns("reading_count_before"), label = "Number of readings", min = 1, max = 5, value = 1, step = 1),
        uiOutput(ns("sc_reading_before_ui"))
      ),
      tabPanel("Post-calibration",
        sliderInput(ns("reading_count_after"), label = "Number of readings", min = 1, max = 5, value = 1, step = 1),
        uiOutput(ns("sc_reading_after_ui"))
      )
    )
  )
  
}

manualSc <- function(input, output, session) {
  
  output$sc_reading_before_ui <- renderUI({
    
    ns <- session$ns
    
    lapply(1:input$reading_count_before, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("b_std_value", i)), label = "Std. value")),
        column(1, textInput(ns(paste0("b_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd")),
        column(1, selectInput(ns(paste0("b_std_type", i)), label = "Std. type",
                              choices = c("KCl", "DI", "Other"))),
        column(1, textInput(ns(paste0("b_std_lot", i)), label = "Std. lot")),
        column(1, textInput(ns(paste0("b_reading", i)), label = "Reading")),
        column(1, textInput(ns(paste0("b_temperature", i)), label = "Temperature")),
        column(1, textInput(ns(paste0("b_datetime", i)), label = "Date/Time",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M")))
      )
    })
  })
  
  output$sc_reading_after_ui <- renderUI({
    
    ns <- session$ns
    
    lapply(1:input$reading_count_after, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("a_std_value", i)), label = "Std. value")),
        column(1, textInput(ns(paste0("a_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd")),
        column(1, selectInput(ns(paste0("a_std_type", i)), label = "Std. type",
                              choices = c("KCl", "DI water", "Other"))),
        column(1, textInput(ns(paste0("a_std_lot", i)), label = "Std. lot")),
        column(1, textInput(ns(paste0("a_reading", i)), label = "Reading")),
        column(1, textInput(ns(paste0("a_temperature", i)), label = "Temperature")),
        column(1, textInput(ns(paste0("a_datetime", i)), label = "Date/Time",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M")))
      )
    })
  })
  
  
  
  sc_check_list <- reactive({
    
    
    if(!is.null(input$sc_sensor_sn)) {
      
      #Get data for SC_CHECK table
      
      SENSOR_ID <- input$sc_sensor_sn
      CELL_CONSTANT <- empty_if_null(input$sc_cell_constant)
      AIR_READING <- empty_if_null(input$sc_air_reading)
      COMMENT <- empty_if_null(input$sc_comment)
      
      sc_check_df <- data.frame(SENSOR_ID, CELL_CONSTANT, AIR_READING, COMMENT,
                                stringsAsFactors = FALSE)
      
    } else {
      
      sc_check_df <- data.frame(SENSOR_ID = vector(), CELL_CONSTANT = vector(),
                                AIR_READING = vector(), COMMENT = vector())
      
    }
    
    #Get data for SC_READING table
    
    STD_VALUE <- vector()
    STD_EXPIRATION <- vector()
    STD_TYPE <- vector()
    STD_LOT <- vector()
    READING <- vector()
    TEMPERATURE <- vector()
    DATETIME <- vector()
    TYPE <- vector()
    
    # Get the before data
    for(i in 1:input$reading_count_before) {
      
      if(!is.null(input[[paste0("b_std_value", i)]])) {
        if(input[[paste0("b_std_value", i)]] != "") {
          STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("b_std_value", i)]]
          STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("b_std_expiration", i)]])
          STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("b_std_type", i)]])
          STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("b_std_lot", i)]])
          READING[length(READING) + 1] <- empty_if_null(input[[paste0("b_reading", i)]])
          TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("b_temperature", i)]])
          DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("b_reading", i)]])
          TYPE[length(TYPE) + 1] <- "CALI"
        }
      }
      
    }
    
    #Get the after data
    for(i in 1:input$reading_count_after) {
      
      if(!is.null(input[[paste0("a_std_value", i)]])) {
        if(input[[paste0("a_std_value", i)]] != "") {
          STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("a_std_value", i)]]
          STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("a_std_expiration", i)]])
          STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("a_std_type", i)]])
          STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("a_std_lot", i)]])
          READING[length(READING) + 1] <- empty_if_null(input[[paste0("a_reading", i)]])
          TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("a_temperature", i)]])
          DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("a_reading", i)]])
          TYPE[length(TYPE) + 1] <- "RECL"
        }
      }
      
    }
    
    sc_reading_df <- data.frame(STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING, TEMPERATURE, 
                                DATETIME, TYPE)
    
    list_out <- list(SC_CHECK = sc_check_df, SC_READING = sc_reading_df)
    
  })
  
  return(sc_check_list)
  
}

##########################################################################################################################################################
#
# TBY
# Module for getting turbidity calibration data
#
##########################################################################################################################################################

manualTbyInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textInput(ns("tby_sensor_sn"), "Sensor serial number", placeholder = "12A34567"),
    fluidRow(
      column(2,
        textInput(ns("tby_sensor_limit"), "Sensor limit")
      )
    ),
    textInput(ns("tby_comment"), "Comment", width = "65%"),
    tabsetPanel(
      tabPanel("Calibration",
               sliderInput(ns("reading_count_before"), label = "Number of readings", min = 1, max = 5, value = 1, step = 1),
               uiOutput(ns("tby_reading_before_ui"))
      ),
      tabPanel("Post-calibration",
               sliderInput(ns("reading_count_after"), label = "Number of readings", min = 1, max = 5, value = 1, step = 1),
               uiOutput(ns("tby_reading_after_ui"))
      )
    )
  )
  
}

manualTby <- function(input, output, session) {
  
  output$tby_reading_before_ui <- renderUI({
    
    ns <- session$ns
    
    lapply(1:input$reading_count_before, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("b_std_value", i)), label = "Std. value")),
        column(1, textInput(ns(paste0("b_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd")),
        column(1, selectInput(ns(paste0("b_std_type", i)), label = "Std. type",
                              choices = c("Stablcal", "Formazin", "DI water"))),
        column(1, textInput(ns(paste0("b_std_lot", i)), label = "Std. lot")),
        column(1, textInput(ns(paste0("b_reading", i)), label = "Reading")),
        column(1, textInput(ns(paste0("b_temperature", i)), label = "Temperature")),
        column(1, textInput(ns(paste0("b_datetime", i)), label = "Date/Time",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M")))
      )
    })
  })
  
  output$tby_reading_after_ui <- renderUI({
    
    ns <- session$ns
    
    lapply(1:input$reading_count_after, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("a_std_value", i)), label = "Std. value")),
        column(1, textInput(ns(paste0("a_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd")),
        column(1, selectInput(ns(paste0("a_std_type", i)), label = "Std. type",
                              choices = c("Stablcal", "Formazin", "DI water"))),
        column(1, textInput(ns(paste0("a_std_lot", i)), label = "Std. lot")),
        column(1, textInput(ns(paste0("a_reading", i)), label = "Reading")),
        column(1, textInput(ns(paste0("a_temperature", i)), label = "Temperature")),
        column(1, textInput(ns(paste0("a_datetime", i)), label = "Date/Time",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M")))
      )
    })
    
  })
  
  
  
  tby_check_list <- reactive({
    
    
    if(!is.null(input$tby_sensor_sn)) {
      
      #Get data for tby_CHECK table
      
      SENSOR_ID <- input$tby_sensor_sn
      SENSOR_LIMIT <- input$tby_sensor_limit
      COMMENT <- empty_if_null(input$tby_comment)
      
      tby_check_df <- data.frame(SENSOR_ID, SENSOR_LIMIT, COMMENT,
                                stringsAsFactors = FALSE)
      
    } else {
      
      tby_check_df <- data.frame(SENSOR_ID = vector(), SENSOR_LIMIT = vector(),
                                COMMENT = vector())
      
    }
    
    #Get data for tby_READING table
    
    STD_VALUE <- vector()
    STD_EXPIRATION <- vector()
    STD_TYPE <- vector()
    STD_LOT <- vector()
    READING <- vector()
    TEMPERATURE <- vector()
    DATETIME <- vector()
    TYPE <- vector()
    
    # Get the before data
    for(i in 1:input$reading_count_before) {
      
      if(!is.null(input[[paste0("b_std_value", i)]])) {
        if(input[[paste0("b_std_value", i)]] != "") {
          STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("b_std_value", i)]]
          STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("b_std_expiration", i)]])
          STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("b_std_type", i)]])
          STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("b_std_lot", i)]])
          READING[length(READING) + 1] <- empty_if_null(input[[paste0("b_reading", i)]])
          TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("b_temperature", i)]])
          DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("b_reading", i)]])
          TYPE[length(TYPE) + 1] <- "CALI"
        }
      }
      
    }
    
    #Get the after data
    for(i in 1:input$reading_count_after) {
      
      if(!is.null(input[[paste0("a_std_value", i)]])) {
        if(input[[paste0("a_std_value", i)]] != "") {
          STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("a_std_value", i)]]
          STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("a_std_expiration", i)]])
          STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("a_std_type", i)]])
          STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("a_std_lot", i)]])
          READING[length(READING) + 1] <- empty_if_null(input[[paste0("a_reading", i)]])
          TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("a_temperature", i)]])
          DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("a_reading", i)]])
          TYPE[length(TYPE) + 1] <- "RECL"
        }
      }
      
    }
    
    tby_reading_df <- data.frame(STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING, TEMPERATURE, 
                                DATETIME, TYPE)
    
    list_out <- list(TBY_CHECK = tby_check_df, TBY_READING = tby_reading_df)
    
  })
  
  return(tby_check_list)
  
}

##########################################################################################################################################################
#
# DO
# Module for getting dissolved oxygen calibration data
#
##########################################################################################################################################################


manualDoInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textInput(ns("do_sensor_sn"), "Sensor serial number", placeholder = "12A34567"),
    fluidRow(
      column(3,
        textInput(ns("do_sc_air_saturated_water"), "SC of air saturated water")
      ),
      column(3,
        textInput(ns("do_temp_air_saturated_water"), "Temperature of air saturated water")       
      ),
      column(3,
        textInput(ns("do_salinity"), "Salinity")       
      )
    ),
    fluidRow(
      column(2,
        textInput(ns("do_odo_gain_pre"), "ODO gain")
      ),
      column(2,
        textInput(ns("do_odo_gain_post"), "Post-calibration ODO gain")       
      ),
      column(2,
        checkboxInput(ns("do_odo_cap_changed"), "ODO cap changed")       
      ),
      column(2,
        textInput(ns("do_odo_cap_sn"), "ODO cap serial")       
      )
    ),
    textInput(ns("do_date_barometer_calibrated"), "Date barometer calibrated"),
    textInput(ns("do_comment"), "Comment", width = "65%"),
    tabsetPanel(
      tabPanel("Calibration",
        column(1, textInput(ns("b_temperature"), "Temperature (C)")),
        column(1, textInput(ns("b_pressure"), "Pressure (mmHg)")),
        column(1, textInput(ns("b_salinity_correction"), "Salinity corection", value = "1")),
        column(1, textInput(ns("b_do_table_value"), "DO table value")),
        column(1, textInput(ns("b_reading"), "Reading")),
        column(1, textInput(ns("b_datetime"), "Datetime",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M"))),
        column(1, textInput(ns("b_zero_reading"), "Reading in zero soln."))
      ),
      tabPanel("Post-calibration",
        column(1, textInput(ns("a_temperature"), "Temperature (C)")),
        column(1, textInput(ns("a_pressure"), "Pressure (mmHg)")),
        column(1, textInput(ns("a_salinity_correction"), "Salinity corection", value = "1")),
        column(1, textInput(ns("a_do_table_value"), "DO table value")),
        column(1, textInput(ns("a_reading"), "Reading")),
        column(1, textInput(ns("a_datetime"), "Datetime",
                           value = as.character(Sys.time(), format="%Y-%m-%d %H:%M"))),
        column(1, textInput(ns("a_zero_reading"), "Reading in zero soln."))        
      )
    )
  )
  
}

manualDo <- function(input, output, session) {
  
  do_check_list <- reactive({
  
    if(!is.null(input$do_sensor_sn)) {
      
      #Get data for do_CHECK table
      
      SENSOR_ID <- input$do_sensor_sn
      SC_AIR_SATURATED_WATER <- input$do_sc_air_saturated_water
      TEMP_AIR_SATURATED_WATER <- input$do_temp_air_saturated_water
      SALINITY <- input$do_salinity
      DATE_BAROMETER_CALIBRATED <- input$do_date_barometer_calibrated
      ODO_GAIN_PRE <- input$do_odo_gain_pre
      ODO_CAP_CHANGED <- input$do_odo_cap_changed
      ODO_CAP_SN <- input$do_odo_cap_sn
      ODO_GAIN_POST <- input$do_odo_gain_post
      COMMENT <- input$do_comment
      
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
    
    #Get data for do_READING table
    
    TEMPERATURE <- vector()
    PRESSURE <- vector()
    SALINITY_CORRECTION <- vector()
    DO_TABLE_VALUE <- vector()
    READING <- vector()
    DATETIME <- vector()
    ZERO_READING <- vector()
    TYPE <- vector()
    
    # Get the before data
      
    if(!is.null(input$b_do_table_value)) {
      if(input$b_do_table_value != "") {
        TEMPERATURE[length(TEMPERATURE) + 1] <- input$b_temperature
        PRESSURE[length(PRESSURE) + 1] <- input$b_pressure
        SALINITY_CORRECTION[length(SALINITY_CORRECTION) + 1] <- input$b_salinity_correction
        DO_TABLE_VALUE[length(DO_TABLE_VALUE) + 1] <- input$b_do_table_value
        READING[length(READING) + 1] <- input$b_reading
        DATETIME[length(DATETIME) + 1] <- input$b_datetime
        ZERO_READING[length(ZERO_READING) + 1] <- input$b_zero_reading
        TYPE[length(TYPE) + 1] <- "CALI"
      }
    }
    
    
    #Get the after data
    
    if(!is.null(input$a_do_table_value)) {
      if(input$a_do_table_value != "") {
        TEMPERATURE[length(TEMPERATURE) + 1] <- input$a_temperature
        PRESSURE[length(PRESSURE) + 1] <- input$a_pressure
        SALINITY_CORRECTION[length(SALINITY_CORRECTION) + 1] <- input$a_salinity_correction
        DO_TABLE_VALUE[length(DO_TABLE_VALUE) + 1] <- input$a_do_table_value
        READING[length(READING) + 1] <- input$a_reading
        DATETIME[length(DATETIME) + 1] <- input$a_datetime
        ZERO_READING[length(ZERO_READING) + 1] <- input$a_zero_reading
        TYPE[length(TYPE) + 1] <- "RECL"
      }
    }
    
    
    do_reading_df <- data.frame(TEMPERATURE, PRESSURE, SALINITY_CORRECTION, DO_TABLE_VALUE, READING, DATETIME, 
                                ZERO_READING, TYPE)
    
    list_out <- list(DO_CHECK = do_check_df, DO_READING = do_reading_df)
  
  })

return(do_check_list)
  
}


