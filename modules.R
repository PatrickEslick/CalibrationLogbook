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