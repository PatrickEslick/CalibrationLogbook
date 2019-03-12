##########################################################################################################################################################
#
# SC
# Module for getting specific conductance calibration data
#
##########################################################################################################################################################


manualScInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(2, uiOutput(ns("sensor_sn_ui")))
    ),
    fluidRow(
      column(2,
        uiOutput(ns("cell_constant_ui"))
      ),
      column(2,
        uiOutput(ns("air_reading_ui"))
      )
    ),
    uiOutput(ns("comment_ui")),
    tabsetPanel(
      tabPanel("Calibration",
        fluidRow(
          column(2, uiOutput(ns("before_slider_ui")))
        ),
        uiOutput(ns("sc_reading_before_ui"))
      ),
      tabPanel("Post-calibration",
        fluidRow(
          column(2, uiOutput(ns("after_slider_ui")))
        ),
        uiOutput(ns("sc_reading_after_ui"))
      )
    )
  )
  
}

manualSc <- function(input, output, session, sn = NULL, selected_check = NULL, selected_readings = NULL) {
  
  message("SC CALLED")
  
  output$sensor_sn_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(check)) {
      val <- ""
    } else {
      val <- sn()
    }
    
    textInput(ns("sc_sensor_sn"), "Sensor serial number", placeholder = "12A34567", value = val)
    
  })
  
  outputOptions(output, "sensor_sn_ui", suspendWhenHidden = FALSE)
  
  output$cell_constant_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(check)) {
      val <- ""
    } else {
      val <- check$CELL_CONSTANT
    }
    
    textInput(ns("sc_cell_constant"), label = "Reading in air", value = val)
    
  })
  
  outputOptions(output, "cell_constant_ui", suspendWhenHidden = FALSE)
  
  output$air_reading_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()

    if(is.null(check)) {
      val <- ""
    } else {
      val <- check$AIR_READING
    }
    
    textInput(ns("sc_air_reading"), label = "Cell constant", value = val)
    
  })
  
  outputOptions(output, "air_reading_ui", suspendWhenHidden = FALSE)
  
  output$comment_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(check)) {
      val <- ""
    } else {
      val <- check$COMMENT
    }
    
    textInput(ns("sc_comment"), "Comment", width = "65%", val = val)
    
  })
  
  outputOptions(output, "comment_ui", suspendWhenHidden = FALSE)
  
  output$before_slider_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(readings)) {
      val <- 1
    } else {
      val <- nrow(readings[readings$TYPE == "CALI",])
      if(val == 0)
        val <- 1
    }
    
    sliderInput(ns("reading_count_before"), label = "Number of readings", min = 1, max = 5, value = val, step = 1)
    
  })
  
  outputOptions(output, "before_slider_ui", suspendWhenHidden = FALSE)
  
  output$after_slider_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(readings)) {
      val <- 1
    } else {
      val <- nrow(readings[readings$TYPE == "RECL",])
      if(val == 0)
        val <- 1
    }
    
    sliderInput(ns("reading_count_after"), label = "Number of readings", min = 1, max = 5, value = val, step = 1)
    
  })
  
  outputOptions(output, "after_slider_ui", suspendWhenHidden = FALSE)
  
  output$sc_reading_before_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
  
    if(is.null(readings)) {
      before <- data.frame(
        STD_VALUE = rep("", input$reading_count_before),
        STD_EXPIRATION = rep("", input$reading_count_before),
        STD_TYPE = rep("KCl", input$reading_count_before),
        STD_LOT = rep("", input$reading_count_before),
        READING = rep("", input$reading_count_before),
        TEMPERATURE = rep("", input$reading_count_before),
        DATETIME = rep(as.character(Sys.time(), format = "%Y-%m-%d %H:%M"), input$reading_count_before)
      )
    } else {
      before <- readings %>%
        filter(TYPE == "CALI")
    }

    lapply(1:input$reading_count_before, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("b_std_value", i)), label = "Std. value",
                            value = before$STD_VALUE[i])),
        column(1, textInput(ns(paste0("b_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd",
                            value = before$STD_EXPIRATION[i])),
        column(1, selectInput(ns(paste0("b_std_type", i)), label = "Std. type",
                              choices = c("KCl", "DI", "Other"),
                              selected = before$STD_TYPE[i])),
        column(1, textInput(ns(paste0("b_std_lot", i)), label = "Std. lot",
                            value = before$STD_LOT[i])),
        column(1, textInput(ns(paste0("b_reading", i)), label = "Reading",
                            value = before$READING[i])),
        column(1, textInput(ns(paste0("b_temperature", i)), label = "Temperature",
                            value = before$TEMPERATURE[i])),
        column(1, textInput(ns(paste0("b_datetime", i)), label = "Date/Time",
                            value = before$DATETIME[i]))
      )
    })
  })
  outputOptions(output, "sc_reading_before_ui", suspendWhenHidden = FALSE)
  
  output$sc_reading_after_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(readings)) {
      after <- data.frame(
        STD_VALUE = rep("", input$reading_count_after),
        STD_EXPIRATION = rep("", input$reading_count_after),
        STD_TYPE = rep("KCl", input$reading_count_after),
        STD_LOT = rep("", input$reading_count_after),
        READING = rep("", input$reading_count_after),
        TEMPERATURE = rep("", input$reading_count_after),
        DATETIME = rep(as.character(Sys.time(), format = "%Y-%m-%d %H:%M"), input$reading_count_after),
        USED_FOR_RECAL = rep(FALSE, input$reading_count_after)
      )
    } else {
      after <- readings %>%
        filter(TYPE == "RECL")
      after$USED_FOR_RECAL[is.na(after$USED_FOR_RECAL)] <- FALSE
    }
    
    lapply(1:input$reading_count_after, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("a_std_value", i)), label = "Std. value",
                            value = after$STD_VALUE[i])),
        column(1, textInput(ns(paste0("a_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd",
                            value = after$STD_EXPIRATION[i])),
        column(1, selectInput(ns(paste0("a_std_type", i)), label = "Std. type",
                              choices = c("KCl", "DI water", "Other"),
                              selected = after$STD_TYPE[i])),
        column(1, textInput(ns(paste0("a_std_lot", i)), label = "Std. lot",
                            value = after$STD_LOT[i])),
        column(1, textInput(ns(paste0("a_reading", i)), label = "Reading",
                            value = after$READING[i])),
        column(1, textInput(ns(paste0("a_temperature", i)), label = "Temperature",
                            value = after$TEMPERATURE[i])),
        column(1, textInput(ns(paste0("a_datetime", i)), label = "Date/Time",
                            value = after$DATETIME[i])),
        column(1, checkboxInput(ns(paste0("a_used", i)), label = "Used for recal",
                                value = false_if_null(after$USED_FOR_RECAL[i])))
      )
    })
  })
  outputOptions(output, "sc_reading_after_ui", suspendWhenHidden = FALSE)
  
  sc_check_list <- reactive({
    
    readings <- selected_readings()
    check <- selected_check()
    if(input$sc_sensor_sn != "") {
      
      #Get data for SC_CHECK table
      
      SENSOR_ID <- input$sc_sensor_sn
      CELL_CONSTANT <- empty_if_null(input$sc_cell_constant)
      AIR_READING <- empty_if_null(input$sc_air_reading)
      COMMENT <- empty_if_null(input$sc_comment)
      
      if(!is.null(check)) {
        SC_ID = check$SC_ID[1]
        CAL_ID = check$CAL_ID[1]
        sc_check_df <- data.frame(SC_ID, CAL_ID, SENSOR_ID, CELL_CONSTANT, AIR_READING, COMMENT,
                                  stringsAsFactors = FALSE)
      } else {
        
        sc_check_df <- data.frame(SENSOR_ID, CELL_CONSTANT, AIR_READING, COMMENT,
                                  stringsAsFactors = FALSE)
      }
      
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
    USED_FOR_RECAL <- vector()
    
    # Get the before data
    if(!is.null(input$reading_count_before)) {
      for(i in 1:input$reading_count_before) {
        
        if(!is.null(input[[paste0("b_std_value", i)]])) {
          if(input[[paste0("b_std_value", i)]] != "") {
            STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("b_std_value", i)]]
            STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("b_std_expiration", i)]])
            STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("b_std_type", i)]])
            STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("b_std_lot", i)]])
            READING[length(READING) + 1] <- empty_if_null(input[[paste0("b_reading", i)]])
            TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("b_temperature", i)]])
            DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("b_datetime", i)]])
            TYPE[length(TYPE) + 1] <- "CALI"
            USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- FALSE
          }
        }
      }
    }
    
    #Get the after data
    if(!is.null(input$reading_count_after)) {
      for(i in 1:input$reading_count_after) {
        
        if(!is.null(input[[paste0("a_std_value", i)]])) {
          if(input[[paste0("a_std_value", i)]] != "") {
            STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("a_std_value", i)]]
            STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("a_std_expiration", i)]])
            STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("a_std_type", i)]])
            STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("a_std_lot", i)]])
            READING[length(READING) + 1] <- empty_if_null(input[[paste0("a_reading", i)]])
            TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("a_temperature", i)]])
            DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("a_datetime", i)]])
            TYPE[length(TYPE) + 1] <- "RECL"
            USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- input[[paste0("a_used", i)]]
          }
        }
      }
    }
    
    sc_reading_df <- data.frame(STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING, TEMPERATURE, 
                                DATETIME, TYPE, USED_FOR_RECAL, stringsAsFactors = FALSE)
    
    #If this is a previous entry that is being edited, add the ID columns 
    if(!is.null(readings)) {
      if(nrow(sc_reading_df) >= nrow(readings)) {
        sc_reading_df <- sc_reading_df %>%
          mutate(SC_ID = check$SC_ID[1],
                 SCR_ID = readings$SCR_ID[1:nrow(sc_reading_df)])
      } else {
        sc_reading_df <- sc_reading_df %>%
          mutate(SC_ID = check$SC_ID[1],
                 SCR_ID = NA)
        sc_reading_df$SCR_ID[1:nrow(sc_reading_df)] <- 
          readings$SCR_ID[1:nrow(sc_reading_df)]
      }
      sc_reading_df <- select(sc_reading_df,
                              SCR_ID, SC_ID, STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT,
                              READING, TEMPERATURE, DATETIME, TYPE, USED_FOR_RECAL)
    }
    
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
    fluidRow(
      column(2, uiOutput(ns("sensor_sn_ui")))
    ),
    fluidRow(
      column(2,
        uiOutput(ns("sensor_limit_ui"))
      )
    ),
    fluidRow(
      column(8,
        uiOutput(ns("comment_ui"))
      )
    ),
    tabsetPanel(
      tabPanel("Calibration",
        fluidRow(
          column(2, uiOutput(ns("before_slider_ui")))
        ),
        uiOutput(ns("tby_reading_before_ui"))
      ),
      tabPanel("Post-calibration",
        fluidRow(
          column(2, uiOutput(ns("after_slider_ui")))
        ),
        uiOutput(ns("tby_reading_after_ui"))
      )
    )
  )
  
}

manualTby <- function(input, output, session, sn = NULL, selected_check = NULL, selected_readings = NULL) {
  
  output$sensor_sn_ui <- renderUI({
    
    ns <- session$ns
  
    if(is.null(sn())) {
      val <- ""
    } else {
      val <- sn()
    }
    
    textInput(ns("tby_sensor_sn"), "Sensor serial number", placeholder = "12A34567", value = val)
    
  })
  
  outputOptions(output, "sensor_sn_ui", suspendWhenHidden = FALSE)
  
  output$sensor_limit_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    
    if(is.null(check)) {
      val <- ""
    } else {
      val <- check$SENSOR_LIMIT
    }
    
    textInput(ns("tby_sensor_limit"), "Sensor limit", value = val)
    
  })
  
  outputOptions(output, "sensor_limit_ui", suspendWhenHidden = FALSE)
  
  output$comment_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    
    if(is.null(check)) {
      val <- ""
    } else {
      val <- check$COMMENT
    }
    
    textInput(ns("tby_comment"), "Comment", width = "65%", value = val)  
    
  })
  
  outputOptions(output, "comment_ui", suspendWhenHidden = FALSE)
  
  output$before_slider_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(readings)) {
      val <- 1
    } else {
      val <- nrow(readings[readings$TYPE == "CALI",])
      if(val == 0)
        val <- 1
    }
    
    sliderInput(ns("reading_count_before"), label = "Number of readings", min = 1, max = 5, value = val, step = 1)
    
  })
  outputOptions(output, "before_slider_ui", suspendWhenHidden = FALSE)
  
  output$after_slider_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(readings)) {
      val <- 1
    } else {
      val <- nrow(readings[readings$TYPE == "RECL",])
      if(val == 0)
        val <- 1
    }
    
    sliderInput(ns("reading_count_after"), label = "Number of readings", min = 1, max = 5, value = val, step = 1)
    
  })
  outputOptions(output, "after_slider_ui", suspendWhenHidden = FALSE)
  
  output$tby_reading_before_ui <- renderUI({
    
    ns <- session$ns
    
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(readings)) {
      before <- data.frame(
        STD_VALUE = rep("", input$reading_count_before),
        STD_EXPIRATION = rep("", input$reading_count_before),
        STD_TYPE = rep("Stablcal", input$reading_count_before),
        STD_LOT = rep("", input$reading_count_before),
        READING = rep("", input$reading_count_before),
        TEMPERATURE = rep("", input$reading_count_before),
        DATETIME = rep(as.character(Sys.time(), format = "%Y-%m-%d %H:%M"), input$reading_count_before)
      )
    } else {
      before <- readings %>%
        filter(TYPE == "CALI")
    }
    
    lapply(1:input$reading_count_before, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("b_std_value", i)), label = "Std. value",
                            value = before$STD_VALUE[i])),
        column(1, textInput(ns(paste0("b_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd", value = before$STD_EXPIRATION[i])),
        column(1, selectInput(ns(paste0("b_std_type", i)), label = "Std. type",
                              choices = c("Stablcal", "Formazin", "DI water"),
                              selected = before$STD_TYPE[i])),
        column(1, textInput(ns(paste0("b_std_lot", i)), label = "Std. lot",
                            value = before$STD_LOT[i])),
        column(1, textInput(ns(paste0("b_reading", i)), label = "Reading",
                            value = before$READING[i])),
        column(1, textInput(ns(paste0("b_temperature", i)), label = "Temperature",
                            value = before$TEMPERATURE[i])),
        column(1, textInput(ns(paste0("b_datetime", i)), label = "Date/Time",
                            value = before$DATETIME[i]))
      )
    })
  })
  outputOptions(output, "tby_reading_before_ui", suspendWhenHidden = FALSE)
  
  output$tby_reading_after_ui <- renderUI({
    
    ns <- session$ns
 
    readings <- selected_readings()
    check <- selected_check()
    
    if(is.null(readings)) {
      after <- data.frame(
        STD_VALUE = rep("", input$reading_count_after),
        STD_EXPIRATION = rep("", input$reading_count_after),
        STD_TYPE = rep("Stablcal", input$reading_count_after),
        STD_LOT = rep("", input$reading_count_after),
        READING = rep("", input$reading_count_after),
        TEMPERATURE = rep("", input$reading_count_after),
        DATETIME = rep(as.character(Sys.time(), format = "%Y-%m-%d %H:%M"), input$reading_count_after),
        USED_FOR_RECAL = rep(FALSE, input$reading_count_after)
      )
    } else {
      after <- readings %>%
        filter(TYPE == "RECL")
      after$USED_FOR_RECAL[is.na(after$USED_FOR_RECAL)] <- FALSE
    }
    
    lapply(1:input$reading_count_after, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("a_std_value", i)), label = "Std. value",
                            value = after$STD_VALUE[i])),
        column(1, textInput(ns(paste0("a_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd",
                            value = after$STD_EXPIRATION[i])),
        column(1, selectInput(ns(paste0("a_std_type", i)), label = "Std. type",
                              choices = c("Stablcal", "Formazin", "DI water"),
                              selected = after$STD_TYPE[i])),
        column(1, textInput(ns(paste0("a_std_lot", i)), label = "Std. lot",
                            value = after$STD_LOT[i])),
        column(1, textInput(ns(paste0("a_reading", i)), label = "Reading",
                            value = after$READING[i])),
        column(1, textInput(ns(paste0("a_temperature", i)), label = "Temperature",
                            value = after$TEMPERATURE[i])),
        column(1, textInput(ns(paste0("a_datetime", i)), label = "Date/Time",
                            value = after$DATETIME[i])),
        column(1, checkboxInput(ns(paste0("a_used", i)), label = "Used for recal",
                                value = false_if_null(after$USED_FOR_RECAL[i])))
      )
    })
  })
  outputOptions(output, "tby_reading_after_ui", suspendWhenHidden = FALSE)
  
  tby_check_list <- reactive({

    readings <- selected_readings()
    check <- selected_check()

    if(input$tby_sensor_sn != "") {

      #Get data for tby_CHECK table

      SENSOR_ID <- input$tby_sensor_sn
      SENSOR_LIMIT <- input$tby_sensor_limit
      COMMENT <- empty_if_null(input$tby_comment)

      if(!is.null(check)) {
        TBY_ID = check$TBY_ID[1]
        CAL_ID = check$CAL_ID[1]
        tby_check_df <- data.frame(TBY_ID, CAL_ID, SENSOR_ID, SENSOR_LIMIT, COMMENT,
                                   stringsAsFactors = FALSE)
      } else {

        tby_check_df <- data.frame(SENSOR_ID, SENSOR_LIMIT, COMMENT,
                                  stringsAsFactors = FALSE)
      }

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
    USED_FOR_RECAL <- vector()

    # Get the before data
    if(!is.null(input$reading_count_before)) {
      for(i in 1:input$reading_count_before) {
        if(!is.null(input[[paste0("b_std_value", i)]])) {
          if(input[[paste0("b_std_value", i)]] != "") {
            STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("b_std_value", i)]]
            STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("b_std_expiration", i)]])
            STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("b_std_type", i)]])
            STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("b_std_lot", i)]])
            READING[length(READING) + 1] <- empty_if_null(input[[paste0("b_reading", i)]])
            TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("b_temperature", i)]])
            DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("b_datetime", i)]])
            TYPE[length(TYPE) + 1] <- "CALI"
            USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- FALSE
          }
        }
      }
    }
    #Get the after data
    if(!is.null(input$reading_count_after)) {
      for(i in 1:input$reading_count_after) {
        if(!is.null(input[[paste0("a_std_value", i)]])) {
          if(input[[paste0("a_std_value", i)]] != "") {
            STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("a_std_value", i)]]
            STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("a_std_expiration", i)]])
            STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("a_std_type", i)]])
            STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("a_std_lot", i)]])
            READING[length(READING) + 1] <- empty_if_null(input[[paste0("a_reading", i)]])
            TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("a_temperature", i)]])
            DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("a_datetime", i)]])
            TYPE[length(TYPE) + 1] <- "RECL"
            USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- input[[paste0("a_used", i)]]
          }
        }
      }
    }

    tby_reading_df <- data.frame(STD_VALUE, STD_EXPIRATION, STD_TYPE, STD_LOT, READING, TEMPERATURE,
                                DATETIME, TYPE, USED_FOR_RECAL, stringsAsFactors = FALSE)

    #If this is a previous entry that is being edited, add the ID columns
    if(!is.null(readings)) {
      if(nrow(tby_reading_df) >= nrow(readings)) {
        tby_reading_df <- tby_reading_df %>%
          mutate(TBY_ID = check$TBY_ID[1],
                 TBYR_ID = readings$TBYR_ID[1:nrow(tby_reading_df)])
      } else {
        tby_reading_df <- tby_reading_df %>%
          mutate(TBY_ID = check$TBY_ID[1],
                 TBYR_ID = NA)
        tby_reading_df$TBYR_ID[1:nrow(tby_reading_df)] <-
          readings$TBYR_ID[1:nrow(tby_reading_df)]
      }
      tby_reading_df <- select(tby_reading_df,
                              TBYR_ID, TBY_ID, STD_VALUE, STD_EXPIRATION, STD_TYPE,
                              STD_LOT, READING, TEMPERATURE,
                              DATETIME, TYPE, USED_FOR_RECAL)
    }
    
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
    fluidRow(
      column(2, uiOutput(ns("sensor_sn_ui")))
    ),
    fluidRow(
      column(3,
        uiOutput(ns("do_sc_air_saturated_water_ui"))
      ),
      column(3,
        uiOutput(ns("do_temp_air_saturated_water_ui"))       
      ),
      column(3,
        uiOutput(ns("do_salinity_ui"))       
      )
    ),
    fluidRow(
      column(2,
        uiOutput(ns("do_odo_gain_pre_ui"))
      ),
      column(2,
        uiOutput(ns("do_odo_gain_post_ui"))       
      ),
      column(2,
        uiOutput(ns("do_odo_cap_changed_ui"))       
      ),
      column(2,
        uiOutput(ns("do_odo_cap_sn_ui"))       
      )
    ),
    fluidRow(
      column(2, uiOutput(ns("do_date_barometer_calibrated_ui")))
    ),
    fluidRow(
      column(2, uiOutput(ns("do_comment_ui")))
    ),
    tabsetPanel(
      tabPanel("Calibration",
        uiOutput(ns("before_ui"))
      ),
      tabPanel("Post-calibration",
        uiOutput(ns("after_ui") )
      )
    )
  )
  
}

manualDo <- function(input, output, session, sn = NULL, selected_check = NULL, selected_readings = NULL) {
  
  output$sensor_sn_ui <- renderUI({
    
    ns <- session$ns
    
    if(!is.null(sn())) {
      val <- sn()
    } else {
      val <- ""
    }
    
    textInput(ns("do_sensor_sn"), "Sensor serial number", placeholder = "12A34567", value = val)
    
  })
  outputOptions(output, "sensor_sn_ui", suspendWhenHidden = FALSE)
  
  output$do_sc_air_saturated_water_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$SC_AIR_SATURATED_WATER 
    } else {
      val <- ""
    }
    
    textInput(ns("do_sc_air_saturated_water"), "SC of air saturated water", value = val)
    
  })
  
  outputOptions(output, "do_sc_air_saturated_water_ui", suspendWhenHidden = FALSE)
  
  output$do_temp_air_saturated_water_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$TEMP_AIR_SATURATED_WATER
    } else {
      val <- ""
    }
    
    textInput(ns("do_temp_air_saturated_water"), "Temperature of air saturated water", value = val)
    
  })
  
  outputOptions(output, "do_temp_air_saturated_water_ui", suspendWhenHidden = FALSE)
  
  output$do_salinity_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$SALINITY 
    } else {
      val <- ""
    }
    
    textInput(ns("do_salinity"), "Salinity", value = val)
    
  })
  
  outputOptions(output, "do_salinity_ui", suspendWhenHidden = FALSE)
  
  output$do_odo_gain_pre_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$ODO_GAIN_PRE
    } else {
      val <- ""
    }
    
    textInput(ns("do_odo_gain_pre"), "ODO gain", value = val)
    
  })
  
  outputOptions(output, "do_odo_gain_pre_ui", suspendWhenHidden = FALSE)
  
  output$do_odo_gain_post_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$ODO_GAIN_POST 
    } else {
      val <- ""
    }
    
    textInput(ns("do_odo_gain_post"), "Post-calibration ODO gain", value = val) 
    
  })
  
  outputOptions(output, "do_odo_gain_post_ui", suspendWhenHidden = FALSE)
  
  output$do_odo_cap_changed_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- as.logical(check$ODO_CAP_CHANGED)
    } else {
      val <- FALSE
    }

    checkboxInput(ns("do_odo_cap_changed"), "ODO cap changed", value = false_if_null(val))
    
  })
  
  outputOptions(output, "do_odo_cap_changed_ui", suspendWhenHidden = FALSE)
  
  output$do_odo_cap_sn_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$ODO_CAP_SN
    } else {
      val <- ""
    }
    
    textInput(ns("do_odo_cap_sn"), "ODO cap serial", value = val)  
    
  })
  
  outputOptions(output, "do_odo_cap_sn_ui", suspendWhenHidden = FALSE)
  
  output$do_date_barometer_calibrated_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$DATE_BAROMETER_CALIBRATED
    } else {
      val <- ""
    }
  
    textInput(ns("do_date_barometer_calibrated"), "Date barometer calibrated", value = "")
        
  })
  
  outputOptions(output, "do_date_barometer_calibrated_ui", suspendWhenHidden = FALSE)
  
  output$do_comment_ui <- renderUI({
    
    ns <- session$ns
    check <- selected_check()
    if(!is.null(check)) {
      val <- check$COMMENT 
    } else {
      val <- ""
    }
    
    textInput(ns("do_comment"), "Comment", width = "65%", value = val)
    
  })
  
  outputOptions(output, "do_comment_ui", suspendWhenHidden = FALSE)
  
  output$before_ui <- renderUI({
    
    ns <- session$ns
    readings <- selected_readings()

    if(!is.null(readings)) {
      before <- readings %>%
        filter(TYPE == "CALI")
    } else {
      before <- data.frame(TEMPERATURE = "", PRESSURE = "", SALINITY_CORRECTION = "1",
                           DO_TABLE_VALUE = "", READING = "", 
                           DATETIME = as.character(Sys.time(), format = "%Y-%m-%d %H:%M"),
                           ZERO_READING = "", USED_FOR_RECAL = "")
    }
    
    fluidRow(
      column(1, textInput(ns("b_temperature"), "Temperature (C)",
                          value = before$TEMPERATURE[1])),
      column(1, textInput(ns("b_pressure"), "Pressure (mmHg)",
                          value = before$PRESSURE[1])),
      column(1, textInput(ns("b_salinity_correction"), "Salinity corection", 
                          value = before$SALINITY_CORRECTION[1])),
      column(1, textInput(ns("b_do_table_value"), "DO table value",
                          value = before$DO_TABLE_VALUE[1])),
      column(1, textInput(ns("b_reading"), "Reading",
                          value = before$READING[1])),
      column(1, textInput(ns("b_datetime"), "Datetime", placeholder = "yyyy-mm-dd hh:mm",
                          value = before$DATETIME[1])),
      column(1, textInput(ns("b_zero_reading"), "Reading in zero soln.",
                          value = before$ZERO_READING[1]))
    )
    
  })
  outputOptions(output, "before_ui", suspendWhenHidden = FALSE)
  
  output$after_ui <- renderUI({
    
    ns <- session$ns
    readings <- selected_readings()
    
    if(!is.null(readings)) {
      after <- readings %>%
        filter(TYPE == "RECL")
    } else {
      after <- data.frame(TEMPERATURE = "", PRESSURE = "", SALINITY_CORRECTION = "1",
                           DO_TABLE_VALUE = "", READING = "", 
                           DATETIME = as.character(Sys.time(), format = "%Y-%m-%d %H:%M"),
                           ZERO_READING = "", USED_FOR_RECAL = "")
    }
    
    fluidRow(
      column(1, textInput(ns("a_temperature"), "Temperature (C)",
                          value = after$TEMPERATURE[1])),
      column(1, textInput(ns("a_pressure"), "Pressure (mmHg)",
                          value = after$PRESSURE[1])),
      column(1, textInput(ns("a_salinity_correction"), "Salinity corection", 
                          value = after$SALINITY_CORRECTION[1])),
      column(1, textInput(ns("a_do_table_value"), "DO table value",
                          value = after$DO_TABLE_VALUE[1])),
      column(1, textInput(ns("a_reading"), "Reading",
                          value = after$READING[1])),
      column(1, textInput(ns("a_datetime"), "Datetime",
                          value = after$DATETIME[1])),
      column(1, textInput(ns("a_zero_reading"), "Reading in zero soln.",
                          value = after$ZERO_READING[1]))        
    )
    
  })
  outputOptions(output, "after_ui", suspendWhenHidden = FALSE)
  
  do_check_list <- reactive({
  
    check <- selected_check()
    readings <- selected_readings()
    
    if(input$do_sensor_sn != "") {
      
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
      
      if(!is.null(check)) {
        DO_ID <- check$DO_ID[1]
        CAL_ID <- check$CAL_ID[1]
        do_check_df <- data.frame(DO_ID, CAL_ID, SENSOR_ID, SC_AIR_SATURATED_WATER, TEMP_AIR_SATURATED_WATER, SALINITY,
                                  DATE_BAROMETER_CALIBRATED, ODO_GAIN_PRE, ODO_CAP_CHANGED, ODO_CAP_SN,
                                  ODO_GAIN_POST, COMMENT,
                                  stringsAsFactors = FALSE)
      } else {
      
        do_check_df <- data.frame(SENSOR_ID, SC_AIR_SATURATED_WATER, TEMP_AIR_SATURATED_WATER, SALINITY,
                                  DATE_BAROMETER_CALIBRATED, ODO_GAIN_PRE, ODO_CAP_CHANGED, ODO_CAP_SN,
                                  ODO_GAIN_POST, COMMENT,
                                  stringsAsFactors = FALSE)
      }
      
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
    USED_FOR_RECAL <- vector()
    
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
        USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- FALSE
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
        USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- FALSE
      }
    }
    
    do_reading_df <- data.frame(TEMPERATURE, PRESSURE, SALINITY_CORRECTION, DO_TABLE_VALUE, READING, DATETIME, 
                                ZERO_READING, TYPE, USED_FOR_RECAL, stringsAsFactors = FALSE)
    
    #If this is a previous entry that is being edited, add the ID columns 
    if(!is.null(readings)) {
      if(nrow(do_reading_df) >= nrow(readings)) {
        do_reading_df <- do_reading_df %>%
          mutate(DO_ID = check$DO_ID[1],
                 DOR_ID = readings$DOR_ID[1:nrow(do_reading_df)])
      } else {
        do_reading_df <- do_reading_df %>%
          mutate(DO_ID = check$DO_ID[1],
                 DOR_ID = NA)
        do_reading_df$DOR_ID[1:nrow(do_reading_df)] <- 
          readings$DOR_ID[1:nrow(do_reading_df)]
      }
      do_reading_df <- select(do_reading_df,
                              DOR_ID, DO_ID, TEMPERATURE, PRESSURE, SALINITY_CORRECTION,
                              DO_TABLE_VALUE, READING, DATETIME, ZERO_READING, TYPE,
                              USED_FOR_RECAL)
    }
    
    list_out <- list(DO_CHECK = do_check_df, DO_READING = do_reading_df)
  
  })

return(do_check_list)
  
}

##########################################################################################################################################################
#
# pH
# Module for getting pH calibration data
#
##########################################################################################################################################################

manualPhInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textInput(ns("ph_sensor_sn"), "Sensor serial number", placeholder = "12A34567"),
    textInput(ns("ph_comment"), "Comment", width = "65%"),
    tabsetPanel(
      tabPanel("Calibration",
               sliderInput(ns("reading_count_before"), label = "Number of readings", min = 1, max = 5, value = 1, step = 1),
               uiOutput(ns("ph_reading_before_ui"))
      ),
      tabPanel("Post-calibration",
               sliderInput(ns("reading_count_after"), label = "Number of readings", min = 1, max = 5, value = 1, step = 1),
               uiOutput(ns("ph_reading_after_ui"))
      )
    )
  )
  
}

manualPh <- function(input, output, session) {
  
  output$ph_reading_before_ui <- renderUI({
    
    ns <- session$ns
    
    lapply(1:input$reading_count_before, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("b_std_uncorrected", i)), label = "Std. value, uncorrected")),
        column(1, textInput(ns(paste0("b_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd")),
        column(1, selectInput(ns(paste0("b_std_type", i)), label = "Std. type",
                              choices = c("RICCA", "Other"))),
        column(1, textInput(ns(paste0("b_std_lot", i)), label = "Std. lot")),
        column(1, textInput(ns(paste0("b_temperature", i)), label = "Temperature")),
        column(1, textInput(ns(paste0("b_std_value", i)), label = "Std. value")),
        column(1, textInput(ns(paste0("b_reading", i)), label = "Reading")),
        column(1, textInput(ns(paste0("b_millivolts", i)), label = "Millivolts")),
        column(1, textInput(ns(paste0("b_datetime", i)), label = "Datetime",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M")))
      )
    })
  })
  
  output$ph_reading_after_ui <- renderUI({
    
    ns <- session$ns
    
    lapply(1:input$reading_count_after, function(i) {
      fluidRow(
        column(1, textInput(ns(paste0("a_std_uncorrected", i)), label = "Std. value, uncorrected")),
        column(1, textInput(ns(paste0("a_std_expiration", i)), label = "Std. expiration",
                            placeholder = "yyyy-mm-dd")),
        column(1, selectInput(ns(paste0("a_std_type", i)), label = "Std. type",
                              choices = c("RICCA", "Other"))),
        column(1, textInput(ns(paste0("a_std_lot", i)), label = "Std. lot")),
        column(1, textInput(ns(paste0("a_temperature", i)), label = "Temperature")),
        column(1, textInput(ns(paste0("a_std_value", i)), label = "Std. value")),
        column(1, textInput(ns(paste0("a_reading", i)), label = "Reading")),
        column(1, textInput(ns(paste0("a_millivolts", i)), label = "Millivolts")),
        column(1, textInput(ns(paste0("a_datetime", i)), label = "Datetime",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M"))),
        column(1, checkboxInput(ns(paste0("a_used", i)), label = "Used for recal"))
      )
    })
    
  })
  
  ph_check_list <- reactive({
    
    
    if(input$ph_sensor_sn != "") {
      
      #Get data for tby_CHECK table
      
      SENSOR_ID <- input$ph_sensor_sn
      COMMENT <- empty_if_null(input$ph_comment)
      
      ph_check_df <- data.frame(SENSOR_ID, COMMENT, 
                                stringsAsFactors = FALSE)
      
    } else {
      
      ph_check_df <- data.frame(SENSOR_ID = vector(), COMMENT = vector())
      
    }
    
    #Get data for tby_READING table
    
    STD_UNCORRECTED <- vector()
    STD_EXPIRATION <- vector()
    STD_TYPE <- vector()
    STD_LOT <- vector()
    TEMPERATURE <- vector()
    STD_VALUE <- vector()
    READING <- vector()
    DATETIME <- vector()
    MILLIVOLTS <- vector()
    TYPE <- vector()
    USED_FOR_RECAL <- vector()
    
    # Get the before data
    for(i in 1:input$reading_count_before) {
      
      if(!is.null(input[[paste0("b_std_value", i)]])) {
        if(input[[paste0("b_std_value", i)]] != "") {
          STD_UNCORRECTED[length(STD_UNCORRECTED) + 1] <- input[[paste0("b_std_uncorrected", i)]]
          STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("b_std_expiration", i)]])
          STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("b_std_type", i)]])
          STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("b_std_lot", i)]])
          TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("b_temperature", i)]])
          STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("b_std_value", i)]]
          READING[length(READING) + 1] <- empty_if_null(input[[paste0("b_reading", i)]])
          DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("b_datetime", i)]])
          MILLIVOLTS[length(MILLIVOLTS) + 1] <- empty_if_null(input[[paste0("b_millivolts", i)]])
          TYPE[length(TYPE) + 1] <- "CALI"
          USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- FALSE
        }
      }
      
    }
    
    #Get the after data
    for(i in 1:input$reading_count_after) {
      
      if(!is.null(input[[paste0("a_std_value", i)]])) {
        if(input[[paste0("a_std_value", i)]] != "") {
          STD_UNCORRECTED[length(STD_UNCORRECTED) + 1] <- input[[paste0("a_std_uncorrected", i)]]
          STD_EXPIRATION[length(STD_EXPIRATION) + 1] <- empty_if_null(input[[paste0("a_std_expiration", i)]])
          STD_TYPE[length(STD_TYPE) + 1] <- empty_if_null(input[[paste0("a_std_type", i)]])
          STD_LOT[length(STD_LOT) + 1] <- empty_if_null(input[[paste0("a_std_lot", i)]])
          TEMPERATURE[length(TEMPERATURE) + 1] <- empty_if_null(input[[paste0("a_temperature", i)]])
          STD_VALUE[length(STD_VALUE) + 1] <- input[[paste0("a_std_value", i)]]
          READING[length(READING) + 1] <- empty_if_null(input[[paste0("a_reading", i)]])
          DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("a_datetime", i)]])
          MILLIVOLTS[length(MILLIVOLTS) + 1] <- empty_if_null(input[[paste0("a_millivolts", i)]])
          TYPE[length(TYPE) + 1] <- "RECL"
          USED_FOR_RECAL[length(USED_FOR_RECAL) + 1] <- input[[paste0("a_used", i)]]
        }
      }
      
    }
    
    ph_reading_df <- data.frame(STD_UNCORRECTED, STD_EXPIRATION, STD_TYPE, STD_LOT, TEMPERATURE, STD_VALUE, READING,
                                DATETIME, MILLIVOLTS, TYPE, USED_FOR_RECAL, stringsAsFactors = FALSE)
    
    list_out <- list(PH_CHECK = ph_check_df, PH_READING = ph_reading_df)
    
  })
  
  return(ph_check_list)
  
}

##########################################################################################################################################################
#
# WT
# Module for getting water temperature calibration data
#
##########################################################################################################################################################


manualWtInput <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    textInput(ns("wt_sensor_sn"), label = "Sensor serial number", placeholder = "12A34567"),
    textInput(ns("wt_comment"), label = "Comment", width = "65%"),
    tabsetPanel(
      tabPanel("Comparison",
        fluidRow(
          column(3, textInput(ns("field_sensor_sn"), label = "Field Sensor Serial Number", placeholder = "12A3456")),
          column(3, textInput(ns("wt_comparison_datetime"), label = "Date/Time", placeholder = "yyyy-mm-dd hh:mm"))
        ),
        fluidRow(
          column(2, textInput(ns("nist_temp_comp"), label = "NIST/Field temperature")),
          column(2, textInput(ns("mon_temp_comp"), label = "Site monitor temperature"))
        ),
        fluidRow(
          column(2,
            textInput(ns("last_2_point_check"), label = "Last two point check", placeholder = "yyyy-mm-dd"),
            textInput(ns("last_5_point_check"), label = "Last five point check", placeholder = "yyyy-mm-dd")
          )
        )
      ),
      tabPanel("Multi-point check",
        sliderInput(ns("reading_count"), label = "Number of readings", min = 1, max = 5, value = 1, step = 1),
        fluidRow(
          column(3, textInput(ns("nist_cert_date"), label = "NIST Certification Date", placeholder = "yyyy-mm-dd")),
          column(3, textInput(ns("nist_sn"), label = "NIST Serial Number"))
        ),
        uiOutput(ns("wt_multipoint_ui"))
      )
    )
  )
  
}

manualWt <- function(input, output, session) {
  
  output$wt_multipoint_ui <- renderUI({
    
    ns <- session$ns
    
    lapply(1:input$reading_count, function(i) {
      fluidRow(
        column(2, textInput(ns(paste0("nist_temp", i)), label = "NIST Temperature")),
        column(2, textInput(ns(paste0("mon_temp", i)), label = "Monitor Temperature")),
        column(2, textInput(ns(paste0("datetime", i)), label = "Date/Time",
                            value = as.character(Sys.time(), format="%Y-%m-%d %H:%M")))
      )
    })
  })
  
  wt_list <- reactive({
    
    if(input$wt_sensor_sn != "") {
      
      #Get data for WT_COMPARISON_CHECK
      
      SENSOR_ID <- input$wt_sensor_sn
      FIELD_SENSOR_SN <- empty_if_null(input$field_sensor_sn)
      DATETIME <- empty_if_null(input$wt_comparison_datetime)
      CHECK_MEASURE <- empty_if_null(input$nist_temp_comp)
      SENSOR_MEASURE <- empty_if_null(input$mon_temp_comp)
      TWO_POINT_CHECK_DATE <- empty_if_null(input$last_2_point_check)
      FIVE_POINT_CHECK_DATE <- empty_if_null(input$last_5_point_check)
      COMMENT <- empty_if_null(input$wt_comment)
      
      
      wt_comparison_check_df <- data.frame(SENSOR_ID, FIELD_SENSOR_SN, DATETIME, 
                                           CHECK_MEASURE, SENSOR_MEASURE,
                                           TWO_POINT_CHECK_DATE, FIVE_POINT_CHECK_DATE,
                                           COMMENT,
                                           stringsAsFactors = FALSE)

      if(all(c(FIELD_SENSOR_SN, DATETIME, CHECK_MEASURE, SENSOR_MEASURE, TWO_POINT_CHECK_DATE,
               FIVE_POINT_CHECK_DATE) == ""))
        wt_comparison_check_df <- wt_comparison_check_df[0,]
      
    } else {
      
      wt_comparison_check_df <- data.frame(SENSOR_ID = vector(), FIELD_SENSOR_SN = vector(), 
                                DATETIME = vector(), CHECK_MEASURE = vector(), 
                                SENSOR_MEASURE = vector(), TWO_POINT_CHECK_DATE = vector(),
                                FIVE_POINT_CHECK_DATE = vector(), COMMENT = vector())
      
    }
    
    #Get data for WT_MULTIPOINT_CHECK table
    if(input$wt_sensor_sn != "") {
      
      SENSOR_ID <- input$wt_sensor_sn
      NIST_CERT_DATE <- empty_if_null(input$nist_cert_date)
      NIST_SN <- empty_if_null(input$nist_sn)
      COMMENT <- empty_if_null(input$wt_comment)
      
      wt_multipoint_check_df <- data.frame(SENSOR_ID, NIST_CERT_DATE, NIST_SN, COMMENT,
                                           stringsAsFactors = FALSE)
      
    } else {
      
      wt_multipoint_check_df <- data.frame(SENSOR_ID = vector(), NIST_CERT_DATE = vector(),
                                           NIST_SN = vector(), COMMENT = vector())
      
    }
    
    
    #Get data for WT_MULTIPOINT_READING table
    
    NIST_READING <- vector()
    MONITOR_READING <- vector()
    DATETIME <- vector()
    
    # Get the multi-point data
    for(i in 1:input$reading_count) {
      
      if(!is.null(input[[paste0("nist_temp", i)]])) {
        if(input[[paste0("nist_temp", i)]] != "") {
          NIST_READING[length(NIST_READING) + 1] <- input[[paste0("nist_temp", i)]]
          MONITOR_READING[length(MONITOR_READING) + 1] <- input[[paste0("mon_temp", i)]]
          DATETIME[length(DATETIME) + 1] <- empty_if_null(input[[paste0("datetime", i)]])
        }
      }
      
    }
    
    wt_multipoint_reading_df <- data.frame(NIST_READING, MONITOR_READING, DATETIME, 
                                           stringsAsFactors = FALSE)
    
    list_out <- list(WT_COMPARISON_CHECK = wt_comparison_check_df, 
                     WT_MULTIPOINT_CHECK = wt_multipoint_check_df,
                     WT_MULTIPOINT_READING = wt_multipoint_reading_df)
    
  })
  
  return(wt_list)
  
}
