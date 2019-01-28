library(shiny)
library(shinydashboard)
library(dplyr)
source("tools.R")
source("modules.R")

parms <- c("Specific cond at 25C", "Turbidity, FNU", "Dissolved oxygen", "pH")

ui <- dashboardPage(
  dashboardHeader(title = "Calibration Logbook"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("New calibration", startExpanded = TRUE,
        menuSubItem("From XML", tabName = "new_cal_xml"),
        menuSubItem("Manual", tabName = "new_cal_manual")
      ),
      menuItem("View calibrations", tabName = "view_cal"),
      menuItem("Export data", tabName = "export_data")
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "new_cal_xml",
        box(
          h3("Upload an XML file from SVMAQ"),
          fluidRow(
                
            column(4,
                     
              fileInput("xml_file", label = "SVMAQ XML file"),
              actionButton("write_xml", "Record file")
                     
            ),
            column(6,
              textOutput("xml_check_text"),
              tableOutput("xml_sensor_table")
                     
            )
          )
        )
      ),
      
      tabItem(tabName = "new_cal_manual",
        tabBox(
          tabPanel("Specific cond at 25C",
            manualScInput("sc_check1"),
            verbatimTextOutput("sc_out")
          ),
          tabPanel("Turbidity, FNU",
            manualTbyInput("tby_check1"),
            verbatimTextOutput("tby_out")
          ),
          tabPanel("Dissolved oxygen",
            manualDoInput("do_check1"),
            fluidRow(
              column(12,
                verbatimTextOutput("do_out")
              )
            )
          ),
          tabPanel("pH",
            manualPhInput("ph_check1"),
            verbatimTextOutput("ph_out")
          ),
          tabPanel("Record",
            h3("Record calibration"),
            actionButton("write_manual", "Record")
          ),
        width = NULL)       
      ),
      tabItem("view_cal",
        box(
          h3("Find a calibration"),
          fluidRow(
            column(4, selectInput("find_cal_parm", "Parameter", choices = parms)),
            column(3, uiOutput("sensor_sn_ui")),
            column(4, dateRangeInput("find_cal_dates", "Dates"))
          ),
          uiOutput("select_calibration_ui"),
          tableOutput("view_check_out"),
          tableOutput("view_reading_out")
        )
      ),
      tabItem("export_data",
        box()        
      )
    )
  )
)

server <- function(input, output, session) { 
  
  dbcon <- cal_book_connect("cal_db.db")
  
  sv_data <- reactive({
    
    input_file <- input$xml_file
    
    if(is.null(input_file))
      return(NULL)
    
    sv_data <- read_sv_xml(input_file$datapath) %>%
      get_ALL()
    
    return(sv_data)
    
  })
  
  output$xml_sensor_table <- renderTable({
    
    if(is.null(sv_data()))
      return(NULL)
    
    sensors <- sv_data()[["SENSOR"]]
    
    return(sensors)
    
  })
  
  output$xml_check_text <- renderText({
    
    if(is.null(sv_data()))
      return(NULL)
    
    base <- "Calibration data found for"
    parms <- vector()
    
    if(nrow(sv_data()[["SC_CHECK"]]) > 0) {
      parms[length(parms) + 1] <- "specific conductance"
    }
    if(nrow(sv_data()[["TBY_CHECK"]]) > 0) {
      parms[length(parms) + 1] <- "turbidity"
    }
    if(nrow(sv_data()[["PH_CHECK"]]) > 0) {
      parms[length(parms) + 1] <- "pH"
    }
    if(nrow(sv_data()[["DO_CHECK"]]) > 0) {
      parms[length(parms) + 1] <- "dissolved oxygen"
    }
    
    parms <- paste(parms, collapse = ", ")
    
    text <- paste(base, parms)
    
    return(text)
    
  })
  
  observeEvent(input$write_xml, {
    
    if(is.null(sv_data())) {
      
      showNotification("No file selected", type = "error")
      
    } else {
      
      inFile <- input$xml_file
      filename <- basename(inFile$datapath)
      
      max_keys <- get_max_keys(dbcon)
      write_data <- sv_data() %>%
        add_keys(max_keys, source_file = filename) %>%
        lookup_sensors(dbcon)
      
      write_sv_data(write_data, dbcon)
      
      showNotification("File recorded", type = "message")
      
    }
    
    
  })
  
  sc_check <- callModule(manualSc, "sc_check1")
  tby_check <- callModule(manualTby, "tby_check1")
  do_check <- callModule(manualDo, "do_check1")
  ph_check <- callModule(manualPh, "ph_check1")
  
  output$sc_out <- renderPrint({
    
    print(sc_check())
    
  })
  
  output$tby_out <- renderPrint({
    
    print(tby_check())
    
  })
  
  output$do_out <- renderPrint({
    
    print(do_check())
    
  })
  
  output$ph_out <- renderPrint({
    
    print(ph_check())
    
  })
  
  observeEvent(input$write_manual, {
      
    combined_data <- combine_manual(sc_check(), tby_check(), do_check(), ph_check())
    
    max_keys <- get_max_keys(dbcon)
    write_data <- combined_data %>%
      add_keys(max_keys, source_file = "Manually entered") %>%
      lookup_sensors(dbcon)
    
    write_sv_data(write_data, dbcon)
    
    showNotification("File recorded", type = "message")
    
  })
  
  sensor_list <- reactive({
    
    sensors <- tbl(dbcon, "SENSOR")
    return(sensors)
    
  })
  
  output$sensor_sn_ui <- renderUI({
    
    sensors <- sensor_list()
    if(input$find_cal_parm != "All") {
      sensors <- sensors %>%
        filter(PARAMETER == input$find_cal_parm)
    }
    sensor_sns <- pull(sensors, SENSOR_SN)
    
    selectInput("find_cal_sn", "Serial number", choices = c("All", sensor_sns))
    
  })
  
  calibration_list <- reactive({
    
    #Find a list of calibrations that meet the given criteria
    if(input$find_cal_parm == "Specific cond at 25C") {
      basetable <- "SC_CHECK"
    } else if (input$find_cal_parm == "Turbidity, FNU") {
      basetable <- "TBY_CHECK"
    } else if (input$find_cal_parm == "Dissolved oxygen") {
      basetable <- "DO_CHECK"
    } else if(input$find_cal_parm == "pH") {
      basetable <- "PH_CHECK"
    } else {
      basetable <- "GEN_CHECK"
    }
    
    check <- tbl(dbcon, basetable)
    sensor <- tbl(dbcon, "SENSOR") %>%
      select(SENSOR_ID, SENSOR_SN)
    calibration <- tbl(dbcon, "CALIBRATION") %>%
      select(CAL_ID, DATE)
    
    matching_cal <- inner_join(check, sensor) %>%
      inner_join(calibration) %>%
      filter(DATE >= input$find_cal_dates[1],
             DATE <= input$find_cal_dates[2])
    
    if(!is.null(input$find_cal_sn)) {
      if(input$find_cal_sn != "All")
        matching_cal <- matching_cal %>%
          filter(SENSOR_SN == input$find_cal_sn)
    }
    
    matching_cal <- select(matching_cal, DATE, CAL_ID)
    
    cal_choices <- pull(matching_cal, CAL_ID)
    names(cal_choices) <- pull(matching_cal, DATE)
    
    return(cal_choices)
    
  })
  
  output$select_calibration_ui <- renderUI({
    
    selectizeInput("which_cal", "View data from", choices = calibration_list(), selected = NULL)
    
  })
  
  selected_calibration_out <- reactive({
    
    if(input$which_cal == "")
      return(NULL)
    
    #Find a list of calibrations that meet the given criteria
    if(input$find_cal_parm == "Specific cond at 25C") {
      check_basetable <- "SC_CHECK"
      reading_basetable <- "SC_READING"
    } else if (input$find_cal_parm == "Turbidity, FNU") {
      check_basetable <- "TBY_CHECK"
      reading_basetable <- "TBY_READING"
    } else if (input$find_cal_parm == "Dissolved oxygen") {
      check_basetable <- "DO_CHECK"
      reading_basetable <- "DO_READING"
    } else if(input$find_cal_parm == "pH") {
      check_basetable <- "PH_CHECK"
      reading_basetable <- "PH_READING"
    } else {
      check_basetable <- "GEN_CHECK"
      check_basetable <- "GEN_READING"
    }
    
    check_table <- tbl(dbcon, check_basetable) %>%
      filter(CAL_ID == input$which_cal)
    
    reading_table <- tbl(dbcon, reading_basetable)
    
    sensor_table <- tbl(dbcon, "SENSOR") %>%
      select(SENSOR_ID, SENSOR_SN)
    
    check <- check_table %>%
      inner_join(sensor_table) %>%
      select(-ends_with("_ID"))
    
    check_table <- check_table %>%
      select(ends_with("_ID"))
    
    reading <- check_table %>%
      inner_join(reading_table) %>%
      select(-ends_with("_ID"))
    
    out <- list(check, reading)
    names(out) <- c("check","reading")
    return(out)
    
  })
  
  output$view_check_out <- renderTable({
    
    selected_calibration_out()$check
    
  })
  
  output$view_reading_out <- renderTable({
    
    selected_calibration_out()$reading
    
  })
  
}

shinyApp(ui, server)