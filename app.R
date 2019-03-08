library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(knitr)
source("tools.R")
source("plotting.R")
source("modules.R")

ui <- dashboardPage(
  dashboardHeader(title = "Calibration Logbook"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("New calibration", startExpanded = TRUE,
        menuSubItem("From XML", tabName = "new_cal_xml"),
        menuSubItem("Manual", tabName = "new_cal_manual")
      ),
      menuItem("View calibrations", tabName = "view_cal"),
      menuItem("Reports", startExpanded = TRUE,
        menuSubItem("Probe history", tabName = "probe_history")
      )
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
        textInput("monitor_sn", label = "Monitor serial number", placeholder = "12A34567"),
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
          tabPanel("WT",
            manualWtInput("wt_check1"),
            verbatimTextOutput("wt_out")
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
            column(4, selectInput("find_cal_parm", "Parameter", choices=  
                                    c("Specific cond at 25C", "Turbidity, FNU", "Dissolved oxygen", "pH",
                                      "Temperature, water (comparison)", "Temperature, water (multi-point)"))),
            column(3, uiOutput("sensor_sn_ui")),
            column(4, uiOutput("select_dates_ui"))
          ),
          fluidRow(
            column(3, uiOutput("select_calibration_ui"))
          ),
          tableOutput("view_check_out"),
          tableOutput("view_reading_out"),
        width = 11)
      ),
      tabItem("probe_history",
        box(
          h3("Probe history report"),
          selectInput("probe_history_parameter", label = "Parameter",
                      choices = c("Specific cond at 25C", "Turbidity, FNU", "pH", "Dissolved oxygen")),
          uiOutput("probe_history_sn_choices"),
          downloadButton("download_probe_history", "Get report")
        )        
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
  wt_check <- callModule(manualWt, "wt_check1")
  
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
  
  output$wt_out <- renderPrint({
    
    print(wt_check())
    
  })
  
  observeEvent(input$write_manual, {
      
    combined_data <- combine_manual(input$monitor_sn, sc_check(), tby_check(), do_check(), 
                                    ph_check(), wt_check())
    
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
    
    parameter <- input$find_cal_parm
    if(parameter %in% c("Temperature, water (comparison)", "Temperature, water (multi-point)"))
      parameter <- "Temperature, water"
    
    if(input$find_cal_parm != "All") {
      sensors <- sensors %>%
        filter(PARAMETER == parameter)
    }
    sensor_sns <- pull(sensors, SENSOR_SN)
    
    selectInput("find_cal_sn", "Serial number", choices = c("All", sensor_sns))
    
  })
  
  view_base_table <- reactive({
    
    if(input$find_cal_parm == "Specific cond at 25C") {
      basetable <- c("SC_CHECK", "SC_READING")
    } else if (input$find_cal_parm == "Turbidity, FNU") {
      basetable <- c("TBY_CHECK", "TBY_READING")
    } else if (input$find_cal_parm == "Dissolved oxygen") {
      basetable <- c("DO_CHECK", "DO_READING")
    } else if(input$find_cal_parm == "pH") {
      basetable <- c("PH_CHECK", "PH_READING")
    } else if(input$find_cal_parm == "Temperature, water (comparison)") {
      basetable <- c("WT_COMPARISON_CHECK", "")
    } else if(input$find_cal_parm == "Temperature, water (multi-point)") {
      basetable <- c("WT_MULTIPOINT_CHECK", "WT_MULTIPOINT_READING")
    } else {
      basetable <- c("GEN_CHECK", "GEN_READING")
    }
    
    return(basetable)
    
  })
  
  calibration_list <- reactive({
    
    basetable <- view_base_table()[1]
    
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
    
    matching_cal <- select(matching_cal, DATE, CAL_ID) %>%
      arrange(DATE)
    
    cal_choices <- pull(matching_cal, CAL_ID)
    names(cal_choices) <- pull(matching_cal, DATE)
    
    return(cal_choices)
    
  })
  
  output$select_dates_ui <- renderUI({
    
    start_date <- Sys.Date() - as.difftime(8760, units = "hours")
    
    dateRangeInput("find_cal_dates", "Dates", start = start_date)
    
  })
  
  output$select_calibration_ui <- renderUI({
    
    selectizeInput("which_cal", "View data from", choices = calibration_list(), selected = NULL)
    
  })
  
  selected_calibration_out <- reactive({
    
    if(input$which_cal == "")
      return(NULL)
    
    #Find a list of calibrations that meet the given criteria
    check_basetable <- view_base_table()[1]
    reading_basetable <- view_base_table()[2]
    
    check_table <- tbl(dbcon, check_basetable) %>%
      filter(CAL_ID == input$which_cal)
    
    if(reading_basetable != "")
      reading_table <- tbl(dbcon, reading_basetable)
    
    sensor_table <- tbl(dbcon, "SENSOR") %>%
      select(SENSOR_ID, SENSOR_SN)
    
    check <- check_table %>%
      inner_join(sensor_table) %>%
      select(-ends_with("_ID"))
    
    check_table <- check_table %>%
      select(ends_with("_ID"))
    
    if(reading_basetable != "") {
      
      reading <- check_table %>%
        inner_join(reading_table) %>%
        select(-ends_with("_ID"))
      out <- list(check, reading)
      names(out) <- c("check","reading")
      
    } else {
      
      out <- list(check)
      names(out) <- "check"
      
    }

    return(out)
    
  })
  
  output$view_check_out <- renderTable({
    
    selected_calibration_out()$check
    
  })
  
  output$view_reading_out <- renderTable({
    
    selected_calibration_out()$reading
    
  })
  
  output$probe_history_sn_choices <- renderUI({
    
    serial_number_choices <- tbl(dbcon, "SENSOR") %>%
      filter(PARAMETER == input$probe_history_parameter) %>%
      pull(SENSOR_SN)
    
    selectInput("probe_history_sn", "Probe serial number", choices = serial_number_choices)
    
  })
  
  probe_history_plot <- reactive({
    
    plot <- error_history_plot(input$probe_history_parameter, input$probe_history_sn, dbcon)
    plot
    
  })
  
  probe_history_cal_list <- reactive({
    
    cal_list <- get_cal_list(input$probe_history_parameter, input$probe_history_sn, dbcon)
    print(cal_list)
    cal_list
    
  })
  
  output$download_probe_history <- downloadHandler(
    
    filename = function() {
      paste(input$probe_history_sn, ".html")
    }, 
    content = function(file) {
      
      
      
      temp_report <- file.path(tempdir(), "cal_history.Rmd")
      file.copy("cal_history.Rmd", temp_report, overwrite = TRUE)
      
      params <- list(plot = probe_history_plot(), cal_list = probe_history_cal_list())
      
      rmarkdown::render(temp_report, output_file = file, 
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    }
  )
  
  
}

shinyApp(ui, server)