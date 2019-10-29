library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(knitr)
library(fs)
library(RSQLite)
library(DBI)
library(dbplyr)
source("tools.R")
source("plotting.R")
source("modules.R")

ui <- dashboardPage(
  dashboardHeader(title = "Calibration Logbook"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info"),
      menuItem("View calibrations", tabName = "view_cal"),
      menuItem("New calibration", startExpanded = FALSE,
        menuSubItem("From XML", tabName = "new_cal_xml"),
        menuSubItem("Manual", tabName = "new_cal_manual")
      ),
      menuItem("Reports", startExpanded = FALSE,
        menuSubItem("Probe history", tabName = "probe_history")
      )
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "info",
        box(
          includeMarkdown("help.md")
        , width = 10)
      ),
      
      tabItem(tabName = "new_cal_xml",
        box(
          tabsetPanel(
            tabPanel("Single",
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
            ),
            tabPanel("Batch",
              h3("Upload multiple XML files from SVMAQ"),
              fluidRow(
                column(4,
                  fileInput("batch_xml_file", label = "SVMAQ XML files", multiple = TRUE),
                  actionButton("write_xml_batch", "Record files")
                )
              )
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
            column(4, uiOutput("find_cal_parm_ui")),
            column(3, uiOutput("sensor_sn_ui")),
            column(4, uiOutput("select_dates_ui"))
          ),
          fluidRow(
            column(3, uiOutput("select_calibration_ui"))
          ),
          tabsetPanel(
            tabPanel("View",
              tableOutput("view_check_out"),
              tableOutput("view_reading_out"),
              actionButton("refresh", "Refresh")
            ),
            tabPanel("Edit",
              conditionalPanel("input.find_cal_parm == 'Specific cond at 25C'",
                manualScInput("sc_edit")
              ),
              conditionalPanel("input.find_cal_parm == 'Turbidity, FNU'",
                manualTbyInput("tby_edit")
              ),
              conditionalPanel("input.find_cal_parm == 'Dissolved oxygen'",
                manualDoInput("do_edit")
              ),
              conditionalPanel("input.find_cal_parm == 'pH'",
                manualPhInput("ph_edit")                 
              ),
              conditionalPanel("input.find_cal_parm == 'Temperature, water (comparison)'",
                manualWtInput("wt_edit_comp")                 
              ),
              conditionalPanel("input.find_cal_parm == 'Temperature, water (multi-point)'",
                manualWtInput("wt_edit_multi")               
              ),
              conditionalPanel("!(input.find_cal_parm in ['Specific cond at 25C', 'Turbidity, FNU', 'pH', 'Temperature, water (comparison)', 'Temperature, water (multi-point)']",
                manualGenInput("gen_edit"),
                verbatimTextOutput("gen_edit_out")
              ),
              fluidRow(
                column(1, actionButton("update", "Update")),
                column(1, actionButton("delete_check", "Delete")),
                column(1, textInput("delete_keyword", label = NULL, placeholder = "delete"))
              )
            )
          ),
        width = 11)
      ),
      tabItem("probe_history",
        box(
          h3("Probe history report"),
          uiOutput("probe_history_parameter_ui"),
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
    if(length(sv_data()[["GENERIC"]]) > 0) {
      parms <- c(parms, map_chr(sv_data()[["GENERIC"]], c("GEN_CHECK", "PARAMETER")))
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
  
  observeEvent(input$write_xml_batch, {
    
    if(is.null(input$batch_xml_file)) {
      
      showNotification("No file selected", type = "error")
      
    } else {
      
      files <- input$batch_xml_file
      
      for(i in 1:nrow(files)) {
      
        filepath <- files$datapath[i]
        filename <- files$name[i]
        
        print(filename)
        
        write_data <- read_sv_xml(filepath) %>%
          get_ALL()
        
        print(write_data)
        
        if(!is.null(write_data)) {
          max_keys <- get_max_keys(dbcon)
          write_data %>%
            add_keys(max_keys, source_file = filename) %>%
            lookup_sensors(dbcon) %>%
            write_sv_data(dbcon)
          message <- paste(filename, "written")
          showNotification(message, type = "message")
        } else {
          message <- paste(filename, "not written")
          showNotification(message, type = "error")
        }
      }
    }
  })
  
  sc_check <- callModule(manualSc, "sc_check1", 
                         sn = reactive(NULL), 
                         selected_check = reactive(NULL), 
                         selected_readings = reactive(NULL))
  tby_check <- callModule(manualTby, "tby_check1",
                          sn = reactive(NULL),
                          selected_check = reactive(NULL),
                          selected_readings = reactive(NULL))
  do_check <- callModule(manualDo, "do_check1",
                         sn = reactive(NULL),
                         selected_check = reactive(NULL),
                         selected_readings = reactive(NULL))
  ph_check <- callModule(manualPh, "ph_check1",
                         sn = reactive(NULL),
                         selected_check = reactive(NULL),
                         selected_readings = reactive(NULL))
  wt_check <- callModule(manualWt, "wt_check1",
                         sn = reactive(NULL),
                         selected_check = reactive(NULL),
                         selected_readings = reactive(NULL))
  
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
  
  output$find_cal_parm_ui <- renderUI({
    
    generic_parms <- tbl(dbcon, "GEN_CHECK") %>%
      pull(PARAMETER) %>%
      unique()
    
    ch = c("Specific cond at 25C", "Turbidity, FNU", "Dissolved oxygen", "pH",
        "Temperature, water (comparison)", "Temperature, water (multi-point)", generic_parms)
    
    selectInput("find_cal_parm", "Parameter", choices = ch)
    
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
  
  output$select_dates_ui <- renderUI({
    
    start_date <- Sys.Date() - as.difftime(8760, units = "hours")
    
    dateRangeInput("find_cal_dates", "Dates", start = start_date)
    
  })
  
  calibration_list <- reactive({
    
    basetable <- view_base_table()[1]
    
    check <- tbl(dbcon, basetable)

    sensor <- tbl(dbcon, "SENSOR") %>%
      select(SENSOR_ID, SENSOR_SN)

    calibration <- tbl(dbcon, "CALIBRATION") %>%
      select(CAL_ID, DATE)

    matching_cal <- inner_join(check, sensor) %>%
      inner_join(calibration)
    
    if(!is.null(input$find_cal_dates)) {
      matching_cal <- matching_cal %>%
        filter(DATE >= local(input$find_cal_dates[1]),
             DATE <= local(input$find_cal_dates[2]))
    }
    
    if(!is.null(input$find_cal_sn)) {
      if(!(input$find_cal_sn %in% c("All", "")))
        matching_cal <- matching_cal %>%
          filter(SENSOR_SN == local(input$find_cal_sn))
    }
    
    if(basetable == "GEN_CHECK") {
      matching_cal <- matching_cal %>%
        filter(PARAMETER == local(input$find_cal_parm))
    }
    
    matching_cal <- select(matching_cal, DATE, CAL_ID) %>%
      arrange(DATE)
    
    cal_choices <- pull(matching_cal, CAL_ID)

    names(cal_choices) <- pull(matching_cal, DATE)
    
    print(head(cal_choices))
    
    return(cal_choices)
    
  })
  
  output$select_calibration_ui <- renderUI({
    
    selectizeInput("which_cal", "View data from", choices = calibration_list(), selected = NULL)
    
  })
  
  
  selected_calibration_out <- reactive({
    
    input$refresh
    
    if(is.null(input$which_cal))
      return(NULL)
    
    if(input$which_cal == "")
      return(NULL)
    
    #Find a list of calibrations that meet the given criteria
    check_basetable <- view_base_table()[1]
    reading_basetable <- view_base_table()[2]
    
    check_table <- tbl(dbcon, check_basetable) %>%
      filter(CAL_ID == local(input$which_cal))
    
    if(check_basetable == "GEN_CHECK") {
      check_table <- check_table %>%
        filter(PARAMETER == local(input$find_cal_parm))
    }
    
    if(reading_basetable != "")
      reading_table <- tbl(dbcon, reading_basetable)
    
    sensor_table <- tbl(dbcon, "SENSOR") %>%
      select(SENSOR_ID, SENSOR_SN)
    
    check <- check_table %>%
      inner_join(sensor_table) %>%
      collect()
    
    check_table <- check_table %>%
      select(ends_with("_ID"))
    
    if(reading_basetable != "") {
      reading <- check_table %>%
        inner_join(reading_table) %>%
        collect()
      
      out <- list(check, reading)
      names(out) <- c("check","reading")
      
    } else {
      
      out <- list(check, reading = NULL)
      names(out) <- c("check", "reading")
      
    }
    return(out)
    
  })
  
  selected_calibration_sn <- reactive({
    if(!is.null(check()))
      sensor_id <- check()$SENSOR_ID
    sensor_sn <- tbl(dbcon, "SENSOR") %>%
      filter(SENSOR_ID == sensor_id) %>%
      pull(SENSOR_SN)
    sensor_sn <- sensor_sn[1]
    return(sensor_sn)
  })
  
  check <- reactive({
    
    check <- selected_calibration_out()
    check <- check[["check"]]
    return(check)
  })
  
  readings <- reactive({
    
    readings <- selected_calibration_out()
    readings <- readings[["reading"]]
    return(readings)
  })
  
  output$view_check_out <- renderTable({
    
    if(!is.null(check())) {
      out <- check() %>%
        select(-ends_with("_ID"))
    } else {
      out <- NULL
    }
    
    out
    
  })
  
  output$view_reading_out <- renderTable({
    
    if(!is.null(readings())) {
      out <- readings() %>%
        select(-ends_with("_ID"))
    } else {
      out <- NULL
    }
    
    out
    
  })

  sc_edit <- callModule(manualSc, "sc_edit",
                        sn = reactive(input$find_cal_sn),
                        selected_check = reactive(check()),
                        selected_readings = reactive(readings()))
  tby_edit <- callModule(manualTby, "tby_edit",
                         sn = reactive(input$find_cal_sn),
                         selected_check = reactive(check()),
                         selected_readings = reactive(readings()))
  do_edit <- callModule(manualDo, "do_edit",
                        sn = reactive(input$find_cal_sn),
                        selected_check = reactive(check()),
                        selected_readings = reactive(readings()))
  ph_edit <- callModule(manualPh, "ph_edit",
                        sn = reactive(input$find_cal_sn),
                        selected_check = reactive(check()),
                        selected_readings = reactive(readings()))
  wt_edit_comp <- callModule(manualWt, "wt_edit_comp",
                             sn = reactive(input$find_cal_sn),
                             selected_check = reactive(check()),
                             selected_readings = reactive(readings()))
  wt_edit_multi <- callModule(manualWt, "wt_edit_multi",
                              sn = reactive(input$find_cal_sn),
                              selected_check = reactive(check()),
                              selected_readings = reactive(readings()))
  gen_edit <- callModule(manualGen, "gen_edit",
                         sn = reactive(selected_calibration_sn()),
                         selected_check = reactive(check()),
                         selected_readings = reactive(readings()))
  
  output$gen_edit_out <- renderPrint({
    print(gen_edit())
  })
  
  observeEvent(input$update, {

    max_keys <- get_max_keys(dbcon)
    
    if(input$find_cal_parm == "Specific cond at 25C") {
      edit <- sc_edit()
      reading_id <- "SCR_ID"
    } else if(input$find_cal_parm == "Turbidity, FNU") {
      edit <- tby_edit()
      reading_id <- "TBYR_ID"
    } else if(input$find_cal_parm == "Dissolved oxygen") {
      edit <- do_edit()
      reading_id <- "DOR_ID"
    } else if(input$find_cal_parm == "pH") {
      edit <- ph_edit()
      reading_id <- "PHR_ID"
    } else if(input$find_cal_parm == "Temperature, water (comparison)") {
      edit <- wt_edit_comp()
      reading_id <- ""
    } else if(input$find_cal_parm == "Temperature, water (multi-point)") {
      edit <- wt_edit_multi()
      reading_id <- "WTR_MULTIPOINT_ID"
    } else {
      edit <- gen_edit()
      reading_id <- "GENR_ID"
    }
    
    #Look up the sensor
    sensors <- tbl(dbcon, "SENSOR")
    sensor_sn <- edit[[view_base_table()[1]]]$SENSOR_ID
    sensor_id <- sensors %>%
      filter(SENSOR_SN == sensor_sn, PARAMETER == local(input$find_cal_parm)) %>%
      pull(SENSOR_ID)
    
    #If it's a new sensor, write it to the database
    if(length(sensor_id) == 0) {
      SENSOR_ID <- max_keys["SENSOR_ID"] + 1
      new_sensor <- data.frame(
        SENSOR_ID = SENSOR_ID,
        SENSOR_SN = sensor_sn,
        PARAMETER = input$find_cal_parm,
        MANUFACTURER = "",
        MODEL = ""
      )
      dbWriteTable(dbcon, "SENSOR", new_sensor, append = TRUE)
      sensor_id <- new_sensor$SENSOR_ID
    }
    
    edit[[view_base_table()[1]]]$SENSOR_ID <- sensor_id
    
    if(reading_id != "") {
      if(nrow(edit[[view_base_table()[2]]]) > 0) {
        edit[[view_base_table()[2]]][,reading_id] <- 
          1:nrow(edit[[view_base_table()[2]]]) + max_keys[reading_id]
      }
    }
    
    update(input$find_cal_parm, 
           edit[[view_base_table()[1]]], 
           edit[[view_base_table()[2]]], 
           dbcon)
    
  })
  
  observeEvent(input$delete_check, {
    
    max_keys <- get_max_keys(dbcon)
    
    if(input$find_cal_parm == "Specific cond at 25C") {
      edit <- sc_edit()
      reading_id <- "SCR_ID"
    } else if(input$find_cal_parm == "Turbidity, FNU") {
      edit <- tby_edit()
      reading_id <- "TBYR_ID"
    } else if(input$find_cal_parm == "Dissolved oxygen") {
      edit <- do_edit()
      reading_id <- "DOR_ID"
    } else if(input$find_cal_parm == "pH") {
      edit <- ph_edit()
      reading_id <- "PHR_ID"
    } else if(input$find_cal_parm == "Temperature, water (comparison)") {
      edit <- wt_edit_comp()
      reading_id <- ""
    } else if(input$find_cal_parm == "Temperature, water (multi-point)") {
      edit <- wt_edit_multi()
      reading_id <- "WTR_MULTIPOINT_ID"
    }
    
    delete(input$find_cal_parm, 
           edit[[view_base_table()[1]]], 
           edit[[view_base_table()[2]]], 
           dbcon,
           input$delete_keyword)
    
  })
  
  output$probe_history_parameter_ui <- renderUI({
    
    generic_parms <- tbl(dbcon, "GEN_CHECK") %>%
      pull(PARAMETER) %>%
      unique()
    generic_parms <- generic_parms[generic_parms != "Temperature, water"]
    
    ch = c("Specific cond at 25C", "Turbidity, FNU", "Dissolved oxygen", "pH", generic_parms)
    
    selectInput("probe_history_parameter", label = "Parameter", choices = ch)
    
  })

  output$probe_history_sn_choices <- renderUI({
    
    serial_number_choices <- tbl(dbcon, "SENSOR") %>%
      filter(PARAMETER == local(input$probe_history_parameter)) %>%
      pull(SENSOR_SN)
    
    selectInput("probe_history_sn", "Probe serial number", choices = serial_number_choices)
    
  })
  
  probe_history_plot <- reactive({
    
    plot <- error_history_plot(input$probe_history_parameter, input$probe_history_sn, dbcon)
    plot
    
  })
  
  probe_history_cal_list <- reactive({
    
    cal_list <- get_cal_list(input$probe_history_parameter, input$probe_history_sn, dbcon)
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