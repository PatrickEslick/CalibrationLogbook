library(shiny)
library(shinydashboard)
source("tools.R")

ui <- dashboardPage(
  dashboardHeader(title = "Calibration Logbook"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("New calibration", startExpanded = TRUE,
        menuSubItem("From XML", tabName = "new_cal_xml"),
        menuSubItem("Manual", tabName = "new_cal_manual")
      )
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "new_cal_xml",
        box(
          fluidRow(
              
            column(4,
                     
              fileInput("xml_file", label = "SVMAQ XML file"),
              actionButton("write_db", "Record file")
                     
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
                
            textInput("sc_sensor_sn", "Sensor serial number", placeholder = "12A34567"),
            fluidRow(
              column(2,
                textInput("sc_cell_constant", "Cell constant")    
              ),
              column(2,
                textInput("sc_air_reading", "Reading in air")
              )
            ),
            textInput("sc_comment", "Comment", width = "65%"),
            
            uiOutput("sc_reading_ui")
            
            
          ),
          tabPanel("Turbidity, FNU"),
          tabPanel("Dissolved oxygen"),
          tabPanel("pH")
        )       
      )
    )
  )
)

server <- function(input, output) { 
  
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
  
  observeEvent(input$write_db, {
    
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
  
  output$sc_reading_ui <- renderUI({
    
    tabsetPanel(
      tabPanel("Check/Pre-calibration"),
      tabPanel("Post-calibration")
    )
    
  })
  
}

shinyApp(ui, server)