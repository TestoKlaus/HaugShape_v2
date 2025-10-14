library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "HaugShape Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Shape Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Overview Plots", tabName = "overview", icon = icon("chart-area")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Shape Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Shape Analysis Settings", status = "primary", solidHeader = TRUE,
            width = 4,
            fileInput("shapeFiles", "Upload Shape Files",
                     multiple = TRUE,
                     accept = c(".jpg", ".jpeg", ".png")),
            checkboxInput("normalize", "Normalize shapes", TRUE),
            numericInput("numPCs", "Number of PCs to display", 
                        value = 10, min = 1, max = 50),
            actionButton("runAnalysis", "Run Analysis", class = "btn-primary")
          ),
          box(
            title = "Results", status = "success", solidHeader = TRUE,
            width = 8,
            plotOutput("pcPlot"),
            br(),
            downloadButton("downloadResults", "Download Results", class = "btn-success")
          )
        )
      ),
      
      # Overview Plots Tab  
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Overview Plot Settings", status = "primary", solidHeader = TRUE,
            width = 4,
            fileInput("dataFile", "Upload Data File",
                     accept = c(".csv", ".xlsx", ".xls")),
            selectInput("groupCol", "Group Column:", choices = NULL),
            selectInput("plotCols", "Columns to Plot:", choices = NULL, multiple = TRUE),
            actionButton("createOverview", "Create Overview", class = "btn-primary")
          ),
          box(
            title = "Overview Plots", status = "success", solidHeader = TRUE,
            width = 8,
            plotOutput("overviewPlot", height = "600px")
          )
        )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
        fluidRow(
          box(
            title = "About HaugShape", status = "info", solidHeader = TRUE,
            width = 12,
            h4("HaugShape: Morphometric Shape Analysis"),
            p("HaugShape is a comprehensive toolkit for morphometric shape analysis, including:"),
            tags$ul(
              tags$li("Elliptical Fourier Analysis (EFA)"),
              tags$li("Principal Component Analysis (PCA)"),
              tags$li("Advanced visualization tools"),
              tags$li("Statistical analysis capabilities")
            ),
            h4("Getting Started"),
            p("1. Upload your shape files or data in the respective tabs"),
            p("2. Configure the analysis settings"),
            p("3. Run the analysis and download results"),
            br(),
            p("For more information, visit: "),
            a("https://github.com/TestoKlaus/HaugShape_v2", 
              href = "https://github.com/TestoKlaus/HaugShape_v2", 
              target = "_blank")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  analysisResults <- reactiveVal(NULL)
  uploadedData <- reactiveVal(NULL)
  
  # Update column choices when data is uploaded
  observeEvent(input$dataFile, {
    req(input$dataFile)
    
    # Read the uploaded file
    ext <- tools::file_ext(input$dataFile$datapath)
    if(ext == "csv") {
      data <- read.csv(input$dataFile$datapath)
    } else if(ext %in% c("xlsx", "xls")) {
      data <- openxlsx::read.xlsx(input$dataFile$datapath)
    }
    
    uploadedData(data)
    
    # Update column choices
    updateSelectInput(session, "groupCol", 
                     choices = c("None" = "", names(data)))
    updateSelectInput(session, "plotCols", 
                     choices = names(data))
  })
  
  # Run shape analysis
  observeEvent(input$runAnalysis, {
    # Placeholder for shape analysis
    # You'll implement this with your actual shape_analysis function
    showNotification("Shape analysis completed!", type = "success")
  })
  
  # Create overview plots
  observeEvent(input$createOverview, {
    # Placeholder for overview creation
    # You'll implement this with your actual Haug_overview function
    showNotification("Overview plots created!", type = "success")
  })
  
  # Placeholder outputs
  output$pcPlot <- renderPlot({
    plot(1:10, main = "PC Contributions Plot", 
         xlab = "Principal Component", ylab = "Contribution")
  })
  
  output$overviewPlot <- renderPlot({
    plot(1:10, main = "Overview Plot Placeholder", 
         xlab = "X", ylab = "Y")
  })
  
}