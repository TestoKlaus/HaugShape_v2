# HaugShape v2 - Minimal Starting Point
# Clean Shiny app with empty Data Import tab

library(shiny)
library(shinydashboard)
library(DT)

# Source module files if running as a standalone app script
module_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in module_files) try(source(f, local = TRUE), silent = TRUE)

# Define UI
ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "HaugShape v2 - Morphometric Analysis"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Data Import", tabName = "import", icon = icon("upload")),
      menuItem("2. Image Processing", tabName = "images", icon = icon("image")),
      menuItem("3. Shape Analysis", tabName = "shape", icon = icon("chart-area"))
    )
  ),
  
  # Body
  dashboardBody(
    tabItems(
      
      # Data Import Tab
      tabItem(tabName = "import",
        data_import_ui("import_excel")
      ),
      
      # Image Processing Tab
      tabItem(tabName = "images",
        image_processing_ui("img_proc")
      )
      ,
      # Shape Analysis Tab
      tabItem(tabName = "shape",
        shape_analysis_ui("shape_an")
      )
      
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Initialize data import module
  imported <- data_import_server("import_excel")

  # Example: observe when data is available
  observeEvent(imported$data(), {
    df <- imported$data()
    if (!is.null(df)) {
      message(sprintf("Imported %d rows and %d columns", nrow(df), ncol(df)))
    }
  })

  # Initialize image processing module
  image_processing_server("img_proc")

  # Initialize shape analysis module
  shape_analysis_server("shape_an")
}

# Run the application
shinyApp(ui = ui, server = server)