# HaugShape v2 Embedded Shiny App (installed under inst/app)

# Note: This app composes the exported modules from the HaugShape_v2 package.
# It is bundled into the installed package so users can launch it via
# HaugShape_v2::run_haug_app().

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "HaugShape v2"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("1. Image Processing", tabName = "image_processing", icon = icon("images")),
      menuItem("2. Shape Analysis",   tabName = "shape_analysis",   icon = icon("project-diagram")),
      menuItem("3. Data Import",      tabName = "data_import",      icon = icon("table")),
      menuItem("4. Plotting",         tabName = "plotting",         icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
  tabItem(tabName = "image_processing", HaugShape_v2::image_processing_ui("img")),
  tabItem(tabName = "shape_analysis",   HaugShape_v2::shape_analysis_ui("sa")),
  tabItem(tabName = "data_import",      HaugShape_v2::data_import_ui("di")),
  tabItem(tabName = "plotting",         HaugShape_v2::plotting_ui("pl"))
    )
  )
)

server <- function(input, output, session) {
  # Initialize modules
  HaugShape_v2::image_processing_server("img")
  HaugShape_v2::shape_analysis_server("sa")

  # Data Import provides data for plotting
  imported <- HaugShape_v2::data_import_server("di")  # list with $data reactive
  HaugShape_v2::plotting_server("pl", data_reactive = imported$data)
}

shinyApp(ui, server)
