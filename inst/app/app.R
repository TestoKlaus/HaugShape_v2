# HaugShape v2 Embedded Shiny App (installed under inst/app)

# Note: This app composes the exported modules from the HaugShapeV2 package.
# It is bundled into the installed package so users can launch it via
# HaugShapeV2::run_haug_app().

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
      menuItem("4. Plotting",         tabName = "plotting",         icon = icon("chart-line")),
      menuItem("5. Overview",         tabName = "overview",         icon = icon("th-large"))
    )
  ),
  dashboardBody(
    tabItems(
  tabItem(tabName = "image_processing", HaugShapeV2::image_processing_ui("img")),
  tabItem(tabName = "shape_analysis",   HaugShapeV2::shape_analysis_ui("sa")),
  tabItem(tabName = "data_import",      HaugShapeV2::data_import_ui("di")),
  tabItem(tabName = "plotting",         HaugShapeV2::plotting_ui("pl")),
  tabItem(tabName = "overview",         HaugShapeV2::overview_ui("ov"))
    )
  )
)

server <- function(input, output, session) {
  # Initialize modules
  HaugShapeV2::image_processing_server("img")
  HaugShapeV2::shape_analysis_server("sa")

  # Data Import provides data for plotting
  imported <- HaugShapeV2::data_import_server("di")  # list with $data reactive
  HaugShapeV2::plotting_server("pl", data_reactive = imported$data)
  HaugShapeV2::overview_server("ov", data_reactive = imported$data)
}

shinyApp(ui, server)
