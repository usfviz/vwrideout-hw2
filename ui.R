library(shiny)
library(ggvis)

ui <- fluidPage(
  headerPanel('Life Expectancy and Fertility Rate by Country'),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)