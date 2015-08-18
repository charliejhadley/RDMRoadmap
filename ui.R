## UI for RDM Roadmap Dashboard

# Packages to load
library(shinythemes)

# UI Elements


# Shiny UI

shinyUI(fluidPage(
  titlePanel("Research Data Management Roadmap"),
  navbarPage(theme = shinytheme("cerulean"),
             "",
             source("external/about.R", local = TRUE)$value,
             source("external/timeline.R", local = TRUE)$value,
             source("external/budget-comparison.R", local = TRUE)$value
)
))
