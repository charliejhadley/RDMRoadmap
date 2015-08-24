## UI for RDM Roadmap Dashboard

# Packages to load
library(shinythemes)

## Useful variables

earliest.proj.start <- min(projects.df$Project.Start.Date)
lastest.proj.end <- max(projects.df$Project.End.Date)

# UI Elements


# Shiny UI

shinyUI(fluidPage(
  titlePanel("Research Data Management Roadmap"),
  navbarPage(theme = shinytheme("cerulean"),
             "",
             source("external/about.R", local = TRUE)$value,
             source("external/timeline.R", local = TRUE)$value,
             source("external/BudgetTreemap.R", local = TRUE)$value
)
))
