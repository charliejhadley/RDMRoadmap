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
             source("external/project-timeline.R", local = TRUE)$value,
             source("external/resource-comparison.R", local = TRUE)$value
)
))
