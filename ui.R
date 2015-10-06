## UI for RDM Roadmap Dashboard

# Packages to load
library(shinythemes)


shinyUI(fluidPage(
  tags$head(includeScript("google-analytics.js")),
  tags$style(type="text/css", "body { overflow-y: scroll; }"),
  theme = shinytheme("cerulean"),
  navbarPage(
    theme = shinytheme("cerulean"),
    "Research Data Management Roadmap",
    source("external/AboutRDM.R", local = TRUE)$value,
    source("external/RDMServices.R", local = TRUE)$value,
    source("external/RDMProjects.R", local = TRUE)$value,
    source("external/RDMComms&Training.R", local = TRUE)$value,
    source("external/RDMResources.R", local = TRUE)$value
  )
))
