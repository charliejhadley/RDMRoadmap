## UI for RDM Roadmap Dashboard

# Packages to load
library(shinythemes)


shinyUI(fluidPage(
  tags$style(type="text/css", "body { overflow-y: scroll; }"),
  theme = shinytheme("cerulean"),
  navbarPage(
    theme = shinytheme("cerulean"),
    "Research Data Management Roadmap",
    source("external/about.R", local = TRUE)$value,
    #             source("external/data-lifecycle.R", local = TRUE)$value,
    source("external/project-timeline.R", local = TRUE)$value,
    source("external/commsPlans.R", local = TRUE)$value,
    source("external/resource-comparison.R", local = TRUE)$value
  )
))
