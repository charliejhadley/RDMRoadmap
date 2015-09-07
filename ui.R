## UI for RDM Roadmap Dashboard

# Packages to load
library(shinythemes)

## Useful variables

# earliest.proj.start <- min(projects.df$Project.Start.Date)
# lastest.proj.end <- max(projects.df$Project.End.Date)
# allCommsTypes <- levels(commsplanMultiDay.df$Comms.Type)
# allCommsSources <- levels(commsplanMultiDay.df$Source)
# allITBoards <- levels(projects.df$IT.Board)

# UI Elements


# Shiny UI

shinyUI(fluidPage(
  tags$style(type="text/css", "body { overflow-y: scroll; }"),
  theme = shinytheme("cerulean"),
  titlePanel("Research Data Management Roadmap"),
  navbarPage(
    theme = shinytheme("cerulean"),
    "",
    source("external/about.R", local = TRUE)$value,
    #             source("external/data-lifecycle.R", local = TRUE)$value,
    source("external/project-timeline.R", local = TRUE)$value,
    source("external/commsPlans.R", local = TRUE)$value,
    source("external/resource-comparison.R", local = TRUE)$value
  )
))
