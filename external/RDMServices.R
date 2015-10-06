# Timeline for RDM Roadmap Dashboard

## Note that a more complicated version of this could be built from http://stackoverflow.com/q/19779665/1659890

navbarMenu(
  "RDM Services",
  tabPanel(
    "ORA Data Summary",
    source("external/project-summaries/ORA-Data.R", local = TRUE)$value
    )
  )