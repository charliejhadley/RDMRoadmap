# Comms Plan Timeline (Multi-day, non-repeating) for RDM Roadmap Dashboard

tabPanel("Communication Plans",
         fluidPage(
           fluidRow(column(
             6,uiOutput("commsCommTypesUI")
           ),
           column(
             6, uiOutput("commsCommSourcesUI")
           )),
           uiOutput(
             "commsPlanMultiDayUI", hover = "hover", click = "click", height = "auto"
           ),
           htmlOutput("commsPlanMultiDaySummary")
         ))