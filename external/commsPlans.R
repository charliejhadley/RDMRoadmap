# Comms Plan Timeline (Multi-day, non-repeating) for RDM Roadmap Dashboard

tabPanel("Communication Plans",
         fluidPage(
           fluidRow(column(
             6,selectInput(
               'selCommsType', 'Communication Types', allCommsTypes, selected = allCommsTypes,  multiple =
                 TRUE, selectize = TRUE
             )
           ),
           column(
             6, selectInput(
               'selCommsSource', 'Communication Plans', allCommsSources, selected = allCommsSources,  multiple =
                 TRUE, selectize = TRUE
             )
           )),
           uiOutput(
             "commsPlanMultiDayUI", hover = "hover", click = "click", height = "auto"
           ),
           htmlOutput("commsPlanMultiDaySummary")
         ))