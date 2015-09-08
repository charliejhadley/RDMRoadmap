# Comms Plan Timeline (Multi-day, non-repeating) for RDM Roadmap Dashboard

tabPanel("Communication Plans",
         fluidPage(
           wellPanel(
             HTML("This Gantt Chart visualises all available Communication Plans for projects relating to RDM at Oxford University.</p>
                  Selecting an entry in the Gantt Chart will provide a summary at the bottom of this page.</p>
                  By default, all Comms Plans and Types are displayed. 
                  To filter, simply select and delete items below. Items can be re-added by typing into the field
                  ")
             ),
           fluidRow(
             column(4,uiOutput("commsTimeSliderUI")),
             column(4,uiOutput("commsCommTypesUI")),
             column(4, uiOutput("commsCommSourcesUI"))
           ),
           uiOutput(
             "commsPlanMultiDayUI", hover = "hover", click = "click", height = "auto"
           ),
           htmlOutput("commsPlanMultiDaySummary")
         ))