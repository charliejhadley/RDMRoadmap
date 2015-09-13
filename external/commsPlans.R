# Comms Plan Timeline (Multi-day, non-repeating) for RDM Roadmap Dashboard

navbarMenu("Comms & Events",
           tabPanel(
             "Communication Plans",
         fluidPage(
           wellPanel(
             HTML("This Gantt Chart visualises all available Communication Plans for projects relating to RDM at Oxford University.</p>
                  Selecting an entry in the Gantt Chart will provide a summary at the bottom of this page.</p>
                  By default, all Comms Plans and Types are displayed. 
                  To filter, simply select and delete items below. Items can be re-added by typing into the field.</p>
                  Note that the dashed black line represents today.
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
         )),
         tabPanel(
           "RDM Events",
           fluidPage(
             wellPanel(
               HTML(paste(
                 "<h1>BETA</h1></p>",
                 "This calendar heatmap will display RDM events pulled from a variety of sources, the data displayed is not currently accurate"
               ,sep=""))
             ),
             uiOutput("heatmapUI"),
             uiOutput("heatmapSummary")
           )
         )
)