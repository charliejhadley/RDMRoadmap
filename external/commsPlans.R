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
           "RDM Training Events",
           fluidPage(
             wellPanel(
               HTML(paste(
                 "<p>This calendar heatmap visualises all RDM-related training courses currently listed on <a href =http://courses.it.ox.ac.uk>http://courses.it.ox.ac.uk</a></p>",
                 "<p>The colour of each day indicates the total number of events scheduled for that date, clicking on a date will ",
                 "provide a summary at the bottom of this page</p>",
                 "<p>As more data is obtained the available filtering options for this visualisation will be increased</p>"
               ,sep=""))
             ),
             uiOutput("trainingCategoriesUI"),
             uiOutput("trainingHeatmapUI"),
             uiOutput("trainingHeatmapSummary")
           )
         )
)