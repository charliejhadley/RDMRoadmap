# Timeline for RDM Roadmap Dashboard

## Note that a more complicated version of this could be built from http://stackoverflow.com/q/19779665/1659890

tabPanel("Project Timeline",
         fluidPage(
           fluidRow(column(
             6,uiOutput("projTimeSliderUI")
           ),
           column(
             6, uiOutput("projITBoardUI")
           )),
           uiOutput("projtimelineUI"),
           ## Selected point output,
           htmlOutput("projTimelineSummary")
         ))