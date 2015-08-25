# Timeline for RDM Roadmap Dashboard

## Note that a more complicated version of this could be built from http://stackoverflow.com/q/19779665/1659890

tabPanel("Project Timeline",
         fluidPage(
           fluidRow(column(
             6,sliderInput(
               "projTimelineRange", "Range:",
               min = as.numeric(format(earliest.proj.start,"%Y")),
               max = as.numeric(format(lastest.proj.end,"%Y")),
               step = 1,
               value = c(as.numeric(format(
                 earliest.proj.start,"%Y"
               )),as.numeric(format(
                 lastest.proj.end,"%Y"
               )))
             )
           ),
           column(
             6,selectInput(
               "projTimelineFilter", "Choose a dataset:",
               choices = c("rock", "pressure", "cars")
             )
           )),
           uiOutput("projtimelineUI"),
           ## Selected point output,
           htmlOutput("projTimelineSummary")
         ))