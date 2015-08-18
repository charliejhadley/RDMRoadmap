# Timeline for RDM Roadmap Dashboard

tabPanel("Project Timeline",
         fluidPage(
         uiOutput("plotui"),
         ## Selected point output,
         uiOutput("projSummary"),
         dataTableOutput("selecProject")
         )
)