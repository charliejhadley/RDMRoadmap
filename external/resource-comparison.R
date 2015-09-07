# Treemap for RDM Roadmap Dashboard

tabPanel("Resource Comparison",
         fluidPage(
           uiOutput("resourceTreemapUI"),
           htmlOutput("resourceTreemapSummary")
         )
)