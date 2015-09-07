# Treemap for RDM Roadmap Dashboard

tabPanel("Resource Comparison",
         fluidPage(
           plotOutput("treemapUI", hover = "hover", click = "click"),
           htmlOutput("treemapSummary")
         )
)