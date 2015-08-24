# Treemap for RDM Roadmap Dashboard

tabPanel("Budget Comparison",
         fluidPage(
           plotOutput("treemapUI", hover = "hover", click = "click"),
           htmlOutput("treemapSummary")
         )
)