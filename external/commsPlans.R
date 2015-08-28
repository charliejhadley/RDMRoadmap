# Comms Plan Timeline (Multi-day, non-repeating) for RDM Roadmap Dashboard

tabPanel("Communication Plans",
         fluidPage(
           uiOutput("commsPlanMultiDayUI", hover = "hover", click = "click", height="auto"),
           htmlOutput("commsPlanMultiDaySummary")
         )
)