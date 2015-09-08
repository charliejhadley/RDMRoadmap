# Treemap for RDM Roadmap Dashboard

navbarMenu("RDM Resources",
           tabPanel("Project Budget Comparison",
         fluidPage(
           wellPanel(
             HTML("This Treemap displays all RDM-related projects included in the 'Projects Timeline', 
                  the relative size of each project is calculated from the requested budget for the project.</p>
                  Selecting a project will display a short summary below the Treemap.</p>
                  ")
             ),
           uiOutput("resourceTreemapUI"),
           htmlOutput("resourceTreemapSummary")
         )),
         tabPanel("Projects Split by Staff",
                  "I've not been implemented yet")
)