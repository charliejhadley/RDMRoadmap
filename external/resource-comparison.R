# Treemap for RDM Roadmap Dashboard

navbarMenu("RDM Resources",
           tabPanel("Project Budget Comparison",
         fluidPage(
           wellPanel(
             HTML("This Treemap displays all RDM-related projects included in the 'Projects Timeline', 
                  the relative size of each project is calculated from the requested budget for the project.</p>
                  Selecting a project will display a short summary below the Treemap.</p>
                  Only projects that start and end within the selected range will be displayed.
                  ")
             ),
           uiOutput("resourceTreemapTimeSliderUI"),
           uiOutput("resourceTreemapUI"),
           htmlOutput("resourceTreemapSummary")
         ))
)