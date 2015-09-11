# Timeline for RDM Roadmap Dashboard

## Note that a more complicated version of this could be built from http://stackoverflow.com/q/19779665/1659890

navbarMenu(
  "RDM Projects",
  tabPanel(
    "Projects Description",
    fluidPage(
      navlistPanel(
        "",
        tabPanel("RDM Projects",
                 uiOutput("projSummaryText")
        ),
        tabPanel("Participant Data Project",
                 HTML(
                   paste("<p>The Participant Data Project commenced in September 2015 with the aim of scoping 
                   and implementing improved support for researchers working with human participants, 
                   chiefly in the Medical and Social Sciences.</p>",
                   "<p><a href='http://blogs.it.ox.ac.uk/acit-rs-team/advice/research-data-management/participant-data-project'>Read more...</a>"
                 ,sep="")
                   )
        )
      )
    )
  ),
  tabPanel(
    "Projects Timeline",
    fluidPage(
      wellPanel(
        HTML(
          "This Gantt Chart visualises all known projects (past, present and future) relating to RDM undertaken by Oxford University.</p>
          Selecting an entry in the Gantt Chart will provide a summary at the bottom of this page.</p>
          By default, projects from all IT Boards are displayed. To filter, simply select and delete items below.
          Items can be re-added by typing into the field.</p>
          Note that the dashed black line represents today.
          "
        )
        ),
      fluidRow(column(6,uiOutput("projTimeSliderUI")),
               column(6, uiOutput("projITBoardUI"))),
      uiOutput("projtimelineUI"),
      ## Selected point output,
      htmlOutput("projTimelineSummary")
        )
    ),
  tabPanel("Projects Spreadsheet",
           fluidPage(
             uiOutput("projectsDataTableColUI"),
             dataTableOutput("projectsDataTable")
           ))
    )