shinyApp(
  ui = fluidPage(
    uiOutput("projectsDataTableColUI"),
    DT::dataTableOutput("projectsDataTable")
  ),
  
  server = function(input, output, session){
    output$projectsDataTableColUI <-
      renderUI(selectInput(
        'proj_Cols', 'Columns to show:',
        names(projects.df)[4:(ncol(projects.df) -
                                2)], selected = c(
                                  "Project.Short.Name","Project.Start.Date",
                                  "Project.End.Date"
                                ),
        multiple = TRUE
      ))
    
    output$projectsDataTable <-
      DT::renderDataTable(projects.df[, input$proj_Cols, drop = FALSE],
                          filter = "top")
    gs_key("1I6Z94prfJrmSSmD_mwqazkp8Qx8AUmmsp9hAW6bF8yQ",
           visibility = "public")
    projects.df <- gs_read(projects.workbook, ws = "Approved-Data")
    # projects.df$Project.Start.Date <-
    #   as.POSIXct(projects.df$Project.Start.Date)
    # projects.df$Project.End.Date <-
    #   as.POSIXct(projects.df$Project.End.Date)
    projects.df$Project.Start.Date <- {
      ymd <- ymd(projects.df$Project.Start.Date)
      mdy <- mdy(projects.df$Project.Start.Date)
      ymd[is.na(ymd)] <- mdy[is.na(ymd)]
      force_tz(ymd, tzone = "GMT")
    }
    projects.df$Project.End.Date <- {
      ymd <- ymd(projects.df$Project.End.Date)
      mdy <- mdy(projects.df$Project.End.Date)
      ymd[is.na(ymd)] <- mdy[is.na(ymd)]
      force_tz(ymd, tzone = "GMT")
    }
    projects.df$Project.Short.Name <-
      as.character(projects.df$Project.Short.Name)
    projects.df$IT.Board <- as.factor(projects.df$IT.Board)
    projects.df$Project.Sponsor <-
      as.factor(projects.df$Project.Sponsor)
    projects.df$Project.Manager <-
      as.factor(projects.df$Project.Manager)
    projects.df$Senior.Supplier <-
      as.factor(projects.df$Senior.Supplier)
    projects.df$Senior.User <- as.factor(projects.df$Senior.User)
    ## Re-order data.frame according to this: http://stackoverflow.com/a/32333974/1659890
    projects.df <-
      projects.df[order(
        projects.df$Project.Start.Date, projects.df$Project.Short.Name
      ),]
    projects.df$projectID <- as.factor(nrow(projects.df):1)
  }
)