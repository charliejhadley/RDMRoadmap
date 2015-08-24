# library(shiny)
# library(googlesheets)
# library(treemap)
#
tmLocate <- function(coor, tmSave) {
  tm <- tmSave$tm
  
  # retrieve selected rectangle
  rectInd <- which(tm$x0 < coor[1] &
                     (tm$x0 + tm$w) > coor[1] &
                     tm$y0 < coor[2] &
                     (tm$y0 + tm$h) > coor[2])
  
  return(tm[rectInd[1],])
}
e <- environment()

#
# # Load data
#
# projects.sheet <-
#   gs_key("1I6Z94prfJrmSSmD_mwqazkp8Qx8AUmmsp9hAW6bF8yQ",
#          visibility = "public")
# projects.df <- gs_read(projects.sheet)
# ## Coerce Types correctly:
# projects.df$Project.Start.Date <-
#   as.POSIXct(projects.df$Project.Start.Date)
# projects.df$Project.End.Date <-
#   as.POSIXct(projects.df$Project.End.Date)
# projects.df$Project.Short.Name <-
#   as.factor(projects.df$Project.Short.Name)
# projects.df$IT.Board <- as.factor(projects.df$IT.Board)
# projects.df$Project.Sponsor <-
#   as.factor(projects.df$Project.Sponsor)
# projects.df$Project.Manager <-
#   as.factor(projects.df$Project.Manager)
# projects.df$Senior.Supplier <-
#   as.factor(projects.df$Senior.Supplier)
# projects.df$Senior.User <- as.factor(projects.df$Senior.User)
# str(projects.df)



shinyApp(
  ui = fluidPage(
    plotOutput("treemapUI", hover = "hover", click = "click"),
    htmlOutput("treemapSummary")
  ),
  
  server = function(input, output, session) {
    
    ## getTreemapClickID creates a dataframe for me
    getTreemapClickID <- reactive({
      x <- input$click$x
      y <- input$click$y
      if (!is.null(tm)) {
        x <- (x - tm$vpCoorX[1]) / (tm$vpCoorX[2] - tm$vpCoorX[1])
        y <- (y - tm$vpCoorY[1]) / (tm$vpCoorY[2] - tm$vpCoorY[1])
        l <- tmLocate(list(x = x, y = y), tm)
        if (is.na(l[1,1])) {
          return(NULL)
        } else
          return(as.list(l[1,]))
      } else {
        return(NULL)
      }
    })
    
    getTreemapData <- reactive({
      l <- getTreemapClickID()
      
      # create summary line on hover
      sizeID <- which(names(l) == "vSize")
      id <- switch(
        "value",
        comp = sizeID + 2,
        dens = sizeID + 2,
        value = sizeID + 1,
        index = sizeID,
        categorical = sizeID + 1,
        depth = sizeID,
        color = sizeID
      )
      l <- l[1:id]
      names(l)[sizeID] <- "Budget.Requested"
      dt <- as.data.frame(l)
    })
    
    output$treemapUI <- renderPlot({
      tm <- treemap(
        projects.df,
        index = "Project.Short.Name",
        vSize = "Budget.Requested",
        vColor = "Budget.Requested",
        type = "value",
        title = ""
      )
      assign("tm", tm, envir = e)
    })
    
    output$treemapSummary <-
      renderUI({
        projData <- getTreemapData()
      if (is.null(input$click$x))
        return()
      wellPanel(
        titlePanel(projData$Project.Short.Name),
        HTML(paste(
          "<b>Budget Requested:</b> £",as.character(projData$Budget.Requested),"<p>"
        )),
        HTML(newlineFn(projData$Project.Summary))
      )}
      )
    
    }
)
