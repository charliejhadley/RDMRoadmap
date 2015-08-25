# App body for RDM Roadmap Dashboard

# ===================== Project Timeline Outputs ===============================
output$projtimelineUI <- renderUI({
  plotOutput("projtimeline", height = 300,
             click = "projtimeline_click")
})

output$projtimeline <- renderPlot({
  dat <-
    projects.df[projects.df$Project.Start.Date >= paste(input$projTimelineRange[1],"01","01",sep =
                                                          "-") &
                  projects.df$Project.End.Date <= paste(input$projTimelineRange[2],"12","31",sep =
                                                          "-"),]
  if (nrow(dat) == 0)
    return(NULL)
  base <- ggplot(
    dat,
    aes(
      Project.Short.Name, Project.Start.Date, ymin = Project.Start.Date,color =
        as.factor(IT.Board),ymax = Project.End.Date,xticks
    )
  ) + xlab("Project.Short.Name")
  base + geom_linerange(size = 4,alpha = .7) +  coord_flip()  + scale_colour_brewer(palette = "Spectral")
})


## selected timeline project summary:
output$projTimelineSummary <- renderUI({
  if (is.null(input$projtimeline_click$y))
    return()
  projData <-
    projects.df[projects.df$Project.Start.Date >= paste(input$projTimelineRange[1],"01","01",sep =
                                                          "-") &
                  projects.df$Project.End.Date <= paste(input$projTimelineRange[2],"12","31",sep =
                                                          "-"),]
  
  projData <-
    projData[order(projData$Budget.Requested)[round(input$projtimeline_click$y)],]
  
  #  projData <- projData[round(input$projtimeline_click$y),]
  wellPanel(titlePanel(projData$Project.Short.Name),
            HTML(paste(
              "<b>Budget Requested:</b> £",as.character(projData$Budget.Requested),"<p>"
            )),
            HTML(paste("<a href='",projData$Dummy.Link,"'>",projData$Dummy.Link,"</a><p>")),
            HTML(newlineFn(projData$Project.Summary)))
})

# ===================== Budget Treemap Outputs ===============================

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
    wellPanel(titlePanel(projData$Project.Short.Name),
              HTML(paste(
                "<b>Budget Requested:</b> £",as.character(projData$Budget.Requested),"<p>"
              )),
              HTML(newlineFn(projData$Project.Summary)))
  })
