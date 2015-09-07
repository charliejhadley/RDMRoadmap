# App body for RDM Roadmap Dashboard

## ======== Life cycle Diagram =============

getLifeCycle<-function() {
  return(includeHTML("external/lifecycleTest.html"))
}

output$lifeCycle<-renderUI({getLifeCycle()})

# ===================== Project Timeline Outputs ===============================
output$projtimelineUI <- renderUI({
  plotOutput("projtimeline", height = 300,
             click = "projtimeline_click")
})

output$projtimeline <- renderPlot({
  proj.df <- subset(projects.df, subset = IT.Board %in% input$selITBoard)
  
  proj.df <-
    proj.df[proj.df$Project.Start.Date >= paste(input$projTimelineRange[1],"01","01",sep =
                                                          "-") &
                  proj.df$Project.End.Date <= paste(input$projTimelineRange[2],"12","31",sep =
                                                          "-"),]
  
  if (nrow(proj.df) == 0)
    return(NULL)
  
  base <-
    ggplot(proj.df, aes(x = Project.Start.Date, y = taskID, color = as.factor(IT.Board)))
  base +
    scale_y_discrete(breaks = proj.df$taskID, labels = proj.df$Project.Short.Name) +
    geom_segment(aes(xend = Project.End.Date, y = taskID, yend = taskID), size = 5)
  
## OLD WORKING
#   base <- ggplot(
#     proj.df,
#     aes(
#       Project.Short.Name, Project.Start.Date, ymin = Project.Start.Date,color =
#         as.factor(IT.Board),ymax = Project.End.Date,xticks,order = Budget.Requested
#     )
#   ) + xlab("Project.Short.Name")
#   base + geom_linerange(size = 4,alpha = .7) +  coord_flip()  + scale_colour_brewer(palette = "Spectral")
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
  
  wellPanel(
    titlePanel(projData$Project.Short.Name),
    HTML(paste(
      "<b>Budget Requested:</b> £",as.character(projData$Budget.Requested),"<p>"
    )),
    HTML(
      paste(
        "<a href='",projData$Dummy.Link,"'>",projData$Dummy.Link,"</a><p>"
      )
    ),
    HTML(newlineFn(projData$Project.Summary))
  )
})


# ===================== Comms Plan Timelines ================================

output$commsPlanMultiDayUI <- renderUI({
  plotOutput("commsPlanMultiDay", height = 500,
             click = "commsPlanMultiDay_click")
})

output$commsPlanMultiDay <- renderPlot({
  comms.df <-
    subset(
      commsplanMultiDay.df, Comms.Type %in% input$selCommsType &
        Source %in% input$selCommsSource
    )
  if (nrow(comms.df) == 0)
    return()
  
  base <-
    ggplot(comms.df, aes(x = Start.Date, y = taskID, color = Comms.Type))
  base +
    scale_y_discrete(breaks = NULL) +
    geom_segment(aes(xend = End.Date, y = taskID, yend = taskID), size = 5) +
    facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE)
})

output$commsPlanMultiDaySummary <- renderUI({
  if (is.null(input$commsPlanMultiDay_click$y))
    return()
  comms.df <- commsplanMultiDay.df
  
  slction <-
    subset(comms.df, subset = Source == input$commsPlanMultiDay_click$panelvar1)
  slction <- slction[rev(order(slction$Start.Date,slction$Action)),]
  slction <- slction[round(input$commsPlanMultiDay_click$y),]
  
  wellPanel(titlePanel(slction$Action),
            HTML(paste(
              "<b>Comms Type:</b>",as.character(slction$Comms.Type),"<p>"
            )),
            HTML(paste(
              "<b>Start Date:</b>",as.character(slction$Start.Date),"<p>"
            )),
            HTML(paste(
              "<b>End Date:</b>",as.character(slction$End.Date),"<p>"
            )))
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
