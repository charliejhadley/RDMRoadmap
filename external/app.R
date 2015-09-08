# App body for RDM Roadmap Dashboard

## ================ Projects Sheet Loading and Manipulation ==================
projects.sheet <-
  gs_key("1I6Z94prfJrmSSmD_mwqazkp8Qx8AUmmsp9hAW6bF8yQ",
         visibility = "public")
projects.df <- gs_read(projects.sheet, ws = "Approved-Data")
projects.df$Project.Start.Date <-
  as.POSIXct(projects.df$Project.Start.Date)
projects.df$Project.End.Date <-
  as.POSIXct(projects.df$Project.End.Date)
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
## Note use of `rev` to accommodate use of ggplot later
projects.df <-
  projects.df[rev(order(
    projects.df$Budget.Requested, projects.df$Project.Short.Name
  )),]
projects.df$projectID <- as.factor(nrow(projects.df):1)

## ==================  Comms Plan (Multi-day, non-repeating) ============================
commsplanMultiDay.sheet <-
  gs_key("1ZWxJhJM9p6UaoQnBOHUsuyfht40OaEw4qKybVdFtjTc",
         visibility = "public")
commsplanMultiDay.df <<- gs_read(commsplanMultiDay.sheet)
commsplanMultiDay.df$Action <<-
  as.factor(commsplanMultiDay.df$Action)
commsplanMultiDay.df$Start.Date <<-
  as.POSIXct(commsplanMultiDay.df$Start.Date)
commsplanMultiDay.df$End.Date <<-
  as.POSIXct(commsplanMultiDay.df$End.Date)
commsplanMultiDay.df$Comms.Type <<-
  as.factor(commsplanMultiDay.df$Comms.Type)
commsplanMultiDay.df$Source <<-
  as.factor(commsplanMultiDay.df$Source)
commsplanMultiDay.df$Department <<-
  as.factor(commsplanMultiDay.df$Department)
commsplanMultiDay.df$Audience <<-
  as.factor(commsplanMultiDay.df$Audience)
## Re-order data.frame according to this: http://stackoverflow.com/a/32333974/1659890
commsplanMultiDay.df$Action <<-
  as.character(commsplanMultiDay.df$Action)
commsplanMultiDay.df <<-
  commsplanMultiDay.df[order(commsplanMultiDay.df$Start.Date, commsplanMultiDay.df$Action),]
commsplanMultiDay.df$taskID <<-
  as.factor(nrow(commsplanMultiDay.df):1)

## ======== Life cycle Diagram =============

# getLifeCycle<-function() {
#   return(includeHTML("external/lifecycleTest.html"))
# }
#
# output$lifeCycle<-renderUI({getLifeCycle()})

# ===================== Project Timeline Outputs ===============================

earliest.proj.start <-
  reactive(year(min(projects.df$Project.Start.Date)))
latest.proj.start <-
  reactive(year(max(projects.df$Project.End.Date)))
allITBoards <- reactive(levels(projects.df$IT.Board))

output$projTimeSliderUI <- renderUI({
  sliderInput(
    "projTimelineRange", "Time period of interest:",
    min = earliest.proj.start(),
    max = latest.proj.start(),
    step = 1,
    value = c(earliest.proj.start(),latest.proj.start())
  )
})

output$projITBoardUI <- renderUI({
  selectInput(
    'selITBoard', 'IT Boards', allITBoards(), selected = allITBoards(),  multiple = TRUE, selectize = TRUE
  )
})

output$projtimelineUI <- renderUI({
  plotOutput("projtimeline", height = 300,
             click = "projtimeline_click")
})

output$projtimeline <- renderPlot({
  proj.df <-
    subset(projects.df, subset = IT.Board %in% input$selITBoard)
  
  proj.df <-
    proj.df[proj.df$Project.Start.Date >= paste(input$projTimelineRange[1],"01","01",sep =
                                                  "-") &
              proj.df$Project.End.Date <= paste(input$projTimelineRange[2],"12","31",sep =
                                                  "-"),]
  
  if (nrow(proj.df) == 0)
    return(NULL)
  
  ## Note use of `rev` to accommodate use of ggplot later
  proj.df <-
    proj.df[rev(order(proj.df$Budget.Requested, proj.df$Project.Short.Name)),]
  
  base <-
    ggplot(proj.df, aes(
      x = Project.Start.Date, y = projectID, color = as.factor(IT.Board)
    ))
  gantt <- {
    base +
      scale_y_discrete(breaks = proj.df$projectID, labels = proj.df$Project.Short.Name) +
      geom_segment(aes(xend = Project.End.Date, y = projectID, yend = projectID), size = 5)
  }
  gantt <-
    gantt + ylab(NULL) + xlab(NULL) + labs(color = "IT Board") + guides(color = guide_legend(title.hjust = 0.5))
  gantt <-
    gantt + scale_x_datetime(
      breaks = "3 month", labels = date_format("%Y-%b"), minor_breaks = "3 month"
    )
  gantt + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_vline(xintercept = as.integer(as.POSIXct(today())), linetype = "dashed")
  
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
    subset(projData, subset = IT.Board %in% input$selITBoard)
  
  projData <-
    projData[order(projData$Budget.Requested, projData$Project.Short.Name),]
  projData <- projData[round(input$projtimeline_click$y),]
  
  
  wellPanel(
    titlePanel(projData$Project.Short.Name),
    HTML(paste("<b>Project Manager:</b> ",projData$Project.Manager,"<p>")),
    HTML(paste("<b>Project Sponsor:</b> ",projData$Project.Sponsor,"<p>")),
    HTML(paste("<b>Budget Requested:</b> ",
               paste("£",format(projData$Budget.Requested, big.mark = ","),sep = ""),"<p>")),
    # HTML(paste("<a href='",projData$Dummy.Link,"'>",projData$Dummy.Link,"</a><p>")),
  HTML(newlineFn(projData$Project.Summary))
)
})

## Show projects.df as a DataTable

output$projectsDataTableColUI <- renderUI(selectInput('proj_Cols', 'Columns to show:',
                                                          names(projects.df)[1:(ncol(projects.df)-1)], selected = c("Project.Short.Name","Project.Start.Date",
                                                                                                                    "Project.End.Date","Project.Summary"), 
                                                          multiple = TRUE))

output$projectsDataTable <- renderDataTable(
  projects.df[, input$proj_Cols, drop = FALSE],
  option = list( drawCallback = I("function( settings ) {document.getElementById('ex1').style.width = '100%';}")) 
)

# ===================== Comms Plan Timelines ================================

earliest.comms.start <-
  reactive(year(min(commsplanMultiDay.df$Start.Date)))
latest.comms.start <-
  reactive(year(max(commsplanMultiDay.df$End.Date)))
allCommsTypes <- reactive(levels(commsplanMultiDay.df$Comms.Type))
allCommsSources <- reactive(levels(commsplanMultiDay.df$Source))

output$commsTimeSliderUI <- renderUI({
  sliderInput(
    "commsTimelineRange", "Time period of interest:",
    min = earliest.comms.start(),
    max = latest.comms.start(),
    step = 1,
    value = c(earliest.comms.start(),latest.comms.start())
  )
})

output$commsCommTypesUI <- renderUI({
  selectInput(
    'selCommsType', 'Communication Types', allCommsTypes(), selected = allCommsTypes(),  multiple =
      TRUE, selectize = TRUE
  )
})

output$commsCommSourcesUI <- renderUI({
  selectInput(
    'selCommsSource', 'Communication Plans', allCommsSources(), selected = allCommsSources(),  multiple =
      TRUE, selectize = TRUE
  )
})

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
  
  comms.df <-
    comms.df[comms.df$Start.Date >= paste(input$commsTimelineRange[1],"01","01",sep =
                                                          "-") &
               comms.df$End.Date <= paste(input$commsTimelineRange[2],"12","31",sep =
                                                          "-"),]
  
  if (nrow(comms.df) == 0)
    return()
  
  base <- ggplot(comms.df, aes(x = Start.Date, y = taskID, color = Comms.Type))
  gantt <- {
    base +
      scale_y_discrete(breaks = NULL) +
      geom_segment(aes(xend = End.Date, y = taskID, yend = taskID), size = 5, color = "black") +
      geom_segment(aes(xend = End.Date, y = taskID, yend = taskID), size = 4) +
      facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE)
  }
  gantt <-
    gantt + ylab(NULL) + xlab(NULL) + labs(color = "Comms Type") + guides(color = guide_legend(title.hjust = 0.5))
  gantt <-
    gantt + scale_x_datetime(
      breaks = "3 month", labels = date_format("%Y-%b"), minor_breaks = "3 month"
    )
  gantt + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_vline(xintercept = as.integer(as.POSIXct(today())), linetype = "dashed")
  
})

output$commsPlanMultiDaySummary <- renderUI({
  if (is.null(input$commsPlanMultiDay_click$y))
    return()
  comms.df <- commsplanMultiDay.df
  
  slction <-
    subset(comms.df, subset = Source == input$commsPlanMultiDay_click$panelvar1)
  
  slction <-
    subset(
      slction, Comms.Type %in% input$selCommsType &
        Source %in% input$selCommsSource
    )
  
  slction <-
    slction[slction$Start.Date >= paste(input$commsTimelineRange[1],"01","01",sep =
                                            "-") &
              slction$End.Date <= paste(input$commsTimelineRange[2],"12","31",sep =
                                            "-"),]
  
  slction <- slction[rev(order(slction$Start.Date,slction$Action)),]
  slction <- slction[round(input$commsPlanMultiDay_click$y),]
  
  wellPanel(titlePanel(slction$Action),
            HTML(paste(
              "<b>Delivery Window:</b>",as.character(slction$Start.Date), " - ",as.character(slction$End.Date),"<p>"
            )),
            HTML(paste(
              "<b>Comms Type:</b>",as.character(slction$Comms.Type),"<p>"
            )),
            HTML(paste(
              "<b>Source:</b>",as.character(slction$Source),"<p>"
            )),
            
            
            
            HTML(paste(
              "<b>Contacts:</b>",gsub(pattern = "\n", replacement = ", ", x = as.character(slction$Contacts)),"<p>"
            )),
            HTML(newlineFn(slction$Description))
            )
})
# ===================== Budget Treemap Outputs ===============================

output$resourceTreemapUI <- renderUI({
  plotOutput("resourceTreemap", height = 500,
             click = "resourceTreemap_click")
})

e <- environment()

output$resourceTreemap <- renderPlot({
  tm <- treemap(
    projects.df,
    index = "Project.Short.Name",
    vSize = "Budget.Requested",
    vColor = "Budget.Requested",
    type = "value",
    title = "", position.legend = "none"
  )
  assign("tm", tm, envir = e)
})

tmLocate <- function(coor, tmSave) {
  tm <- tmSave$tm
  # retrieve selected rectangle
  rectInd <- which(tm$x0 < coor[1] &
                     (tm$x0 + tm$w) > coor[1] &
                     tm$y0 < coor[2] &
                     (tm$y0 + tm$h) > coor[2])
  
  return(tm[rectInd[1],])
}

getTreemapClickID <- reactive({
  x <- input$resourceTreemap_click$x
  y <- input$resourceTreemap_click$y
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

treeCheck <- function(x) try(x)

output$resourceTreemapSummary <-
  renderUI({
    
    # print(getTreemapClickID()$Project.Short.Name)
    
    if (class(try(getTreemapClickID()))=="try-error")
      return(NULL)
    if(try(is.null(getTreemapClickID()$Project.Short.Name), silent = TRUE))
      return(NULL)
    
    projData <-
      projects.df[projects.df$Project.Short.Name == getTreemapClickID()$Project.Short.Name,]

    wellPanel(
      titlePanel(projData$Project.Short.Name),
      HTML(paste("<b>Project Manager:</b> ",projData$Project.Manager,"<p>")),
      HTML(paste("<b>Project Sponsor:</b> ",projData$Project.Sponsor,"<p>")),
      HTML(paste("<b>Budget Requested:</b> ",
                 paste("£",format(projData$Budget.Requested, big.mark = ","),sep = ""),"<p>")),
      # HTML(paste("<a href='",projData$Dummy.Link,"'>",projData$Dummy.Link,"</a><p>")),
      HTML(newlineFn(projData$Project.Summary))
    )
    
  })
