# ===================== Project Timeline Outputs ===============================

earliest.proj.start <-
  reactive(year(min(projects.df$Project.Start.Date)))
latest.end.start <-
  reactive(year(max(projects.df$Project.End.Date)))
allITBoards <- reactive(levels(projects.df$IT.Board))

output$projTimeSliderUI <- renderUI({
  sliderInput(
    "projTimelineRange", "Time period of interest:",
    min = earliest.proj.start(),
    max = latest.end.start(),
    step = 1,
    value = c(earliest.proj.start(),latest.end.start())
  )
})

output$projITBoardUI <- renderUI({
  selectInput(
    'selITBoard', 'IT Boards', allITBoards(), selected = allITBoards(),  multiple = TRUE, selectize = TRUE
  )
})

output$hideMilestonesUI <-renderUI({
  checkboxInput("hideMilestones","Hide milestone labels", value = FALSE)
})

output$projtimelineUI <- renderUI({
  plotOutput("projtimeline", height = 400,
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
  
  
  milestone.df <- data.frame(milestone = c(as.POSIXct("2010-10-01"),as.POSIXct("2012-07-01"),
                                           as.POSIXct("2014-12-01"),as.POSIXct("2014-08-01"),as.POSIXct(today())),
                             descrip =c("RDM Website Launched","RDM Policy Ratified","ORA-Data Launched","ORDS Launched","Today"), 
                             colour = c(rep("red",4),"blackFOOBAR"),
                             ganttItems = rep(nrow(proj.df),5) # necessary to insert data into geom_text(aes)
  )
  
  milestone.df <-
    milestone.df[milestone.df$milestone > paste(input$projTimelineRange[1],"01","01",sep =
                                                  "-") &
                   milestone.df$milestone < paste(input$projTimelineRange[2],"01","01",sep =
                                                    "-"),]
  
  if (nrow(proj.df) == 0)
    return(NULL)
  
  proj.df <-
    proj.df[order(proj.df$Project.Start.Date, proj.df$Project.Short.Name),]
  
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
    gantt + scale_x_datetime(breaks = "3 month", labels = date_format("%Y-%b"), minor_breaks = "3 month") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  gantt + geom_vline(data = milestone.df, aes(xintercept = as.integer(milestone), guide = FALSE), linetype = "dashed", guide = FALSE) +
    if(!input$hideMilestones)
      geom_text(data = milestone.df, aes(x = milestone,
                                         y = (2 + nchar(as.vector(descrip))/5), 
                                         label = descrip),
                angle = 90, color = "black", text = element_text(size = 14), guide = FALSE)
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
    projData[rev(order(projData$Project.Start.Date, projData$Project.Short.Name)),]
  projData <- projData[round(input$projtimeline_click$y),]
  
  
  wellPanel(
    titlePanel(projData$Project.Short.Name),
    HTML(
      paste("<b>Project Manager:</b> ",projData$Project.Manager,"<p>")
    ),
    HTML(
      paste("<b>Project Sponsor:</b> ",projData$Project.Sponsor,"<p>")
    ),
    HTML(paste(
      "<b>Budget Requested:</b> ",
      paste("£",format(projData$Budget.Requested, big.mark = ","),sep = ""),"<p>"
    )),
    # HTML(paste("<a href='",projData$Dummy.Link,"'>",projData$Dummy.Link,"</a><p>")),
    HTML(newlineFn(projData$Project.Summary))
  )
})

## Show projects.df as a DataTable

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

# ===================== Comms Plan Timelines ================================

earliest.comms.start <-
  reactive(year(min(commsplanMultiDay.df$Start.Date)))
latest.comms.end <-
  reactive(year(max(commsplanMultiDay.df$End.Date)))
allCommsTypes <- reactive(levels(commsplanMultiDay.df$Comms.Type))
allCommsSources <- reactive(levels(commsplanMultiDay.df$Source))

output$commsTimeSliderUI <- renderUI({
  sliderInput(
    "commsTimelineRange", "Time period of interest:",
    min = earliest.comms.start(),
    max = latest.comms.end(),
    step = 1,
    value = c(earliest.comms.start(),latest.comms.end())
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
  
  base <-
    ggplot(comms.df, aes(x = Start.Date, y = taskID, color = Comms.Type))
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
    subset(slction, Comms.Type %in% input$selCommsType &
             Source %in% input$selCommsSource)
  
  slction <-
    slction[slction$Start.Date >= paste(input$commsTimelineRange[1],"01","01",sep =
                                          "-") &
              slction$End.Date <= paste(input$commsTimelineRange[2],"12","31",sep =
                                          "-"),]
  
  slction <- slction[rev(order(slction$Start.Date,slction$Action)),]
  slction <- slction[round(input$commsPlanMultiDay_click$y),]
  
  wellPanel(
    titlePanel(slction$Action),
    HTML(
      paste(
        "<b>Delivery Window:</b>",as.character(slction$Start.Date), " - ",as.character(slction$End.Date),"<p>"
      )
    ),
    HTML(paste(
      "<b>Comms Type:</b>",as.character(slction$Comms.Type),"<p>"
    )),
    HTML(paste(
      "<b>Source:</b>",as.character(slction$Source),"<p>"
    )),
    
    
    
    HTML(paste(
      "<b>Contacts:</b>",gsub(
        pattern = "\n", replacement = ", ", x = as.character(slction$Contacts)
      ),"<p>"
    )),
    HTML(newlineFn(slction$Description))
  )
})
# ===================== Budget Treemap Outputs ===============================

output$resourceTreemapTimeSliderUI <- renderUI({
  sliderInput(
    "resourceTreemapTimelineRange", "Time period of interest:",
    min = earliest.proj.start(),
    max = latest.end.start(),
    step = 1,
    value = c(earliest.proj.start(),latest.end.start())
  )
})

output$resourceTreemapUI <- renderUI({
  plotOutput("resourceTreemap", height = 500,
             click = "resourceTreemap_click")
})

e <- environment()

output$resourceTreemap <- renderPlot({
  proj.df <-
    projects.df[projects.df$Project.Start.Date >= paste(input$resourceTreemapTimelineRange[1],"01","01",sep =
                                                          "-") &
                  projects.df$Project.End.Date <= paste(input$resourceTreemapTimelineRange[2],"12","31",sep =
                                                          "-"),]
  
  tm <- treemap(
    proj.df,
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


output$resourceTreemapSummary <-
  renderUI({
    # print(getTreemapClickID()$Project.Short.Name)
    
    if (class(try(getTreemapClickID()))  ==  "try-error")
      return(NULL)
    if (try(is.null(getTreemapClickID()$Project.Short.Name), silent = TRUE))
      return(NULL)
    
    projData <-
      projects.df[projects.df$Project.Short.Name == getTreemapClickID()$Project.Short.Name,]
    
    wellPanel(
      titlePanel(projData$Project.Short.Name),
      HTML(
        paste("<b>Project Manager:</b> ",projData$Project.Manager,"<p>")
      ),
      HTML(
        paste("<b>Project Sponsor:</b> ",projData$Project.Sponsor,"<p>")
      ),
      HTML(paste(
        "<b>Budget Requested:</b> ",
        paste("£",format(projData$Budget.Requested, big.mark = ","),sep = ""),"<p>"
      )),
      # HTML(paste("<a href=",projData$Dummy.Link,">",projData$Dummy.Link,"</a><p>")),
      HTML(newlineFn(projData$Project.Summary))
    )
    
  })

# ===================== Calendar Heatmap ===============================


allTrainingSoftware <- reactive(levels(trainingEvents.df$Software))
allTrainingPLanguages <- reactive(levels(trainingEvents.df$Programming.Language))
allTrainingCategories <- reactive(levels(trainingEvents.df$Category))

output$trainingCategoriesUI <- renderUI({
  tagList(
    selectInput('selCategories', 'Training Category', sort(allTrainingCategories()), 
                selected = sort(allTrainingCategories()),  multiple = TRUE, selectize = TRUE),
    tags$style(type="text/css", "select#selCategories + .selectize-control{ width: 800px}")
  )
})


output$trainingHeatmapUI <- renderUI({
  plotOutput("trainingHeatmapPlot", height = 500,
             click = "trainingHeatmapPlot_click")
})

output$trainingHeatmapPlot <- renderPlot({
  
  training.df <-
    subset(
      trainingEvents.df, subset = Category %in% input$selCategories
    )
  
  ## Count total events on each day:
  eventFreq.df <- as.data.frame(table(training.df$Date))
  colnames(eventFreq.df) <- c("date","Freq")
  eventFreq.df$date <- as.POSIXct(eventFreq.df$date)
  
  ## http://www.r-bloggers.com/ggplot2-time-series-heatmaps/
  ### Function to calculate week of month:
  weekOfMonth <- function(x){
    weekN <- function(x) as.numeric(format(x, "%U"))
    weekN(x) - weekN(as.Date(cut(x, "month"))) + 1
  } 
  # Factor the eventFreq.df
  # We will facet by year ~ month, and each subgraph will
  # show week-of-month versus weekday
  # the year is simple
  eventFreq.df$year<-as.numeric(as.POSIXlt(eventFreq.df$date)$year+1900)
  # the month too 
  eventFreq.df$month<-as.numeric(as.POSIXlt(eventFreq.df$date)$mon+1)
  # but turn months into ordered facors to control the appearance/ordering in the presentation
  eventFreq.df$monthf<-factor(eventFreq.df$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
  # the day of week is again easily found
  eventFreq.df$weekday = as.POSIXlt(eventFreq.df$date)$wday
  # again turn into factors to control appearance/abbreviation and ordering
  # I use the reverse function rev here to order the week top down in the graph
  # you can cut it out to reverse week order
  eventFreq.df$weekdayf<-factor(eventFreq.df$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
  # the monthweek part is a bit trickier 
  # first a factor which cuts the eventFreq.dfa into month chunks
  eventFreq.df$yearmonth<-as.yearmon(eventFreq.df$date)
  eventFreq.df$yearmonthf<-factor(eventFreq.df$yearmonth)
  # then find the "week of year" for each day
  eventFreq.df$week <- as.numeric(format(eventFreq.df$date,"%W"))+1
  # and now for each monthblock we normalize the week to start at 1 
  eventFreq.df<-ddply(eventFreq.df,.(yearmonthf),transform,monthweek=1+week-min(week))
  eventFreq.df$monthweek <- weekOfMonth(eventFreq.df$date)
  
  events <- eventFreq.df
  
  if (nrow(events) == 0)
    return()
  
  trainingHeatMap <- ggplot(eventFreq.df, aes(monthweek, weekdayf, fill = Freq)) + 
    geom_tile(colour = "white") + 
    facet_grid(year~monthf) + 
    scale_fill_gradient(low="yellow", high="red", na.value="white") +
    xlab("Week of Month") + ylab("")
  
  trainingHeatMap + geom_vline(xintercept=seq(0.5, 5.5, 1)) + geom_hline(yintercept = seq(0.5, 5.5, 1), color = "black") +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())
  
  
})

output$trainingHeatmapSummary <- renderUI({
  if (is.null(input$trainingHeatmapPlot_click))
    return()
  
  events <- eventFreq.df
  
  ## This selects from the eventFreq table so as to construct the date - could do this more efficiently be constructing date algorithmically
  ## TODO: Replace this with an algorithmic method
  selected <-
    subset(
      events, monthf %in% input$trainingHeatmapPlot_click$panelvar1 &
        year %in% input$trainingHeatmapPlot_click$panelvar2 &
        weekday %in% (6 - round(input$trainingHeatmapPlot_click$y)) &
        # 6 - to account for reversed weekdays
        monthweek %in% round(input$trainingHeatmapPlot_click$x)
    )
  
  eventsOnDay <-
    trainingEvents.df[trainingEvents.df$Date == selected$date,]
  
  if (nrow(eventsOnDay) == 0)
    return()
  
  ## TODO: Make a useful summary
  wellPanel(HTML(
    paste(
      "<p><h1>Events on ",unique(as.character(eventsOnDay$Date)),"</h1></p>",
      "<table class='table'>",
      "<thead>",
      "<tr>",
      "<th>Event Title</th><th>Event Category</th><th>Software?</th><th>Link</th>",
      "</thead>",
      "<tbody>",
      paste(
        "<tr>","<td>",eventsOnDay$Event.Title,"</td>","<td>",eventsOnDay$Category,"</td>","<td>",
        eventsOnDay$Software,"</td>","<td>","<a href =",eventsOnDay$URL,">",eventsOnDay$URL,"</a>","</td>","</tr>",
        sep = "",
        collapse = "" ),
      "</tbody",
      "</table>",collapse = ""
    )
  )
  
  )
  
})

## Show projects.df as a DataTable

output$trainingDataTableColUI <-
  renderUI(selectInput(
    'training_Cols', 'Columns to show:',
    names(trainingEvents.df)[1:(ncol(trainingEvents.df) - 1)], selected = c(
      "Event.Title","Category","Date"
    ),
    multiple = TRUE
  ))

output$trainingDataTable <-
  DT::renderDataTable(trainingEvents.df[, input$training_Cols, drop = FALSE],
                      filter = "top")

## ============================= Proposed Projects ============================

output$proposedProjectsDataTable <-
  DT::renderDataTable(proposedProjects.df,
                      filter = "top")

## ============================ ORA Data ======================================


output$oraData_DepositsAndPublishedPlot <- renderPlot({
  oraMelted <- melt(as.data.frame(oraData.df.TimeSeries),  id.vars = 'Report.Date', variable.name = 'series')
  oraMelted$Report.Date <- as.POSIXct(oraMelted$Report.Date)
  base <- ggplot(oraMelted, aes(Report.Date, value))
  plot <- base + geom_area(aes(group = series, fill= series), position = 'identity') + geom_point(aes(color = series), color = "black")
  plot <- plot + xlab("Date") + ylab("Number of Deposits/Published")
  plot + scale_fill_discrete(name="",
                             breaks=c("Datasets.Deposited", "Datasets.Published"),
                             labels=c("Datasets Deposited", "Datasets Published")) +
    scale_y_continuous(breaks = seq(0,round(max(oraMelted$value)+5,-1),5)) +
    scale_x_datetime(breaks = "1 month", labels = date_format("%d-%b-%Y"), minor_breaks = "1 month") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$oraData_FundersPlot <- renderPlot({
  ggplot(oraData.FundingGrants, aes(x = rev(funderID), 
                                    y = Number.of.Projects, fill = Funder)) + geom_bar(stat="identity") +
    scale_fill_hue(l=40) + theme(legend.position = "none", 
                                 axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 45, hjust = 1)) + 
    xlab("Funding Body") + ylab("Number of Projects") + scale_x_discrete(breaks = oraData.FundingGrants$funderID, labels = rev(oraData.FundingGrants$Funder)) 
})


