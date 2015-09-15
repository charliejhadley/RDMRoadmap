# App body for RDM Roadmap Dashboard

## ================ Projects Sheet Loading and Manipulation ==================
projects.workbook <-
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
## Note use of `rev` to accommodate use of ggplot later
projects.df <-
  projects.df[rev(order(
    projects.df$Budget.Requested, projects.df$Project.Short.Name
  )),]
projects.df$projectID <- as.factor(nrow(projects.df):1)

## ================== Project Summary Text  ============================

output$projSummaryText <- renderUI(HTML(
  paste(
    "There are currently ",as.character(length(projects.df$Project.Short.Name)), " RDM projects included in this tool, details on some of these projects
    can be accessed by selecting the project from the navigation on the left-hand side of this page. There are also a number of visualisations of these projects
    available from underneath \"RDM Projects\" in the navigation at the top of the page.</p>
    The current list of projects is as follows:</p>",
    paste(
      "<ul>",lapply(projects.df$Project.Short.Name, function(x)
        paste("<li>",x,"</li>",sep = "")),"</ul>", collapse = ""
    ),sep = ""
  )
))

## ==================  Comms Plan (Multi-day, non-repeating) ============================
commsplanMultiDay.sheet <-
  gs_key("1ZWxJhJM9p6UaoQnBOHUsuyfht40OaEw4qKybVdFtjTc",
         visibility = "public")
commsplanMultiDay.df <<- gs_read(commsplanMultiDay.sheet)
commsplanMultiDay.df$Action <<-
  as.factor(commsplanMultiDay.df$Action)
# commsplanMultiDay.df$Start.Date <<-
#   as.POSIXct(commsplanMultiDay.df$Start.Date)
# commsplanMultiDay.df$End.Date <<-
#   as.POSIXct(commsplanMultiDay.df$End.Date)

commsplanMultiDay.df$Start.Date <<- {
  ymd <- ymd(commsplanMultiDay.df$Start.Date)
  mdy <- mdy(commsplanMultiDay.df$Start.Date)
  ymd[is.na(ymd)] <- mdy[is.na(ymd)]
  force_tz(ymd, tzone = "GMT")
}
commsplanMultiDay.df$End.Date <<- {
  ymd <- ymd(commsplanMultiDay.df$End.Date)
  mdy <- mdy(commsplanMultiDay.df$End.Date)
  ymd[is.na(ymd)] <- mdy[is.na(ymd)]
  force_tz(ymd, tzone = "GMT")
}

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

## ============================ Training Events Data Processing ============================

trainingEvents.sheet <- gs_key("13R1LyUVXSr82N7eifN0DDN-PAFvhv8Cyyi8Vm5sJs8U",
                               visibility = "public")
trainingEvents.df <- gs_read(trainingEvents.sheet)
trainingEvents.df$Source <- factor(trainingEvents.df$Source)
#trainingEvents.df$Department <- factor(trainingEvents.df$Department)
#trainingEvents.df$Audience <- factor(trainingEvents.df$Audience)
trainingEvents.df$Category <- factor(trainingEvents.df$Category)
trainingEvents.df$Software <- factor(trainingEvents.df$Software)
trainingEvents.df$Programming.Language <- factor(trainingEvents.df$Programming.Language)
trainingEvents.df$Location <- factor(trainingEvents.df$Location)
trainingEvents.df$Date <- as.POSIXct(trainingEvents.df$Date)
# ifwantTime <- as.POSIXct(paste(trainingEvents.df$eventFreq.dfe,trainingEvents.df$Start.time,sep=" "))
## Re-order eventFreq.dfa.frame according to this: http://stackoverflow.com/a/32333974/1659890
trainingEvents.df$Event.Title <- as.character(trainingEvents.df$Event.Title)  
trainingEvents.df <- trainingEvents.df[order(trainingEvents.df$Date, trainingEvents.df$Event.Title), ]
trainingEvents.df$eventID <- as.factor(nrow(trainingEvents.df):1)

## Count total events on each day:
eventFreq.df <- as.data.frame(table(trainingEvents.df$Date))
colnames(eventFreq.df) <- c("date","Freq")
eventFreq.df$date <- as.POSIXct(eventFreq.df$date)

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

# ===================== Project Timeline Outputs ===============================

output$projDeleteUI <- renderUI(
  selectInput(
    'projDelete', 'Select Project to Delete', projects.df$Project.Short.Name, selectize =
      TRUE
  )
)

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
                                           as.POSIXct("2014-08-01"),as.POSIXct("2014-12-01"),as.POSIXct(today())),
                             descrip =c("RDM Website Launched","RDM Policy Ratified","ORA-Data Launched","ORDS Launched","Today"))
  
  milestone2.df <-
    milestone.df[milestone.df$milestone > paste(input$projTimelineRange[1],"01","01",sep =
                                                  "-") &
                   milestone.df$milestone < paste(input$projTimelineRange[2],"01","01",sep =
                                                    "-"),]
  
  print(milestone2.df)
  
  if (nrow(proj.df) == 0)
    return(NULL)
  
  ## Note use of `rev` to accommodate use of ggplot later
  proj.df <-
    proj.df[rev(order(proj.df$Budget.Requested, proj.df$Project.Short.Name)),]
  
#   base <-
#     ggplot(proj.df, aes(
#       x = Project.Start.Date, y = projectID, color = as.factor(IT.Board)
#     ))
#   gantt <- {
#     base +
#       scale_y_discrete(breaks = proj.df$projectID, labels = proj.df$Project.Short.Name) +
#       geom_segment(aes(xend = Project.End.Date, y = projectID, yend = projectID), size = 5)
#   }
#   gantt <-
#     gantt + ylab(NULL) + xlab(NULL) + labs(color = "IT Board") + guides(color = guide_legend(title.hjust = 0.5))
#   gantt <-
#     gantt + scale_x_datetime(
#       breaks = "3 month", labels = date_format("%Y-%b"), minor_breaks = "3 month"
#     )
#   gantt + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_vline(xintercept = as.integer(as.POSIXct(today())), linetype = "dashed")
#   
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
  
  gantt + geom_vline(data = milestone2.df, aes(xintercept = as.integer(milestone)), linetype = "dashed") +
    geom_text(data = milestone2.df, aes(x = milestone,
#                                       y = (length(milestone) - nchar(as.vector(descrip))/5), # need access to proj.df
                                        y = 15,
                                       label=descrip),
              angle = 90, colour = "black", text = element_text(size = 14)) 
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
    names(projects.df)[1:(ncol(projects.df) -
                            1)], selected = c(
                              "Project.Short.Name","Project.Start.Date",
                              "Project.End.Date","Project.Summary"
                            ),
    multiple = TRUE
  ))

output$projectsDataTable <-
  renderDataTable(projects.df[, input$proj_Cols, drop = FALSE],
                  option = list(
                    drawCallback = I(
                      "function( settings ) {document.getElementById('ex1').style.width = '100%';}"
                    )
                  ))

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
      # HTML(paste("<a href='",projData$Dummy.Link,"'>",projData$Dummy.Link,"</a><p>")),
      HTML(newlineFn(projData$Project.Summary))
    )
    
  })

# ===================== Calendar Heatmap ===============================

output$trainingHeatmapUI <- renderUI({
  plotOutput("trainingHeatmapPlot", height = 500,
             click = "trainingHeatmapPlot_click")
})

output$trainingHeatmapPlot <- renderPlot({
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
    "<table>",
    "<tr>",
    "<td>Event Title</td><td>Event Category</td><td>Software?</td><td>Link</td>",
    paste(
      "<tr>","<td>",eventsOnDay$Event.Title,"</td>","<td>",eventsOnDay$Category,"</td>","<td>",eventsOnDay$Software,"</td>","<td>",eventsOnDay$URL,"</td>","</tr>",sep = "",
      collapse = "" ),
    "</table>",
    "footer",collapse = ""
  )
  )
  
  )
  
})
