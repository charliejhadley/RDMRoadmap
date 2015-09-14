## Import eventFreq.dfa:
library(googlesheets)
trainingEvents.sheet <- gs_key("13R1LyUVXSr82N7eifN0DDN-PAFvhv8Cyyi8Vm5sJs8U",
                         visibility = "public")
trainingEvents.df <- gs_read(trainingEvents.sheet)
str(trainingEvents.df)
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
str(trainingEvents.df)

## Count total events on each day:
library(dplyr)
eventFreq.df <- as.data.frame(table(trainingEvents.df$Date))
colnames(eventFreq.df) <- c("date","Freq")
eventFreq.df$date <- as.POSIXct(eventFreq.df$date)

# ## =========== Add empty days:
# ## Compute date range
# dateRange <- {
# minDate <- min(as.Date(trainingEvents.df$Date))
# maxDate <- max(as.Date(trainingEvents.df$Date))
# seq(minDate,maxDate,by="day")
# }
# foo1 <- strptime(dateRange,format="%Y-%m-%d")
# # Show that the objects are identical to one another
# trainingEvents.df$Date[1]==foo1[2]
# # compute setdiff
# correctEmpyDates <- strptime(as.POSIXct(setdiff(unique(trainingEvents.df$Date),strptime(dateRange,format="%Y-%m-%d")),origin = "1960-01-01"),
#          format="%Y-%m-%d")
# # Add empty dates into eventFreq.df
# eventFreq.df <- rbind(eventFreq.df,data.frame(date = emptyDays, Freq = rep(NA,length(emptyDays))))


### Function to calculate week of month:
weekOfMonth <- function(x){
  weekN <- function(x) as.numeric(format(x, "%U"))
  weekN(x) - weekN(as.Date(cut(x, "month"))) + 1
} 
# Factor the eventFreq.df
require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)
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


## Visualise:

trainingHeatMap <- ggplot(eventFreq.df, aes(monthweek, weekdayf, fill = Freq)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="yellow", high="red", na.value="white") +
  xlab("Week of Month") + ylab("")

trainingHeatMap + geom_vline(xintercept=seq(0.5, 5.5, 1)) + geom_hline(yintercept = seq(0.5, 5.5, 1), color = "black") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank())

trainingHeatMap <- trainingHeatMap + scale_x_continuous(breaks=seq(0.5, 5.5, 1), limits = c(0.5,5.5), minor_breaks = NULL, 
                                                labels = 1:5)


baseHeatMap + geom_hline(yintercept = seq(0.5, 5.5, 1), color = "black") + 
  scale_y_discrete(labels = c("Friday","Thursday","Wednesday","Tuesday","Monday")) +
  theme(panel.grid.major.y = element_blank())
  




seq(0, 5, 0.5)

shinyApp(
  ui = fluidPage(
    uiOutput("heatmapUI"),
    uiOutput("heatmapSummary")
  ),
  server = function(input, output, session){
    output$heatmapUI <- renderUI({
      plotOutput("heatmapPlot", height = 500,
                 click = "heatmapPlot_click")
    })
    
    output$heatmapPlot <- renderPlot({
      events <- eventFreq.df
      
      if (nrow(events) == 0)
        return()
      
      ggplot(events, aes(monthweek, weekdayf, fill = Freq)) + 
        geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="red", high="yellow") +
        xlab("Week of Month") + ylab("") + scale_x_continuous(limits = c(0,5))
      
    })
    
    output$heatmapSummary <- renderUI({
      if (is.null(input$heatmapPlot_click))
        return()
      
      events <- eventFreq.df
      
      ## This selects from the eventFreq table so as to construct the date - could do this more efficiently be constructing date algorithmically
      ## TODO: Replace this with an algorithmic method
      selected <- subset(events, monthf %in% input$heatmapPlot_click$panelvar1 & year %in% input$heatmapPlot_click$panelvar2 &
                           weekday %in% (6 - round(input$heatmapPlot_click$y)) & # 6 - to account for reversed weekdays
                           monthweek %in% round(input$heatmapPlot_click$x)
      )
      
      print(selected)
      
      eventsOnDay <- trainingEvents.df[trainingEvents.df$Date == selected$date,]
      
      if(nrow(eventsOnDay)==0)
        return()
      
      ## TODO: Make a useful summary
      wellPanel(
        HTML(paste(
          "<p>Events on selected day</p>",
          paste(eventsOnDay$Event.Title,sep="</p>")
          ,sep=""
        ))
      )
      
    }
    )
  })



