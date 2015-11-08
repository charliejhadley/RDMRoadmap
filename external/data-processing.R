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
projects.df <-
  projects.df[order(
    projects.df$Project.Start.Date, projects.df$Project.Short.Name
  ),]
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
commsplanMultiDay.df <<- gs_read(commsplanMultiDay.sheet, ws = "Approved-Data")
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

## ======================== Proposed Projects ======================================
proposedProjects.workbook <-
  gs_key("10HiEzYEe0VUXyWIuhoruBjLkPFbB8HOryvnf8A1y6YY",
         visibility = "public")
proposedProjects.df <<- gs_read(proposedProjects.workbook, ws = "Proposed-Projects")

## ================ ORA Data Loading ==================
oraData.workbook <-
  gs_key("1dufaAF7nYhXnbZ9c1KYRkuc1lh_LClVv72iqx3bGEFA",
         visibility = "public")
oraData.UpdateDate <<- gs_read(oraData.workbook, ws = "Updated-Date")
oraData.UpdateDate <<- as.POSIXct(oraData.UpdateDate[[1]])

oraData.df.Summary <<- gs_read(oraData.workbook, ws = "Summary")
oraData.df.FundingGrants <<- gs_read(oraData.workbook, ws = "Funding-Grants")
oraData.df.TimeSeries <<- gs_read(oraData.workbook, ws = "TimeSeries")
oraData.df.TimeSeries$Report.Date <- as.POSIXct(sapply(oraData.df.TimeSeries$Report.Date, function(x)paste(x,"12:00:00",sep=" ")),tz="GMT")


oraData.FundingGrants <<- gs_read(oraData.workbook, ws = "Funding-Grants")

oraData.FundingGrants <<-
  oraData.FundingGrants[rev(order(
    oraData.FundingGrants$Number.of.Projects, oraData.FundingGrants$Funder)),]

oraData.FundingGrants$funderID <<- as.factor(nrow(oraData.FundingGrants):1)

## ================ ORDS Data Loading ==================

ords.workbook <-
  gs_key("15WCvJcpJCVE1TPWut4iPWM0B0yQ4IrhwWPUJmTGwR6A",
         visibility = "private")
ords.FullAndTrial <<- gs_read(ords.workbook, ws = "Sheet1")
ords.FullAndTrial$Date <- force_tz(dmy(ords.FullAndTrial$Date), tzone = "GMT")
ords.FullAndTrial <- ords.FullAndTrial[c(1,2,3)]

# # projects.df$Project.Start.Date <-
# #   as.POSIXct(projects.df$Project.Start.Date)
# # projects.df$Project.End.Date <-
# #   as.POSIXct(projects.df$Project.End.Date)
# projects.df$Project.Start.Date <- {
#   ymd <- ymd(projects.df$Project.Start.Date)
#   mdy <- mdy(projects.df$Project.Start.Date)
#   ymd[is.na(ymd)] <- mdy[is.na(ymd)]
#   force_tz(ymd, tzone = "GMT")
# }
# projects.df$Project.End.Date <- {
#   ymd <- ymd(projects.df$Project.End.Date)
#   mdy <- mdy(projects.df$Project.End.Date)
#   ymd[is.na(ymd)] <- mdy[is.na(ymd)]
#   force_tz(ymd, tzone = "GMT")
# }
# projects.df$Project.Short.Name <-
#   as.character(projects.df$Project.Short.Name)
# projects.df$IT.Board <- as.factor(projects.df$IT.Board)
# projects.df$Project.Sponsor <-
#   as.factor(projects.df$Project.Sponsor)
# projects.df$Project.Manager <-
#   as.factor(projects.df$Project.Manager)
# projects.df$Senior.Supplier <-
#   as.factor(projects.df$Senior.Supplier)
# projects.df$Senior.User <- as.factor(projects.df$Senior.User)
# ## Re-order data.frame according to this: http://stackoverflow.com/a/32333974/1659890
# projects.df <-
#   projects.df[order(
#     projects.df$Project.Start.Date, projects.df$Project.Short.Name
#   ),]
# projects.df$projectID <- as.factor(nrow(projects.df):1)