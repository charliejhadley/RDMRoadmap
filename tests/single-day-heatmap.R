## Import eventFreq.dfa:
library(googlesheets)
singleDayEvents.sheet <- gs_key("13R1LyUVXSr82N7eifN0DDN-PAFvhv8Cyyi8Vm5sJs8U",
                         visibility = "public")
singleDayEvents.df <- gs_read(singleDayEvents.sheet)
str(singleDayEvents.df)
singleDayEvents.df$Source <- factor(singleDayEvents.df$Source)
singleDayEvents.df$Department <- factor(singleDayEvents.df$Department)
singleDayEvents.df$Audience <- factor(singleDayEvents.df$Audience)
singleDayEvents.df$Date <- as.POSIXct(singleDayEvents.df$Date)
# ifwantTime <- as.POSIXct(paste(singleDayEvents.df$eventFreq.dfe,singleDayEvents.df$Start.time,sep=" "))
## Re-order eventFreq.dfa.frame according to this: http://stackoverflow.com/a/32333974/1659890
singleDayEvents.df$Event.Title <- as.character(singleDayEvents.df$Event.Title)  
singleDayEvents.df <- singleDayEvents.df[order(singleDayEvents.df$Date, singleDayEvents.df$Event.Title), ]
singleDayEvents.df$eventID <- as.factor(nrow(singleDayEvents.df):1)
str(singleDayEvents.df)

## Count total events on each day:
library(dplyr)
eventFreq.df <- as.data.frame(table(singleDayEvents.df$Date))
colnames(eventFreq.df) <- c("date","Freq")
eventFreq.df$date <- as.POSIXct(eventFreq.df$date)
str(eventFreq.df)

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
str(eventFreq.df)
eventFreq.df$weekdayf<-factor(eventFreq.df$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
# the monthweek part is a bit trickier 
# first a factor which cuts the eventFreq.dfa into month chunks
eventFreq.df$yearmonth<-as.yearmon(eventFreq.df$date)
eventFreq.df$yearmonthf<-factor(eventFreq.df$yearmonth)
# then find the "week of year" for each day
eventFreq.df$week <- as.numeric(format(eventFreq.df$date,"%W"))+1
eventFreq.df$week
# and now for each monthblock we normalize the week to start at 1 
eventFreq.df<-ddply(eventFreq.df,.(yearmonthf),transform,monthweek=1+week-min(week))
str(eventFreq.df)

eventFreq.df$monthweek <- weekFn(eventFreq.df$date)


## Visualise:

ggplot(eventFreq.df, aes(monthweek, weekdayf, fill = Freq)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="red", high="yellow") +
  xlab("Week of Month") + ylab("") + scale_x_continuous(limits = c(1,5))