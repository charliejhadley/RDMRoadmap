#### SE Question
library(googlesheets)
commsplanMultiDay.sheet <-
  gs_key("1ZWxJhJM9p6UaoQnBOHUsuyfht40OaEw4qKybVdFtjTc",
         visibility = "public")
commsplanMultiDay.df <- gs_read(commsplanMultiDay.sheet)
commsplanMultiDay.df$Action <-
  as.factor(commsplanMultiDay.df$Action)
commsplanMultiDay.df$Start.Date <-
  as.POSIXct(commsplanMultiDay.df$Start.Date)
commsplanMultiDay.df$End.Date <-
  as.POSIXct(commsplanMultiDay.df$End.Date)
commsplanMultiDay.df$Comms.Type <-
  as.factor(commsplanMultiDay.df$Comms.Type)
commsplanMultiDay.df$Source <-
  as.factor(commsplanMultiDay.df$Source)
commsplanMultiDay.df$Department <-
  as.factor(commsplanMultiDay.df$Department)
commsplanMultiDay.df$Audience <-
  as.factor(commsplanMultiDay.df$Audience)
## Re-order the sources while there is FOOBAR in here:
commsplanMultiDay.df$Source <-
  factor(
    commsplanMultiDay.df$Source, ordered = TRUE, levels = c("RDM - John Southall","Open Access","FOOBAR")
  )

## C8H10N4O2

se.df <- commsplanMultiDay.df[,c(1,2,6,7,8)]

se.df$Action <- as.character(se.df$Action)  
se.df <- se.df[order(se.df$Start.Date, se.df$Action), ]
se.df$taskID <- as.factor(nrow(se.df):1)

ggplot(se.df, aes(x = Start.Date, y=taskID, color = Comms.Type)) +
  scale_y_discrete(breaks=se.df$taskID, labels = se.df$Action) + 
  geom_segment(aes(xend = End.Date, y = taskID, yend = taskID), size = 5) +
  facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE)

rev()


## First Answer

se.df <-se.df[order(se.df$Start.Date,se.df$Action),]
se.df$Action <- factor(se.df$Action, levels=unique(se.df$Action))
ggplot(se.df, aes(x = Start.Date, color = Comms.Type)) +
  geom_segment(aes(xend = End.Date, y = Action, yend = Action), size = 5) +
  facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE)