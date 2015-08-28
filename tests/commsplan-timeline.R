## Comms Plan Timeline

### ============= DATA ==================== ###
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


### duplicating data.frame

new.df <- commsplanMultiDay.df


new.df$orderID <-
  factor(
    order(new.df$Start.Date), ordered = TRUE, levels = order(new.df$Start.Date)
  )

str(new.df)

## Extract elements

sbst <- subset(commsplanMultiDay.df, subset = Source == "FOOBAR")
sbst[,c(2,6:7)]
sbst[order(sbst$Action),c(2,6:7)]

### ======= geom_segment solution ========== ###

sort(commsplanMultiDay.df$Start.Date)

base <- ggplot(#  commsplanMultiDay.df,
  new.df,
  aes(
    x = Start.Date, Action, color = Comms.Type, order = Start.Date
  ))

base + geom_segment(aes(
  xend = End.Date,ystart = Action,yend = Action, order = Start.Date
), color = "black", size = 5) + geom_segment(aes(
  xend = End.Date,ystart = Action,yend = Action, order = Start.Date
),size = 4) + scale_y_discrete(breaks = NULL) + facet_grid(Source ~ .,scale =
                                                             "free_y",space = "free_y", drop = TRUE)



##  ============ Original Visualisation ==================
### This doesn't work as can't coord_flip with non-cartesian coords and geom_linerange is only for vertical
dat <- commsplanMultiDay.df

base <- ggplot(
  dat,
  aes(
    Action, Start.Date, ymin = Start.Date, color = Comms.Type, ymax = End.Date,xticks,
    order = Source
  )
)
base + geom_linerange(size = 2, alpha = 0.6)  + scale_x_discrete(breaks =
                                                                   NULL) +
  coord_flip()  + scale_colour_brewer(palette = "Spectral") +
  facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE) +
  ggtitle("Currently forced 3 factors for Source - FIX") +
  scale_y_datetime()



## Without using coord_flip

base <- ggplot(
  dat,
  aes(
    Action, Start.Date, ymin = Start.Date, color = Comms.Type, ymax = End.Date,xticks,
    order = Source
  )
)
base + geom_hline(size = 2, alpha = 0.6)
