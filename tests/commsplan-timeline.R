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

## Get all Comms.Types
new.df <- commsplanMultiDay.df

levels(commsplanMultiDay.df$Comms.Type)

subset(new.df, subset = Comms.Type == c("Briefing","Presentation"))

is.null(subset(new.df, Comms.Type %in% c("Briefing") & Source %in% c("Foobar")))


subset(new.df, Comms.Type %in% c("Briefing"))


## Extract elements

sbst <- subset(commsplanMultiDay.df, subset = Source == "Open Access")
sbst[,c(2,6:7)]
sbst[order(sbst$Action),c(2,6:7)]
sbst[order(sbst$Start.Date,sbst$Action),c(2,6:7)]

order(sbst$Start.Date,sbst$Action)

str(reorder(new.df$Action,new.df$Start.Date))

new.df[order(new.df$Start.Date,new.df$Action),]
new.df$Action

reorder(new.df$Action,order(new.df$Start.Date,new.df$Action))

comms.df <-
  subset(
    commsplanMultiDay.df, subset = Comms.Type %in% c("Briefing","Email"), drop = TRUE
  )

str(comms.df)
comms.df$Comms.Type
### ======= geom_segment solution (experimental) ========== ###

new.df$Action <- factor



base <- ggplot(#  commsplanMultiDay.df,
  new.df,
  aes(
    x = Start.Date, reorder(Action,Start.Date), color = Comms.Type, order = Action
  ))

base + geom_segment(aes(
  xend = End.Date,ystart = Action, yend = Action
), color = "black", size = 5) + geom_segment(aes(
  xend = End.Date,ystart = Action, yend = Action
), size = 4) 
#  scale_y_discrete(breaks = NULL) + 
#  facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE)


### ======= geom_segment solution (that works) ========== ###

sort(commsplanMultiDay.df$Start.Date)

base <- ggplot(#  commsplanMultiDay.df,
  new.df,
  aes(
    x = Start.Date, reorder(Action,Start.Date), color = Comms.Type, order = Action
  ))

base + geom_segment(aes(
  xend = End.Date,ystart = Action, yend = Action
), color = "black", size = 5) + geom_segment(aes(
  xend = End.Date,ystart = Action, yend = Action
), size = 4) + 
  scale_y_discrete(breaks = NULL) + 
  facet_grid(Source ~ .,scale = "free_y",space = "free_y", drop = TRUE)



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
