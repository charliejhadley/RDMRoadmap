## Server for RDM Roadmap Dashboard

# Packages to load
library(ggplot2)
library(shiny)
library(treemap)

## =========================== Load data sources ======================

## ======= Projects Timeline
library(googlesheets)
projects.sheet <- gs_key("1I6Z94prfJrmSSmD_mwqazkp8Qx8AUmmsp9hAW6bF8yQ",
                         visibility = "public")
projects.df <- gs_read(projects.sheet)
projects.df$Project.Start.Date <- as.POSIXct(projects.df$Project.Start.Date)
projects.df$Project.End.Date <- as.POSIXct(projects.df$Project.End.Date)
projects.df$Project.Short.Name <- as.character(projects.df$Project.Short.Name)
projects.df$IT.Board <- as.factor(projects.df$IT.Board)
projects.df$Project.Sponsor<- as.factor(projects.df$Project.Sponsor)
projects.df$Project.Manager <- as.factor(projects.df$Project.Manager)
projects.df$Senior.Supplier <- as.factor(projects.df$Senior.Supplier)
projects.df$Senior.User <- as.factor(projects.df$Senior.User)
# Reorder projects by budget - note that this is used in the project timeline visualisation
projects.df$Project.Short.Name <- factor(projects.df$Project.Short.Name, 
                                         levels=projects.df[order(projects.df$Budget.Requested),]$Project.Short.Name)
## ====== Comms Plan (Multi-day, non-repeating)
commsplanMultiDay.sheet <-
  gs_key("1ZWxJhJM9p6UaoQnBOHUsuyfht40OaEw4qKybVdFtjTc",
         visibility = "public")
commsplanMultiDay.df <- gs_read(commsplanMultiDay.sheet)
commsplanMultiDay.df$Action <- as.factor(commsplanMultiDay.df$Action)
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
commsplanMultiDay.df$Source <- factor(commsplanMultiDay.df$Source, ordered = TRUE, levels = c("RDM - John Southall","Open Access","FOOBAR") )

## ======================= Utility Functions =====================

# regex for handling new lines in text
newlineFn <- function(text){
  gsub(pattern = "\n", replacement = "<p>", x = text)
}


# Shiny Server

shinyServer(
  function(input, output, session){
    source("external/app.R",local = TRUE)
  }
)