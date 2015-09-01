## Server for RDM Roadmap Dashboard

# Packages to load
library(ggplot2)
library(shiny)
library(treemap)
library(dplyr)

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
#projects.df$Project.Short.Name <- factor(projects.df$Project.Short.Name, 
#                                         levels=projects.df[order(projects.df$Budget.Requested),]$Project.Short.Name)

## Re-order data.frame according to this: http://stackoverflow.com/a/32333974/1659890
projects.df$Project.Short.Name <- as.character(projects.df$Project.Short.Name)  
## Note use of `rev` to accommodate use of ggplot later
projects.df <- projects.df[rev(order(projects.df$Budget.Requested, projects.df$Project.Short.Name)), ]
projects.df$taskID <- as.factor(nrow(projects.df):1)

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
## Re-order data.frame according to this: http://stackoverflow.com/a/32333974/1659890
commsplanMultiDay.df$Action <- as.character(commsplanMultiDay.df$Action)  
commsplanMultiDay.df <- commsplanMultiDay.df[order(commsplanMultiDay.df$Start.Date, commsplanMultiDay.df$Action), ]
commsplanMultiDay.df$taskID <- as.factor(nrow(commsplanMultiDay.df):1)




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