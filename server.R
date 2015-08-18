## Server for RDM Roadmap Dashboard

# Packages to load
library(ggplot2)
library(shiny)

# Data and variables
library(googlesheets)
projects.sheet <- gs_key("1I6Z94prfJrmSSmD_mwqazkp8Qx8AUmmsp9hAW6bF8yQ",
                         visibility = "public")
projects.df <- gs_read(projects.sheet)
## Coerce Types correctly:
projects.df$Project.Start.Date <- as.POSIXct(projects.df$Project.Start.Date)
projects.df$Project.End.Date <- as.POSIXct(projects.df$Project.End.Date)
projects.df$Project.Short.Name <- as.factor(projects.df$Project.Short.Name)
projects.df$IT.Board <- as.factor(projects.df$IT.Board)
projects.df$Project.Sponsor<- as.factor(projects.df$Project.Sponsor)
projects.df$Project.Manager <- as.factor(projects.df$Project.Manager)
projects.df$Senior.Supplier <- as.factor(projects.df$Senior.Supplier)
projects.df$Senior.User <- as.factor(projects.df$Senior.User)
projects.df$Budget.Requested <- as.factor(projects.df$Budget.Requested)

# Shiny Server

shinyServer(
  function(input, output, session){
    source("external/app.R",local = TRUE)
  }
)