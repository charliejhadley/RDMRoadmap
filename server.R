## Server for RDM Roadmap Dashboard

# Packages to load
library(ggplot2)
library(shiny)
library(treemap)
library(dplyr)
library(lubridate)
library(googlesheets)

## ======================= Utility Functions =====================

# regex for handling new lines in text
newlineFn <- function(text){
  gsub(pattern = "\n", replacement = "<p>", x = text)
}

## ======================= Shiny Server =======================

shinyServer(
  function(input, output, session){
    source("external/app.R",local = TRUE)
  }
)