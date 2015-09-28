## Server for RDM Roadmap Dashboard

# Packages to load
library(ggplot2)
library(shiny)
library(treemap)
library(dplyr)
library(lubridate)
library(googlesheets)
library(scales)
library(quantmod)
library(reshape2)
library(plyr)
library(proto) # for geom_text2
library(DT) 
## ======================= Utility Functions =====================

# regex for handling new lines in text
newlineFn <- function(text){
  gsub(pattern = "\n", replacement = "<p>", x = text)
}

## ======================= Shiny Server =======================

shinyServer(
  function(input, output, session){
    source("external/data-processing.R",local = TRUE)
    source("external/visualisations&UI.R",local = TRUE)
  }
)