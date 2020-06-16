#command to add in PowerBI
#source("<FILE LOCATION>/load_function.R", echo = T, prompt.echo = "", spaced = F)
  



if (!require("tidyverse")){install.packages("tidyverse")}
if (!require("lubridate")){install.packages("lubridate")}
if (!require("shiny")){install.packages("shiny")}
if (!require("tempR")){install.packages("tempR")}
if (!require("knitr")){install.packages("knitr")}
if (!require("kableExtra")){install.packages("kableExtra")}
if (!require("semantic.dashboard")) {install.packages("semantic.dashboard")}
if (!require("readxl")) {install.packages("readxl")}

library(tidyverse)
library(readxl)

file_location <-"C:/Users/carlos/Documents/PMDashboard/"

setwd(file_location)

source(paste(file_location,"r/variables.R",sep=""), echo = F, prompt.echo = "", spaced = F)

app_vars$source_system <- "Demo"

app_vars$excel_files <- paste(file_location,app_vars$excel_files,sep="")
app_vars$demo_files <- paste(file_location,app_vars$demo_files,sep="")

source(paste(file_location,"r/eval.R",sep=""), echo = F, prompt.echo = "", spaced = F)
source(paste(file_location,"r/data_loader.R",sep=""), echo = F, prompt.echo = "", spaced = F)

presentation_data <- eval_data(normalised_data,app_vars)

tasks_p <- presentation_data$tasks
updates_p <- presentation_data$updates
issues_p <-  presentation_data$issues
actions_p <- presentation_data$actions
projects_p <- presentation_data$projects
consolidated_p <- presentation_data$t.consolidated_tasks

status_colour_R <-  app_vars$status_colour %>% filter(colour_short=="R")
status_colour_A <-  app_vars$status_colour %>% filter(colour_short=="A")
status_colour_G <-  app_vars$status_colour %>% filter(colour_short=="G")
status_colour_gr <-  app_vars$status_colour %>% filter(colour_short=="g")


