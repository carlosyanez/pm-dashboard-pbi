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

#source("C:/Users/carlos/Documents/PMDashboard/load_function.R", echo = T, prompt.echo = "", spaced = F)


setwd("C:/Users/carlos/Documents/PMDashboard/")

source("C:/Users/carlos/Documents/PMDashboard/r/variables.R", echo = F, prompt.echo = "", spaced = F)
app_vars$source_system <- "Demo"

app_vars$demo_files <- paste("C:/Users/carlos/Documents/PMDashboard",app_vars$demo_files,sep="")

source("C:/Users/carlos/Documents/PMDashboard/r/eval.R", echo = F, prompt.echo = "", spaced = F)
source("C:/Users/carlos/Documents/PMDashboard/r/data_loader.R", echo = F, prompt.echo = "", spaced = F)

output <- normalised_data




