app_vars <-vector(mode = "list", length = 0)

app_vars$source_system <- "Demo"

app_vars$theme <- "cerulean"

app_vars$URL <- "https://replacewithyouradress/"
app_vars$html_loc <- "/var/www/reports/"

app_vars$Ndata_file <- "./files/normalised_data.rds"
app_vars$Pdata_file <- "./files/presentation_data.rds"
app_vars$Rdata_file <- "./files/rendered_data.rds"
app_vars$Vdata_file <- "./files/snapshots_versions.rds"
app_vars$ReportFlex <- "./FlexDashboardReport.Rmd"

app_vars$auto_refresh <- lubridate::dhours(3)

app_vars$programme_board <-c("Sample_Programme")


app_vars$trello_key <- "./trello_secret.txt"

app_vars$status_colours <- tibble(colour=c("NA","green","amber","red","grey","black"),
                         colour_short=c("NA","G","A","R","g","B"),
                         hex=c(semantic_palette[["violet"]],
                               semantic_palette[["green"]],
                               semantic_palette[["orange"]],
                               semantic_palette[["red"]],
                               semantic_palette[["grey"]],
                               semantic_palette[["black"]]))

app_vars$project_kanban_background <- tibble(State=c("Backlog","Planning","In Progress","Complete"),
                                    Task=c("To Do","Blocked","In Progress","Complete"),
                                    Action=c("dummy1","dummy2","Open","Closed"),
                                    Issue=c("dummy1","dummy2","Open","Closed"),
                                    hex=c("#EAE8FF","#D8D5DB","#a8edc9","#dedede"),
                                    hex2=c("#EAE8FF","#D8D5DB","#ADACB5","#2D3142"))


app_vars$project_kanban_background <- app_vars$project_kanban_background %>%
                              mutate(kanban_style=paste("background-color:",hex2,";",sep=""))

app_vars$action_replace <- tibble(Original=c("complete","incomplete"),
                         Replacement=c("Closed","Open"))

app_vars$tasks_states <- tibble(Original=c("To Do",
                                  "Not Started",
                                  "In Progress",
                                  "Doing",
                                  "Complete",
                                  "Done",
                                  "Blocked",
                                  "On Hold"),
                       Normalised=c("To Do",
                                     "To Do",
                                     "In Progress",
                                     "In Progress",
                                     "Complete",
                                     "Complete",
                                     "Blocked",
                                     "Blocked"))

#they are already part of the palette
app_vars$kanban_backlog <- "teal"
app_vars$kanban_planning <- "violet"
app_vars$kanban_progress <- "blue"
app_vars$kanban_complete <- "grey"

app_vars$t.default_colour <- "black"
app_vars$c.background1 <- "white"
app_vars$c.background2 <- "violet"
app_vars$c.background3 <- tempR::adjust.brightness(col2rgb(semantic_palette[["grey"]]),percent=12000)
app_vars$c.background4 <- "white"

app_vars$font_sizes <- tibble(nbr=c(7,6,5,4,3,2,1),
                     size=c("xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large"))
#####

app_vars$RAG_replace <- tibble(colour=c("red","amber","green"),
                      letter=c("R","A","G"))

####

app_vars$horizon_span1 <- lubridate::ddays(3)
app_vars$horizon_span2 <- lubridate::ddays(7)

app_vars$c.U_rows <- tempR::adjust.brightness(col2rgb(semantic_palette[["red"]]),percent=7000)
app_vars$c.M_rows <-  tempR::adjust.brightness(col2rgb(semantic_palette[["yellow"]]),percent=7000)
app_vars$c.L_rows <-  "white"
app_vars$c.X_rows <-  tempR::adjust.brightness(col2rgb(semantic_palette[["purple"]]),percent=5000)
####

app_vars$update_date_limit_1 <- 6
app_vars$update_date_limit_2 <- 10
app_vars$task.R.threshold.1 <- 1
app_vars$task.R.threshold.2  <- 3
app_vars$task.A.threshold.1 <- 3
app_vars$task.A.threshold.2 <- 5
app_vars$issue.R.threshold.1 <- 1
app_vars$issue.R.threshold.2 <- 3
app_vars$issue.A.threshold.1 <- 3
app_vars$issue.A.threshold.2 <- 5
app_vars$action.R.threshold.1 <- 3
app_vars$action.R.threshold.2 <- 5
app_vars$action.A.threshold.1 <- 5 
app_vars$action.A.threshold.2 <- 7


app_vars$color_RAG <- "grey"
app_vars$flexoptions <- tibble(
                               shiny_theme=c("default", "cerulean", "journal", "flatly", "darkly", "readable", "spacelab", "united", "cosmo", "lumen", "paper", "sandstone", "simplex", "yeti",
                                       "cyborg"),
                               
                               flex_theme=c("default", "cerulean", "journal", "flatly", "darkly", "readable", "spacelab", "united", "cosmo", "lumen", "paper", "sandstone", "simplex", "yeti",
                                      "darkly")
                                      )

app_vars$theme_flex <- if(app_vars$theme %in% app_vars$flexoptions$shiny_theme){
  app_vars$flexoptions %>%
    filter(shiny_theme==app_vars$theme) %>% 
    pull(flex_theme)
}else{"flatly"}

app_vars$history_call <- tibble(Name=c("Full Report","Backlog","Planning","In Progress","Complete","Roadmap","Project Summary",
                                       "Issues","Actions"),
                                type=c("reports","api","api","api","api","api","api",
                                       "api","api"),
                                base=c("/FlexDashboard_","/synoptic_backlog","/synoptic_planning","/synoptic_inprogress",
                                       "/synoptic_complete","/roadmap","/project_summaries","/issues",
                                       "/actions"))

###for Excel Import
app_vars$url_value <- "https://www.google.com"
app_vars$date_last_activity <- "1970-01-01"
app_vars$excel_files <- "./excel_files/"
app_vars$demo_files <- "./demo_files/"
app_vars$demo_date <- lubridate::as_datetime("2020-05-22 09:00:00")
app_vars$demo_now <- lubridate::as_datetime("2020-05-22 09:00:00")
app_vars$demo_flexreport <- "https://carlosyanez.github.io/FlexDashboard_demo.html"

app_vars$today<-if(app_vars$source_system=="Demo"){app_vars$demo_now}else{Sys.Date()}


