
project_kanban_tables <- function(presentation_data,app_vars){
  
  output <-vector(mode = "list", length = 0)

  
    k_state <- "Backlog"
    k_colour <- "white"
    
    output$row_values <- nrow(presentation_data$projects)
    
    output$project_list_backlog <- presentation_data$projects %>% 
      mutate(k.Project=ifelse(State==k_state,k.Project,kableExtra::cell_spec("",background = k_colour, color=k_colour)),
             k.Progress=ifelse(State==k_state,k.Progress,""),
             k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
             k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
      ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
      arrange(k.Project) %>%
      knitr::kable(format = "html", escape = F,col.names = NULL,booktabs = T) %>%
      kableExtra::kable_styling(full_width = F) %>%
      kableExtra::column_spec(2:4,italic = TRUE)
  
  
    k_state <- "Planning"
    k_colour <- "white"
    
    output$project_list_planning <-  presentation_data$projects %>% 
      mutate(k.Project=ifelse(State==k_state,k.Project,kableExtra::cell_spec("",background = k_colour, color=k_colour)),
             k.Progress=ifelse(State==k_state,k.Progress,""),
             k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
             k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
      ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
      arrange(k.Project) %>%
      knitr::kable(format = "html", escape = F,col.names = NULL,booktabs = T) %>%
      kableExtra::kable_styling(full_width = F) %>%
      kableExtra::column_spec(2:4,italic = TRUE)

    k_state <- "In Progress"
    k_colour <- "white"
    output$project_list_active <- presentation_data$projects %>% 
      mutate(k.Project=ifelse(State==k_state,k.Project,kableExtra::cell_spec("",background = k_colour, color=k_colour)),
             k.Progress=ifelse(State==k_state,k.Progress,""),
             k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
             k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
      ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
      arrange(k.Project) %>%
      knitr::kable(format = "html", escape = F,col.names = NULL,booktabs = T) %>%
      kableExtra::kable_styling(full_width = F) %>%
      kableExtra::column_spec(2:4,italic = TRUE)
  
  
    k_state <- "Complete"
    k_colour <- "white"
    
    output$project_list_complete <- presentation_data$projects %>% 
      mutate(k.Project=ifelse(State==k_state,k.Project,kableExtra::cell_spec("",background = k_colour, color=k_colour)),
             k.Progress=ifelse(State==k_state,k.Progress,""),
             k.Overdue_Tasks=ifelse(State==k_state,k.Overdue_Tasks,""),
             k.Open_Issues=ifelse(State==k_state,k.Open_Issues,"") 
      ) %>% select(k.Project,k.Progress,k.Overdue_Tasks,k.Open_Issues) %>%
      arrange(k.Project) %>%
      knitr::kable(format = "html", escape = F,col.names = NULL,booktabs = T) %>%
      kableExtra::kable_styling(full_width = F) %>%
      kableExtra::column_spec(2:4,italic = TRUE)
    
    output
 
}

projects_roadmap <- function(presentation_data,app_vars,roadmap_start_date, roadmap_end_date){
  library(plotly)  

 # roadmap_start_date <- lubridate::as_date(cut(app_vars$today, "month"))
  
  roadmap_table<-presentation_data$projects %>%
    mutate(no_start=is.na(strftime(start)),
           no_end=is.na(strftime(end))) %>%
    filter(!(no_start) & !(no_end)) %>%
    mutate(orig_start=start,orig_end=end) %>%
    mutate(start=lubridate::as_date(ifelse(no_start,end,start)),
           end = lubridate::as_date(ifelse(no_end,start,end))) %>%
    mutate(start=lubridate::as_date(ifelse(start<roadmap_start_date,  roadmap_start_date,start)),
           end=lubridate::as_date(ifelse(end<roadmap_start_date,(roadmap_start_date - lubridate::ddays(1)),
                              ifelse(end>roadmap_end_date,roadmap_end_date,end)))) %>%
    filter(end>=roadmap_start_date) %>%
    mutate(comments=paste("Start:", orig_start,
                          " - End:",orig_end,
                          " - State :", RAG,
                          " ",RAG_comment,
                          sep = "")) %>%
    select(event=Name,start,end,group=State,color=State_colour,tooltip=comments)   
  
  roadmap_table <- add_row(roadmap_table,
                           event ="Project Roadamp", start=roadmap_start_date,end=roadmap_end_date,group="Roadmap",color="white", tooltip="")
  
  tl<-vistime::vistime(roadmap_table, title = "Roadmap",optimize_y=FALSE)
  
  line <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y",
    x0 = app_vars$today, 
    x1 = app_vars$today,
    y0 = 0,
    y1 = nrow(projects_roadmap)+3
    
  )
  
  tl <- layout(tl, title = 'Project Roadmap', shapes = line) 
  
  tl
  
}

projects_summary <- function(presentation_data,app_vars){
  
  project_list <- presentation_data$projects %>% 
    mutate(condition1=(!(State %in% c("Complete")) & !is.na(strftime(end))),
           condition2=(State %in% c("Planning","In Progress")),
           condition3= (State %in% c("Complete") & (end+lubridate::ddays(15)>app_vars$today)),
           select = condition1 | condition2 | condition3
    ) %>%
    filter(select) %>%
    select(" "=t.RAG,State=t.State,Due=t.end,"Project Name"=t.Project,
           "%"=t.Progress,OD=t.Overdue_Tasks,
           Issues=t.Open_Issues,Update=t.UpdateDate,Done=t.Done,"To Do"=t.ToDo) 
  p_rows <- nrow(project_list)
  
  if(p_rows>1){
    project_list %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)  %>%
      kableExtra::column_spec(2, width_min="7em") %>%
      kableExtra::column_spec(3, width_min="5em") %>%
      kableExtra::column_spec(4, width_min="8em") %>%
      kableExtra::column_spec(8,width_min="6em") %>%
      kableExtra::column_spec(9,width_min="9em") %>%
      kableExtra::column_spec(10,width_min="9em") %>%
      kableExtra::row_spec(seq(1,p_rows,2),background =app_vars$c.background3,hline_after=TRUE) %>%
      kableExtra::row_spec(seq(2,p_rows,2),background =app_vars$c.background4,hline_after=TRUE)
  }else{
    project_list %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)
    
  }

}

issues_summary <- function(presentation_data,app_vars,project_name="all"){
  
  issue_list <- presentation_data$issues %>% 
    mutate(condition1=((State %in% c("Open")) ),
           condition2= (State %in% c("Closed") & (due+lubridate::ddays(15)>app_vars$today)),
           select = condition1 | condition2
    ) %>%
    filter(select) 
  
  if(project_name=="-1"){project_name <-presentation_data$projects[1,]$Name}
  if(!(project_name=="all")){issue_list <- issue_list %>% filter(Project==project_name)}
  
  issue_list <- issue_list %>% arrange(due) %>%
    select(" "=t.RAG,State=t.State,"Project Name"=t.Project,
           Issue=t.Issue,Assignee=t.Assignee,			
           Impact=t.Impact,Action=t.Action,Due=t.due) 
  
  p_rows <- nrow(issue_list)

  if(p_rows>1){
    issue_list %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)  %>%
      kableExtra::row_spec(seq(1,p_rows,2),background =app_vars$c.background3,hline_after=TRUE) %>%
      kableExtra::row_spec(seq(2,p_rows,2),background =app_vars$c.background4,hline_after=TRUE) %>%
      kableExtra::column_spec(8,width_min="5em") 
  }
  else{
    issue_list %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)
  }
    
}

actions_summary <- function(presentation_data,app_vars,project_name="all",min_action_width="20em"){
  
  action_list <- presentation_data$actions %>% 
    mutate(condition1=((State %in% c("Open")) ),
           condition2= (State %in% c("Closed") & (due+lubridate::ddays(15)>app_vars$today)),
           selection = condition1 | condition2) %>%
    filter(selection)

  if(project_name=="-1"){project_name <-presentation_data$projects[1,]$Name}
  if(!(project_name=="all")){action_list <- action_list %>% filter(Project==project_name)}
  
  
  action_list <- action_list %>%
    arrange(due) %>%
    select(" "=t.RAG,State=t.State,"Project Name"=t.Project,
           Action=t.Action, Assignee=t.assignee, Due=t.due) 
  
  p_rows <- nrow(action_list)
  
  if(p_rows>1){
    action_list %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)  %>%
      kableExtra::row_spec(seq(1,p_rows,2),background =app_vars$c.background3,hline_after=TRUE) %>%
      kableExtra::row_spec(seq(2,p_rows,2),background =app_vars$c.background4,hline_after=TRUE) %>%
      kableExtra::column_spec(4,width_min=min_action_width) %>%
      kableExtra::column_spec(6,width_min="5em") 
  }else{
    action_list %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T) 
  
  }
    
}

consolidated_tasks <- function(presentation_data,app_vars){
  
  X_rows <- which(presentation_data$t.consolidated_tasks$row_colour=="X")
  U_rows <- which(presentation_data$t.consolidated_tasks$row_colour=="U")
  M_rows <- which(presentation_data$t.consolidated_tasks$row_colour=="M")
  L_rows <- which(presentation_data$t.consolidated_tasks$row_colour=="L")
  
    presentation_data$t.consolidated_tasks %>%
      select(" "=t.RAG,Project=t.Project,Type,State=t.State,Due=t.due,Item=t.Item,Assignee=t.assignee)  %>% 
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = F) %>%
      kableExtra::row_spec(X_rows,background=app_vars$c.X_rows,hline_after=TRUE) %>%
      kableExtra::row_spec(U_rows,background=app_vars$c.U_rows,hline_after=TRUE) %>%
      kableExtra::row_spec(M_rows,background=app_vars$c.M_rows,hline_after=TRUE) %>%
      kableExtra::row_spec(L_rows,background=app_vars$c.L_rows,hline_after=TRUE) %>%
      kableExtra::column_spec(4, width_min="7em") %>%
      kableExtra::column_spec(5, width_min="5em") 

}

people_freq <- function(presentation_data,app_vars){
  
    presentation_data$consolidated_stats$person_frequency %>% 
      left_join(app_vars$font_sizes,by="nbr") %>%
      mutate(Assignee=kableExtra::cell_spec(assignee,font_size = size, 
                                bold = (nbr %in% c(1,2,3))
                                ,
                                color = ifelse((nbr %in% c(1,2,3)),
                                               "navy",
                                               app_vars$t.default_colour))) %>%
      mutate(Workload=kableExtra::cell_spec(freq,font_size = size, 
                                bold = (nbr %in% c(1,2,3))
                                ,
                                color = ifelse((nbr %in% c(1,2,3)),
                                               "navy",
                                               app_vars$t.default_colour))) %>%
      select(Assignee,Workload)%>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = F) %>%
      kableExtra::column_spec(1,width_min="20em")

}

project_freq <- function(presentation_data,app_vars){
  
    presentation_data$consolidated_stats$project_frequency %>% 
      left_join(app_vars$font_sizes,by="nbr") %>%
      mutate(Project=kableExtra::cell_spec(Project,font_size = size, 
                               bold = (nbr %in% c(1,2,3))
                               ,
                               color = ifelse((nbr %in% c(1,2,3)),
                                              "navy",
                                              app_vars$t.default_colour))) %>%
      mutate(Workload=kableExtra::cell_spec(freq,font_size = size, 
                                bold = (nbr %in% c(1,2,3))
                                ,
                                color = ifelse((nbr %in% c(1,2,3)),
                                               "navy",
                                               app_vars$t.default_colour))) %>%
      select(Project,Workload)%>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = F) %>%
      kableExtra::column_spec(1,width_min="20em")
    
}

stats_summary1 <- function(presentation_data){
  output <-vector(mode = "list", length = 0)
  
  output$Backlog <-  presentation_data$programme_stats[1,]$Backlog
  output$Planning <-  presentation_data$programme_stats[1,]$Planning
  output$InProgress <-  presentation_data$programme_stats[1,]$"In Progress"
  output$Complete <-  presentation_data$programme_stats[1,]$Complete
  output$R <-  presentation_data$programme_stats[1,]$R
  output$A <-  presentation_data$programme_stats[1,]$A
  output$G <-  presentation_data$programme_stats[1,]$G
  
  output
  
}

stats_summary2 <- function(presentation_data){
  output <-vector(mode = "list", length = 0)
  
  output$late.start <- presentation_data$programme_stats[1,]$late.start
  output$overdue <- presentation_data$programme_stats[1,]$overdue
  output$late.tasks <- presentation_data$programme_stats[1,]$late.tasks
  output$issue.R <- presentation_data$programme_stats[1,]$issue.R
  output$issue.A <- presentation_data$programme_stats[1,]$issue.A
  output$open.actions <- presentation_data$programme_stats[1,]$open.actions

  output
}

stats_daily <- function(presentation_data){
  output <-vector(mode = "list", length = 0)
  
  output$R <- presentation_data$consolidated_stats$R
  output$A <- presentation_data$consolidated_stats$A
  output$G <- presentation_data$consolidated_stats$G

  
  output
  
}

last_update_date <-function(rendered_data){
 # namesvalues <- names(rendered_data)
  datevalue <- shiny::isolate(rendered_data$data_retrieved)
  if(!exists("rendered_data")){
    "Never Updated"
  }else{
    if(!("data_retrieved" %in% c("data_retrieved"))){
      "Never Updated"
    }else{  
      if(is.na(strftime(datevalue))){
        
        "Never Updated"
        
      }else{
        
        
        
        paste("Last Update:",paste(lubridate::day(datevalue)," ",
                                   lubridate::month(datevalue,label=TRUE), " ",
                                   lubridate::year(datevalue)," -- ",
                                   ifelse(lubridate::hour(datevalue)<10,paste("0",lubridate::hour(datevalue),sep=""),lubridate::hour(datevalue)),
                                   ":",
                                   ifelse(lubridate::minute(datevalue)<10,paste("0",lubridate::minute(datevalue),sep=""),lubridate::minute(datevalue)),sep=""),sep=" ")
      }}
  }
}

project_data <- function(presentation_data,selected_project="-1"){
  
  if(selected_project=="-1"){selected_project <-presentation_data$projects[1,]$Name}
  
  presentation_data$projects %>% filter(Name==selected_project) %>% 
    select(t.RAG,Project=Name,t.State,Scope,Objectives,
           t.Project_Lead,t.Project_Manager,
           t.start,t.end,
           task.R,task.A,task.G,task.g,task.progress,
           action.open,issue.open,RAG,RAG_colour2=RAG_colour,url) %>%
    mutate(icon=ifelse(RAG=="R","exclamation triangle",
                       ifelse(RAG=="A",
                       "exclamation circle","thumbs up outline")),
           RAG_colour=ifelse(RAG=="R","red",
                             ifelse(RAG=="A",
                                    "orange",
                                    ifelse(RAG=="G","green","grey"))),
           Project= paste("<a href=\"",url,"\"><p style=\"color:",RAG_colour,"\">",Project,"</p></a>")
           )
  
}

project_tasks <- function(presentation_data,app_vars,project_name="-1",filter_tasks="none"){
  

  if(project_name=="-1"){project_name <-presentation_data$projects[1,]$Name}
  
  task_list <- presentation_data$tasks %>% filter(Project_Name==project_name)
  
  
  if(!(filter_tasks=="none")){
    if(filter_tasks=="Complete"){task_list <- task_list %>% filter(!(State %in% c("Complete")))}
    if(filter_tasks=="Green"){task_list <- task_list %>% filter(!(RAG %in% c("G","g")))}    
  }
  
  task_list <- task_list %>% group_by(Group) %>%
    arrange(Number,.by_group = TRUE) %>% ungroup() %>%
    select(" "=t.RAG,No=t.Number,Task=t.Task,Assignee=t.assignee,State=t.State,Start=t.start,End=t.end) 
  
  p_rows <- nrow(task_list)
  
  if(p_rows>1){
  t<-task_list %>%
    knitr::kable(format = "html", escape = F) %>%
    kableExtra::kable_styling(full_width = T)  %>%
    kableExtra::row_spec(seq(1,p_rows,2),background =app_vars$c.background3,hline_after=TRUE) %>%
    kableExtra::row_spec(seq(2,p_rows,2),background =app_vars$c.background4,hline_after=TRUE) %>%
    kableExtra::column_spec(4,width_min="10em") %>%
    kableExtra::column_spec(6,width_min="5em") %>%
    kableExtra::column_spec(7,width_min="5em") 
  }else{
   t<-  task_list %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)
  }
  
  t
}

project_plan <- function(presentation_data,project_name="-1"){
  library(plotly)  

  if(project_name=="-1"){project_name <-presentation_data$projects[1,]$Name}
  
  roadmap_table<-presentation_data$tasks %>% filter(Project_Name==project_name)
  
  roadmap_table<-presentation_data$tasks %>% filter(Project_Name==project_name) %>%
    mutate(no_start=is.na(strftime(start)),
           no_end=is.na(strftime(end))) %>%
    filter(!(no_start) & !(no_end)) %>%
    mutate(orig_start=start,orig_end=end) %>%
    mutate(start=lubridate::as_date(ifelse(no_start,end,start)),
           end = lubridate::as_date(ifelse(no_end,start,end)))  %>%
    mutate(comments=paste("Start:", orig_start,
                          " - End:",orig_end,
                          " - State :", RAG,
                          " ",RAG_comment,
                          sep = "")) %>%
    mutate(event=paste(Number,Task)) %>%
    select(event,start,end,group=Group,color=RAG_colour,tooltip=comments)   
  
  p_rows <- nrow(roadmap_table)
  
  if(p_rows>0){
  
  tl<-vistime::vistime(roadmap_table, title = "Project Plan",optimize_y=FALSE)
  
  line <- list(
    type = "line",
    line = list(color = "orange"),
    xref = "x",
    yref = "y",
    x0 = app_vars$today, 
    x1 = app_vars$today,
    y0 = 0,
    y1 = nrow(projects_roadmap)+3
    
  )
  
  tl <- layout(tl, title = 'Project Plan', shapes = line) 
  
  tl
  }
}

project_updates <- function(presentation_data,app_vars,project_name="-1"){
  
  if(project_name=="-1"){project_name <-presentation_data$projects[1,]$Name}
  
  updates <- presentation_data$updates %>% filter(Project==project_name) %>%
    mutate(
      Update=kableExtra::cell_spec(Update),
      Done=kableExtra::cell_spec(Done),
      ToDo=kableExtra::cell_spec(ToDo)
    ) %>% select(Update,Done,"To Do"=ToDo)
  
  p_rows <- nrow(updates)
  
  if(p_rows>1){
    updates %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)  %>%
      kableExtra::column_spec(1,width_min="6em") %>%
      kableExtra::column_spec(2,width_min="9em") %>%
      kableExtra::column_spec(3,width_min="9em") %>%
      kableExtra::row_spec(seq(1,p_rows,2),background =app_vars$c.background3,hline_after=TRUE) %>%
      kableExtra::row_spec(seq(2,p_rows,2),background =app_vars$c.background4,hline_after=TRUE)
  }else{
    updates %>%
      knitr::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling(full_width = T)  
    
  }
  
}

past_reports <- function(version_parameter="current",app_vars,snapshots_version){
  
  if(!(version_parameter=="current")){
    version_parameter <- snapshots_version[which(snapshots_version$version==version_parameter),]$timestamp
  }
  
  
  t <- app_vars$history_call %>% mutate(location=paste(app_vars$URL ,type,base,
                                              ifelse(type=="api","?version=",""),
                                              version_parameter,
                                              ifelse(type=="api","",".html"),
                                              sep="")) %>%
    mutate(link=kableExtra::cell_spec(location,link = location))
  
  if(version_parameter=="current"){
    
   t <- t %>% filter(type=="api")
    
  }
  
  
  t  %>%
    select(Name,Link=link) %>%
    knitr::kable(format = "html", escape = F,booktabs = T) %>%
    kableExtra::kable_styling(full_width = F) 
  
  }


message("Render functions loaded")