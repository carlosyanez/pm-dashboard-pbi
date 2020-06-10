
download_excel <- function(file_location,url_value,date_last_activity){
  
 #file_location <- app_vars$excel_files 
 #url_value <- app_vars$url_value
 #date_last_activity <- app_vars$date_last_activity
  
  projects_file <- paste(file_location,"Projects.xlsx",sep="")
  
  Projects <- readxl::read_excel(projects_file, 
                                 sheet = "Projects")#, col_types = c("text", "text", "text",
                                                    #               "text", "text", "numeric", 
                                                    #               "numeric", "text", "text", "text"))
  
  Project_Updates <- readxl::read_excel(projects_file, 
                                        sheet = "Updates", col_types = c("text", 
                                                                         "text", "text", "text"))
  
  Project_Actions <- readxl::read_excel(projects_file, 
                                        sheet = "Actions")#, col_types = c("text", 
                                                          #        "text", "text", "text","text","text"))
  
  
  file_names <-  Projects %>% mutate(name=paste(No," - ",`Project Name`,".xlsx",sep="")) %>%
    select(No,`Project Name`,name)
  
  files <- tibble(name=list.files(path = file_location)) %>% 
    filter(name %in% file_names$name) %>% left_join(file_names,by="name")
  
  
  ###first run
  
  i<-1
  ## project details
  project_details <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                        sheet = "Project_Details",
                                        col_types = c("text", 
                                                      "text")) 
  
  project_details <- project_details %>%  mutate(ProjectID=files[i,]$No,
                                                 Project=files[i,]$"Project Name") %>%
    pivot_wider(names_from = a,values_from = b) 
  
  
  
  project_tasks <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                      sheet = "Plan")#,
                                      #col_types = c("text", "text", "text", "text", 
                                      #              "text", "text", "text"))
  
  project_tasks <- project_tasks[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                             Project=files[i,]$"Project Name") 
  
  
  project_update <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                       sheet = "Updates")#,
                                       #col_types = c("text", "text", "text"))
  
  
  project_update <- project_update[,1:3] %>%  mutate(ProjectID=files[i,]$No,
                                               Project=files[i,]$"Project Name") 
  
  
  project_issues <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                       sheet = "Issues")#,
                                      # col_types = c("text", "text", "text", "text", 
                                       #              "text", "text", "text"))
  
  project_issues <- project_issues[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                               Project=files[i,]$"Project Name") 
  
  # more than one file
  
  if(nrow(files)>1){
    
    for (i in 2:nrow(files)){
      
      
      ## project details
      project_details_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                              sheet = "Project_Details",
                                              col_types = c("text", "text")) 
      
      project_details_i <- project_details_i %>%  mutate(ProjectID=files[i,]$No,
                                                         Project=files[i,]$"Project Name") %>%
        pivot_wider(names_from = a,values_from = b) 
      
      
      
      project_tasks_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                            sheet = "Plan")#,
                                            #col_types = c("text", "text", "text", "text", 
                                            #              "text", "text", "text"))
      
      project_tasks_i <- project_tasks_i[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                                     Project=files[i,]$"Project Name") 
      
      
      project_update_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                             sheet = "Updates")#,
                                            # col_types = c("text", "text", "text"))
      
      
      project_update_i <- project_update_i[,1:3] %>%  mutate(ProjectID=files[i,]$No,
                                                       Project=files[i,]$"Project Name") 
      
      project_issues_i <- readxl::read_excel(paste(file_location,files[i,]$name,sep=""), 
                                             sheet = "Issues")#,
                                             #col_types = c("text", "text", "text", "text", 
                                            #               "text", "text", "text"))
      
      project_issues_i <- project_issues_i[,1:7] %>%  mutate(ProjectID=files[i,]$No,
                                                       Project=files[i,]$"Project Name")
      
      
      ##bind together
      
      project_details <- rbind(project_details_i,project_details)
      project_tasks <- rbind(project_tasks_i,project_tasks)
      project_update <- rbind(project_update_i,project_update)
      project_issues <- rbind(project_issues_i,project_issues)
    }
    
    rm(project_details_i,project_tasks_i,project_update_i,project_issues_i,i)    
  }
  

  output <-vector(mode = "list", length = 0)
  
  #tasks
  #"Number","Task","due","State","Group","Project_Name","assignee","id","Project_id","url","dateLastActivity"
  
  output$tasks<-project_tasks %>% mutate(id=paste("task",ProjectID,No,sep="."),
                                         url=url_value,
                                         start=lubridate::as_date(as.Date(Start),tz=NULL),
                                         due=lubridate::as_date(as.Date(End), tz = NULL),
                                         dateLastActivity=lubridate::as_date(date_last_activity, tz = NULL),
                                         Project_id=as.character(ProjectID)) %>%
    select(Number=No,Task,start=start,due=due,State=Status,Group=Phase,Project_Name=Project,
           assignee=Responsible,id,Project_id=Project_id,url,dateLastActivity)
  
  #issues
  #"Severity","Project","Title","due","Issue","Impact","Action","State","Assignee","id","Project_id","url"
  
  
  output$issues <- project_issues %>% mutate(id = paste("issue",1:n(),sep="-"),
                                             url=url_value,
                                             due=lubridate::as_date(Due, tz = NULL),
                                             Project_id=as.character(ProjectID)) %>%
    select(Severity,Project,Title=Issue,due=due,Issue=Issue,Impact,Action,State,Assignee,
           id,Project_id=Project_id,url)
  
  #Actions
  #"","action","assignee","due","id","Project_id","Task_id","Project","Replacement","State","url"
  
  output$actions <- Project_Actions  %>% mutate(id = paste("action",1:n(),sep="-"),
                                                url=url_value,
                                                due=lubridate::as_date(Due, tz = NULL),
                                                No=as.character(No)) %>%
    select(action=Action,assignee=Responsible,due=due,id,Project_id=No,Project,Task_id=id,
           Replacement=State,
           State=State,
           url)
  
  #comments
  #"","id","card_id","author","comment","date","Done","ToDo"
  
  output$comments <- rbind(Project_Updates %>% mutate(ToDo="") %>%
                             select(Date,
                                    Done="Update/Comments",
                                    ToDo,ProjectID=No,Project),
                           project_update %>% select(Date,Done,ToDo="To Do",
                                                     ProjectID,Project)) %>% 
    mutate(id = paste("comment",1:n(),sep="-"),
           author="",
           comment=paste(Done,ToDo,sep="-"),
           date=lubridate::as_date(Date, tz = NULL)) %>%
    select(id,card_id=ProjectID,author,comment,date=date,Done,ToDo)
  
  
  ### Projects
  # Name              State              labels               due                  id    
  #    Scope            Objectives        Project_Lead       Project_Manager     Parameters 
  #   Project_id          card_id              url             updates_id     
  
  dates_task <- output$tasks %>% group_by(Project_id) %>% 
    summarise(start_t=min(start),due_t=max(due)) %>%
    ungroup() %>% select(No=Project_id,start_t,due_t)
  
  output$projects <- Projects %>% left_join((project_details %>% mutate(No=ProjectID)),by="No") %>%
    #   left_join(dates_task,by="No") %>%
    mutate(No=as.character(No),
           Scope=ifelse(is.na(Scope.y),Scope.x,Scope.y),
           Objectives=ifelse(is.na(Objectives.y),Objectives.x,Objectives.y),
           Project_Lead=ifelse(is.na(.$"Project Lead.y"),.$"Project Lead.x",.$"Project Lead.x"),
           Project_Manager=ifelse(is.na(.$"Project Manager"),PM,.$"Project Manager"),
           Parameters ="",
           labels="",
           url=Link,
           due=lubridate::as_date(End,tz = NULL)) %>%
    select(Name="Project Name",State=State,labels,
           due=due,id=No,
           Scope,Objectives,Project_Lead,
           Project_Manager,Parameters,
           Project_id=No,card_id=No,url,updates_id=No)
  
  output

}


normalised_data <- download_excel(app_vars$excel_files,app_vars$url_value,app_vars$date_last_activity)
normalised_data$data_retrieved <- if(app_vars$source_system=="Demo"){app_vars$demo_now}else{lubridate::now()}


