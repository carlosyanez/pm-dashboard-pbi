### Select from different loading sources.

if(app_vars$source_system=="Trello"){
  
    source("./r/trello.R", echo = F, prompt.echo = "", spaced = F)
  
}else{
  if(app_vars$source_system=="Excel"){
    
    source("./r/excel.R", echo = F, prompt.echo = "", spaced = F)
    
  }else{
    
    app_vars$excel_files  <- app_vars$demo_files
    source("./r/excel.R", echo = F, prompt.echo = "", spaced = F)
    
  }
  
  
  
  
}