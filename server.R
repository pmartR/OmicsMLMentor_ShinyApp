options(shiny.maxRequestSize = 250 * 1024^2, ch.dir = TRUE, DT.TOJSON_ARGS = list(na = "string"))

shinyServer(function(session, input, output) {
  
  file_loads <- c(
    list.files("./Modules/", recursive = T, full.names = T),
    list.files("./Frontend_render_UI/", recursive = T, full.names = T)
  )
  
  file_loads <- file_loads[file_loads != "./Modules//Module_RV_all.R"]
  source("./Modules//Module_RV_all.R", local = T) ## must be first
  
  # file_loads <- grep("filter", file_loads, invert = T, value = T)
  
  for (f in grep(".R$", file_loads, value = T)){
    source(f, local = TRUE)
  }
  
  if (AWS) {
    
    message("AWS VERSION ENABLED")
    
    # Load AWS specific R library
    library(aws.s3)
    
    # Load AWS specific code
    source("./AWS_Functions.R", local = TRUE)
    
    ######## comment this out before push
    
    # wd <-"/Users/rich401/OneDrive-PNNL (Archive)/Desktop/Cleaned_Data_for_Rachel"
    # # wd <-"data_temp"
    # 
    # # AWSobj$e_data <- read.csv(file.path(wd, "e_data.csv"))
    # # AWSobj$f_data <- read.csv(file.path(wd, "f_data.csv"))
    # # AWSobj$e_meta <- read.csv(file.path(wd, "e_meta.csv"))
    # 
    # # AWSobj$e_data <- read.csv(file.path(wd, "e_data_pmartR.csv"))
    # AWSobj$e_data <- read.csv(file.path(wd, "e_data_pmartR_no_log.csv"))
    # AWSobj$f_data <- read.csv(file.path(wd, "f_data_pmartR.csv"))
    # AWSobj$e_meta <- read.csv(file.path(wd, "e_meta_pmartR.csv"))
    # # 
    # ## Temp fix for razor proteins
    # observeEvent(AWSobj$e_meta, {
    #   AWSobj$e_meta <- unique(AWSobj$e_meta[colnames(AWSobj$e_meta) != "Proteins"])
    # }, once = T)
    # 
    # # Specify file type and disable input
    # updatePickerInput(session, "data_type", selected = "Protein")
    
  } else {
    hide(id = "loading-gray-overlay")
  }

  launch_tutorial()
 
  
})
