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
    
    wd <-"/Users/rich401/OneDrive-PNNL (Archive)/Desktop/Cleaned_Data_for_Rachel"
    # wd <-"data_temp"
    # wd <-"tmp/s3-data"

    AWSobj$e_data <- read.csv(file.path(wd, "e_data.csv"))
    AWSobj$f_data <- read.csv(file.path(wd, "f_data.csv"))
    AWSobj$e_meta <- read.csv(file.path(wd, "e_meta.csv"))

    # AWSobj$e_data <- read.csv(file.path(wd, "e_data_pmartR.csv"))
    # AWSobj$e_data <- read.csv(file.path(wd, "956f165a-5752-4214-b9c7-114664ce2ed6_e_data.csv"))[1:12000,]
    # AWSobj$f_data <- read.csv(file.path(wd, "b76efbf1-f698-482c-9df0-f1978957fd6e_f_data.csv"))
    # AWSobj$e_meta <- read.csv(file.path(wd, "d78a0ef5-bfc4-4afa-bf96-db7c68ea0dde_e_meta.csv"))
    #
    
    
    ## Temp fix for razor proteins
    # observeEvent(AWSobj$e_data, {
    #   AWSobj$e_data <- AWSobj$e_data[apply(!is.na(AWSobj$e_data), 2, sum) > 2,]
    #   
      # write.csv(AWSobj$e_data, "e_data_short.csv", row.names = F)
      # write.csv(AWSobj$f_data, "f_data_short.csv", row.names = F)
      # # write.csv(AWSobj$e_meta, "e_meta.csv", row.names = F)
    # }, once = T)
    # 
    # observeEvent(AWSobj$e_meta, {
    #   AWSobj$e_meta <- unique(AWSobj$e_meta[colnames(AWSobj$e_meta) != "Proteins"])
    # }, once = T)
    # 
    # # Specify file type and disable input
    # updatePickerInput(session, "data_type", selected = "Label-free")
    # 
  } else {
    hide(id = "loading-gray-overlay")
  }

  launch_tutorial()
 
  
})
