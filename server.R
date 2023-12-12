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
  
  hide(id = "loading-gray-overlay")
  launch_tutorial()
 
  
})
