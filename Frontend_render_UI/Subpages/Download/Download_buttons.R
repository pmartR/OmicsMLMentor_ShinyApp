
## Download behavoir

observeEvent(input$makezipfile, {
  disable("makezipfile")
  on.exit({
    enable("makezipfile")
  })
  
  method <- input$pick_model_EM
  
  fname <- paste0("SLOPE_", method, 
                  "_", gsub("( |:|-)", "_", Sys.time()))
  
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  temp_dir <- tempdir()
  setwd(temp_dir)
  
  fname <- file.path(temp_dir, 
                     paste0("SLOPE_", method, 
                            "_", gsub("( |:|-)", "_", Sys.time())))

  plots <- flattenlist(reactiveValuesToList(plot_table_current))
  tables <- flattenlist(reactiveValuesToList(table_table_current))
  
  plots_keep <- getShinyInput(input, "checkplot")
  tables_keep <- getShinyInput(input, "checktable")
  
  plots_export <- plots[plots_keep]
  tables_export <- tables[tables_keep]
  
  # Write plots
  if (length(plots_export) > 0) {
    
    withProgress(message = "Writing plot files...", {
      
      file_names_plots <- map2(plots_export, names(plots_export), function(plot, handle){
        ggsave(file.path(tempdir(), paste0(handle, ".png")), 
               plot = plot#, 
               # width = save_options$width, 
               # height = save_options$height, 
               # scale = save_options$scale, 
               # units="px"
               )
        
        incProgress(1 / length(plots_export), 
                    detail = paste0(paste0(handle, ".png"), " done"))
        file.path(tempdir(), paste0(handle, ".png"))
      })
    })
    
  } else file_names_plots <- NULL
  
  # Write tables
  if (length(tables_export) > 0) {
    
    withProgress(message = "Writing data files...", {
      
      file_names_tables <- map2(tables_export, 
                                names(tables_export), function(table, handle){
        
        write.csv(table, 
                  file = file.path(tempdir(), paste0(handle, ".csv")), 
                  row.names = FALSE)
        
        incProgress(1 / length(tables_export), 
                    detail = paste0(paste0(handle, ".csv"), " done"))
        file.path(tempdir(), paste0(handle, ".csv"))
      })
    })
  } else file_names_tables <- NULL
  
  # Write .Rdata
  
  # Write report
  
  zip(zipfile = fname, 
      files = c(file_names_tables, file_names_plots), 
      flags = "-r")
  
  zipped_file$fs <- c(zipped_file$fs, paste0(fname, ".zip"))
})


### Download button ###
output$download_processed_data <- downloadHandler(
  filename = paste0("SLOPE_output_", gsub("( |:|-)", "_", Sys.time()), ".zip"),
  content = function(fname) {
    
    # this is necessary to zip up the entire trelliscope directory
    # specifically, we dont want to have to use the -j flag to get non-directory files..
    # .. so, we navigate to where everything is (tempdir()) and download with just -r
    
    orig_wd <- getwd()
    on.exit(setwd(orig_wd))
    setwd(tempdir())
    
    zip(zipfile = fname, files = zipped_file$fs, flags = "-r")
    if (file.exists(paste0(fname, ".zip"))) {
      file.rename(paste0(fname, ".zip"), fname)
    }
  },
  contentType = "application/zip"
)