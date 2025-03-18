
## Download behavoir
observeEvent(input$makezipfile, {
  disable("makezipfile")
  on.exit({
    enable("makezipfile")
  })
  
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  temp_dir <- tempdir()
  setwd(temp_dir)
  
  fname <- file.path(temp_dir, 
                     paste0("SLOPE_Predict_", gsub("( |:|-)", "_", Sys.time())))
  
  plots_chk <- reactiveValuesToList(plot_table_current)$table
  plots_chk[sapply(plots_chk, is.null)] <- NULL
  
  tables_chk <- reactiveValuesToList(table_table_current)$table
  tables_chk[sapply(tables_chk, is.null)] <- NULL
  
  plots_keep <- getShinyInput(input, "checkplot")
  tables_keep <- getShinyInput(input, "checktable")
  plots_keep <- unlist(sapply(1:length(plots_keep), \(x) if (plots_keep[x] == 1) x else NULL))
  tables_keep <- unlist(sapply(1:length(tables_keep), \(x) if (tables_keep[x] == 1) x else NULL))
  
  plots_export <- plots_chk[plots_keep]
  tables_export <- tables_chk[tables_keep]
  
  # Write plots
  if (length(plots_export) > 0) {
    
    withProgress(message = "Writing plot files...", {
      
      file_names_plots <- map2(plots_export, names(plots_export), function(plot, handle){
        if (inherits(plot, "plotly")) {
          fname <- paste0(handle, ".html")
          saveWidget(plot, fname)
        } else {
          fname <- paste0(handle, ".png")
          ggsave(fname, 
                 plot = plot#, 
                 # width = save_options$width, 
                 # height = save_options$height, 
                 # scale = save_options$scale, 
                 # units="px"
          )
        }
        incProgress(1 / length(plots_export), 
                    detail = paste0(fname, " done"))
        fname
      })
    })
    
  } else file_names_plots <- NULL
  
  # Write tables
  if (length(tables_export) > 0) {
    
    withProgress(message = "Writing data files...", {
      
      file_names_tables <- map2(tables_export, 
                                names(tables_export), function(table, handle){
                                  
                                  write.csv(table, 
                                            file = paste0(handle, ".csv"), 
                                            row.names = FALSE)
                                  
                                  incProgress(1 / length(tables_export), 
                                              detail = paste0(paste0(handle, ".csv"), " done"))
                                  paste0(handle, ".csv")
                                })
    })
  } else file_names_tables <- NULL
  
  # Write .Rdata
  if((!is.null(input$include_model)) && input$include_model){
    
    withProgress(message = "Writing model RDS object...",{
      
      file_names_models <- paste0("Predict_model_", fs::path_sanitize(input$RDS_name))
      
      saveRDS(omicsData$obj_predictions, file = file_names_models)
      
    })
  } else file_names_models <- NULL
  
  zip(zipfile = paste0(fname, ".zip"), 
      files = c(file_names_tables, file_names_plots, file_names_models), 
      flags = "-r")
  
  zipped_file$fs <- paste0(fname, ".zip")
})

output$download_processed_data <- downloadHandler(
  filename = function() {
    paste0("SLOPE_output_", gsub("( |:|-)", "_", Sys.time()), ".zip")
  },
  content = function(fname) {
    file.copy(zipped_file$fs, fname)
  },
  contentType = "application/zip"
)