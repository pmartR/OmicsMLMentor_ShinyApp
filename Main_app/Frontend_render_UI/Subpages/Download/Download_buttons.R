
## Download behavior

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

  plots_chk <- reactiveValuesToList(plot_table_current)$table
  tables_chk <- reactiveValuesToList(table_table_current)$table
  
  plots_keep <- getShinyInput(input, "checkplot")
  tables_keep <- getShinyInput(input, "checktable")
  plots_keep <- unlist(sapply(1:length(plots_keep), \(x) if (plots_keep[x] == 1) x else NULL))
  tables_keep <- unlist(sapply(1:length(tables_keep), \(x) if (tables_keep[x] == 1) x else NULL))
  
  plot_idx <- NULL
  
  for (pg in c("Upload", "QC", "MSU", "PP", "RM")) {
    plot_table <- tibble(
      rows = plot_table_current$names[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))],
      list_el = plot_table_current$table[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    table_with_checks <- plot_table[which(map_lgl(plot_table$rows, \(x) !is.null(x))),]
    
    if (any(names(plots_chk) %in% names(table_with_checks$list_el)))
      plot_idx <- c(plot_idx, which(names(plots_chk) %in% names(table_with_checks$list_el)))
  }
  
  tbl_idx <- NULL
  for (pg in c("Upload", "QC", "MSU", "PP", "RM")) {
    table_table <- tibble(
      rows = table_table_current$names[which(startsWith(names(table_table_current$table), paste0(pg, "__")))],
      list_el = table_table_current$table[which(startsWith(names(table_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    table_with_checks <- table_table[which(map_lgl(table_table$rows, \(x) !is.null(x))),]
    
    if (any(names(tables_chk) %in% names(table_with_checks$list_el)))
      tbl_idx <- c(tbl_idx, which(names(tables_chk) %in% names(table_with_checks$list_el)))
  }
  
  plots_export <- plots_chk[plot_idx][plots_keep]
  tables_export <- tables_chk[tbl_idx][tables_keep]
  
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
  if(!is.null(input$include_model) && input$include_model){
    
    withProgress(message = "Writing model RDS object...",{
      
      file_names_models <- c(paste0("Full_model_", fs::path_sanitize(input$RDS_name)))
      
      saveRDS(omicsData$objRM, file = file_names_models[1])
      
      if(!is.null(omicsData$objRM_reduced)){
        
        file_names_models <- c(file_names_models, paste0("Reduced_model_", fs::path_sanitize(input$RDS_name)))
        
        saveRDS(omicsData$objRM_reduced, file = file_names_models[2])
        
      }
      
    })
    
    
  } else file_names_models <- NULL

  # Write report
  if (input$include_report) {
    tryCatch({
      withProgress(message = "Writing report file...", {
        params <- list(
          user_inputs = reactiveValuesToList(user_inputs), 
          omicsData = reactiveValuesToList(omicsData), 
          tables = reactiveValuesToList(table_table_current), 
          plots = reactiveValuesToList(plot_table_current), 
          titleName = "Omics ML Mentor Report")
        rmarkdown::render(paste0(orig_wd, "/www/markdowns/Report_Template.Rmd"), output_dir = getwd(), output_file = fs::path_sanitize(input$report_name), params = params, envir = new.env())
      })
    }, error = print)
  }
  
  zip(zipfile = paste0(fname, ".zip"), 
      files = c(file_names_tables, file_names_plots, file_names_models, fs::path_sanitize(input$report_name)), 
      flags = "-r")
  
  zipped_file$fs <- paste0(fname, ".zip")
})


### Download button ###
output$download_processed_data <- downloadHandler(
  filename = function() {
    paste0("SLOPE_output_", gsub("( |:|-)", "_", Sys.time()), ".zip")
  },
  content = function(fname) {
    file.copy(zipped_file$fs, fname)
  },
  contentType = "application/zip"
)
