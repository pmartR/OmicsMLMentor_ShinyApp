
observeEvent(input$check_group_cols, {
  
  req(!is.null(input$data_type))
  
  edata <- reactive_dataholder[["e_data"]]$file
  fdata <- reactive_dataholder[["f_data"]]$file
  emeta <- reactive_dataholder[["e_meta"]]$file
  
  edata_cname <- input$e_data_id_col
  fdata_cname <- input$f_data_id_col
  emeta_cname <- input$e_meta_id_col
  
  if(is.null(fdata) || input$use_fdata == "No"){
    fdata <- data.frame(SampleID = colnames(edata)[colnames(edata) != edata_cname],
                        Col1 = colnames(edata)[colnames(edata) != edata_cname])
    fdata_cname <- "SampleID"
  }
  
  data_scale <- input$datascale
  is_normalized <- input$normalized == "Yes"
  na_replace <- input$na_symbol
  
  ## this will need to switch to with naming convention updates
  fn_use <- switch(
    input$data_type,
    "Positive" = "as.lipidData", 
    "Negative" = "as.lipidData", 
    "Label-free" = "as.pepData",
    "Isobaric" = "as.isobaricpepData", 
    "Protein" = "as.proData", 
    "ProteinTMT" = "as.proData", 
    "GC-MS" = "as.metabData", 
    "NMR" = "as.nmrData",
    "RNA-seq" = "as.seqData"
    )
  
  object_fn <- get(fn_use)
  
  if(input$data_type == "RNA-seq"){
    data_scale_hold <- data_scale
    data_scale <- "counts"
  }
  
  # create first object
  od <- tryCatch(
    {
      res <- object_fn(
        e_data = edata, e_meta = emeta, f_data = fdata,
        edata_cname = edata_cname, emeta_cname = emeta_cname, fdata_cname = fdata_cname,
        data_scale = data_scale, is_normalized = is_normalized
      )
      
      if(input$data_type != "RNA-seq"){
        res <- res %>% edata_replace(na_replace, NA)
      } else {
        attr(res, "data_info")$data_scale_actual <- data_scale_hold
      }
      res
    },
    error = function(e) {
      msg <<- paste0("Something went wrong processing your omicsData object \n System error:  ", e)
      # revals$warnings_upload$badupload <- sprintf("<p style = color:red>%s</p>", msg)
      NULL
    }
  )
  
  if (is.null(od)) {
    shinyalert("Something went wrong:", msg)
    return(NULL)
  }
  
  if(is.null(reactive_dataholder[["f_data"]]$file) || input$use_fdata == "No"){
    od$f_data <- NULL
  }

  isolate({
    table_table_current$table$Upload__e_data <- od$e_data
    if(!is.null(od$f_data)) table_table_current$table$Upload__f_data <- od$f_data
    if(!is.null(od$e_meta)) table_table_current$table$Upload__e_meta <- od$e_meta
    table_table_current$table$Upload__summary <- summary(od)
  })
  
  omicsData$obj <-  od
  
})
