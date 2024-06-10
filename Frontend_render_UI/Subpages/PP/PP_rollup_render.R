
## This needs to be modified for the overlord tab -- must skip this section


output$rollup_tab <- renderUI({
  isolate(nm <- get_omicsData_type(omicsData$objPP))
  out <- rollup_tab(nm)
  # if(!(nm %in% c("Pepdata", "Isobaricpepdata"))){
  #   out <- div()
  # }
  load_rollup_observers(nm)
  assign_rollup_output(nm)
  out
})

load_rollup_observers <- function(tab) {
  # rollup
  observeEvent(input[[paste0(tab, "_apply_rollup")]], {
    
    pep <- omicsData$objPP
    
    req(inherits(pep, "pepData"))
    
    updateBoxCollapse(session, paste0(tab, "_rollup_main"), open = "rollup_res")
    
    tryCatch(
      {
        shinyjs::show(paste0(tab, "_rollup_busy"))
        
        on.exit({
          shinyjs::hide(paste0(tab, "_rollup_busy"))
        })
        
        if (input$qc_which_rollup == "zrollup") {
          single_pep <- TRUE
          single_observation <- TRUE
        } else {
          single_pep <- FALSE
          single_observation <- FALSE
        }
        
        cname <- get_edata_cname(pep)
        pep$e_data[[cname]] <- as.character(pep$e_data[[cname]]) #### Weird thing with numerics?
        pep$e_meta[[cname]] <- as.character(pep$e_meta[[cname]])
        
        omicsData$objPP <- protein_quant(pep,
                                                  method = input$qc_which_rollup,
                                                  qrollup_thresh = input$qc_qrollup_thresh / 100,
                                                  single_pep = single_pep,
                                                  single_observation = single_observation,
                                                  combine_fn = input$qc_which_combine_fn,
                                                  parallel = TRUE
        )
        
        # Remove proteins which got filtered out
        transforms_df <- pepQCData$transforms_df[
          -which(!pepQCData$transforms_df[[get_edata_cname(omicsData$objPP)]] %in% omicsData$objPP$e_data[[get_edata_cname(omicsData$objPP)]]),
        ]
        
        if (dim(transforms_df)[1] == 0) {
          transforms_df <- pepQCData$transforms_df
        }
        
        omicsData$objPP <- edata_nathresh_transform(as.slData(omicsData$objPP), transforms_df)
        
        
        
        updateTabsetPanel(session, paste0(tab, "_rollup_res_tabpanel"),
                          selected = "Roll-up Visualization"
        )
        
        shinyjs::show("complete_rollup")
        
        # covariates
      },
      error = function(e) {
        print(e)
      }
    )
  })
}
  
assign_rollup_output <- function(tab) {
  output[[paste0(tab, "_which_rollup")]] <- renderText({
    switch(
      input$qc_which_rollup, 
      rollup = "Centering only",
      rrollup = "Reference",
      qrollup = paste0("Quantile: ", input$qc_qrollup_thresh),
      zrollup = "Z-Score"
    )
  })
  
  output[[paste0(tab, "_which_combine_fn")]] <- renderText({
    input$qc_which_combine_fn
  })
  
  output[[paste0(tab, "_rollup_res_UI")]] <- renderUI({
    if (inherits(omicsData$objPP, "proData")) {
      p <- plotlyOutput(paste0(tab, "_rollup_res"))
    } else {
      p <- "Roll-up has not been applied."
    }
    return(p)
  })
  
  output[[paste0(tab, "_rollup_res")]] <- renderPlotly({
    req(!inherits(omicsData$objPP, "pepData"))
    
    e_data <- omicsData$objPP$e_data
    e_data_cname <- pmartR::get_edata_cname(omicsData$objPP)
    plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)
    group_df <- get_group_DF(omicsData$objPP)
    plot_data <- left_join(plot_data, group_df, by = c("variable" = colnames(group_df)[1]))
    title <- paste0("Protein Roll-up: ", tab, " Data")
    
    plot_data <- arrange(plot_data, !!rlang::sym(colnames(group_df)[2]))
    plot_data$variable <- factor(plot_data$variable, levels = unique(plot_data$variable))
    
    p <- plot_ly(
      data = plot_data,
      x = plot_data$variable,
      y = plot_data$value,
      color = plot_data[[colnames(group_df)[2]]],
      type = "box"
    ) %>%
      layout(
        title = title,
        xaxis = list(title = "Samples"),
        yaxis = list(title = "Values")
      )
    
    isolate(plot_table_current$PP$rollup <- p)
    
    p
  })
  
  output[[paste0(tab, "_rollup_data_summary_UI")]] <- renderUI({
    if (inherits(omicsData$objPP, "proData")) {
      p <- DTOutput(paste0(tab, "_rollup_data_summary"))
    } else {
      p <- "Roll-up has not been applied."
    }
    
    return(p)
  })
  
  output[[paste0(tab, "_rollup_data_summary")]] <- renderDT(
    {
      df <- as.data.frame(summary(omicsData$objPP))
      df <- df[2:nrow(df), ]
      df[[1]] <- as.numeric(as.character(df[[1]]))
      
      if (is.na(df[str_detect(row.names(df), "e_meta"), ])) {
        df <- df[-3, ]
        
        row.names(df) <- c(
          "Unique Samples",
          paste0("Unique ", get_edata_cname(omicsData$objQC)),
          # paste0("Unique ", datatypes_pos$dataholder[[type]]$Emeta_cname),
          "Missing Observations",
          "Proportion Missing",
          row.names(df)[5:nrow(df)]
        )
        
        colnames(df) <- " "
      } else {
        row.names(df) <- c(
          "Unique Samples",
          paste0("Unique ", get_edata_cname(omicsData$objQC)),
          paste0("Unique ", get_emeta_cname(omicsData$objPP)),
          "Missing Observations",
          "Proportion Missing",
          row.names(df)[6:nrow(df)]
        )
        
        colnames(df) <- " "
      }
      
      isolate(table_table_current$PP$rollup <- df)
      
      return(df)
    },
    options = list(dom = "t", scrollX = TRUE),
    selection = "none"
  )
}
