
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
        
        norm_attr <- attr(pep,"data_info")$norm_info$norm_fn 
        
        if(is.null(pep$f_data)){
          pep$f_data <- data.frame(
            SampleID = colnames(pep$e_data)[
              colnames(pep$e_data) != pmartR::get_edata_cname(pep)],
            Temp_col_all = "All"
          )
        }
        
        unregister()
        omicsData$objPP <- protein_quant(pep,
                                                  method = input$qc_which_rollup,
                                                  qrollup_thresh = input$qc_qrollup_thresh / 100,
                                                  single_pep = single_pep,
                                                  single_observation = single_observation,
                                                  combine_fn = input$qc_which_combine_fn,
                                                  parallel = TRUE
        )
        
        unregister()
        attr(omicsData$objPP,"data_info")$norm_info$norm_fn <- norm_attr
        
        if (input$keep_missing != "Yes") {
          
          # thresholds <- pep ### see if we can derive from the object itself in the future?
          thresholds <- filter_settings[[get_omicsData_type(pep)]]$imputefilt
          
          set.seed(2025)
          
          ## No further imputation occurs fyi since all imputed peps are complete at the pro level
          omicsData$objPP <- imputation_function(omicsData$objPP, thresholds)
          
          # # Remove proteins which got filtered out
          # transforms_df <- pepQCData$transforms_df[
          #   -which(!pepQCData$transforms_df[[get_edata_cname(omicsData$objPP)]] %in% omicsData$objPP$e_data[[get_edata_cname(omicsData$objPP)]]),
          # ]
          # 
          # if (dim(transforms_df)[1] == 0) {
          #   transforms_df <- pepQCData$transforms_df
          # }
          # 
          # omicsData$objPP <- edata_nathresh_transform(as.slData(omicsData$objPP), transforms_df)
          # 
          # # Remove all remaining proteins with missingness.
          # # These occur when a protein is only made up of peptides that are 100% missing.
          # if (any(is.na(omicsData$objPP$e_data))) {
          #   transforms_df <- slopeR::get_transform_df(omicsData$objPP, list())
          #   transforms_df$Handling[which(transforms_df$`Percentage missing` > 0)] <- "Remove"
          #   omicsData$objPP <- edata_nathresh_transform(as.slData(omicsData$objPP), transforms_df)
          # }
        }
        
        if(is.null(pep$f_data)){
          omicsData$objPP$f_data <- NULL
        }
        
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
    str_to_title(input$qc_which_combine_fn)
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
    
    drop <- which(colnames(omicsData$objPP$e_data) == get_edata_cname(omicsData$objPP)) 
    
    if(all(unlist(omicsData$objPP$e_data[-drop]) %in% c(0, 1))) {

      ## Still need group designation for color, order
      df <- as.data.frame(table(melt(omicsData$objPP$e_data)[2:3]))
      p <- ggplot(data = df, aes(x = variable, fill = value, y = Freq)) + 
        geom_col() + theme_bw() + 
        labs(
          title = "Protein-summarized data",
          fill = "Conversion",
          y = "Number of biomolecules",
          x = "Sample",
        ) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      
    } else {
      if(!is.null(get_group_DF(omicsData$objPP))){
        p <- plot(omicsData$objPP, 
                  color_by = response_cols_ag(), 
                  order_by = response_cols_ag())
      } else {
        p <- plot(omicsData$objPP)
      }
    }
    
    isolate(plot_table_current$table$PP__rollup <- p)
    isolate(plot_table_current$names$PP__rollup <- "Protein roll-up")
    
    ggplotly(p)

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
