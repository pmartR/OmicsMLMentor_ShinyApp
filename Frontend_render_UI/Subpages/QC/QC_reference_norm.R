#'@details Apply reference normalization to the data in datatypes_pos.  Note 
#'this happens before object creation (groups tab)..
observeEvent(
  input[[paste0(str_to_title(class(omicsData$objQC)[[1]]), "_ref_done_idcols")]], {
    
    tab <- str_to_title(class(omicsData$objQC)[[1]])
    if(tab == "Isobaricpepdata"){
      req(is.null(get_isobaric_norm(omicsData$objQC)) || 
            !get_isobaric_norm(omicsData$objQC))
    } else {
      req(is.null(get_nmr_norm(omicsData$objQC)) || 
            !get_nmr_norm(omicsData$objQC))
    }
    
    tryCatch({
      
      show(paste0(tab, "_ref_busy"))
      
      on.exit({
        hide(paste0(tab, "_ref_busy"))
      })
      
      if (tab == "Isobaricpepdata") {
        
        shinyjs::show("Isobaricpepdata_ref_busy")
        on.exit(shinyjs::hide("Isobaricpepdata_ref_busy"))
        
        if (is.null(omicsData$objQC$f_data)) {
          col1 <- colnames(omicsData$objQC$e_data)
          rmv_col <- which(
            colnames(omicsData$objQC$e_data) == omicsData$objQC$e_data_cname
          )
          col1 <- col1[-rmv_col]
          
          temp_fdata <- data.frame(
            "Sample" = col1,
            "placeholder",
            check.names = F
          )
          
          applied_norm <- as.nmrData(
            e_data = omicsData$objQC$e_data,
            f_data = temp_fdata,
            e_meta = omicsData$objQC$e_meta,
            edata_cname = get_edata_cname(omicsData$objQC),
            fdata_cname = get_fdata_cname(omicsData$objQC),
            emeta_cname = get_emeta_cname(omicsData$objQC),
            data_scale = get_data_scale(omicsData$objQC),
            is_normalized = get_data_norm(omicsData$objQC),
            check.names = FALSE
          )
        } else {
          
          applied_norm <- omicsData$objQC
        }
        
        if(get_data_scale(omicsData$objQC) == "abundance"){
          applied_norm <- edata_transform(applied_norm, "log2")
        }
        
        applied_norm <- normalize_isobaric(applied_norm,
                                           exp_cname = input[[paste0(tab, "_ref_group")]],
                                           refpool_cname = input[[paste0(tab, "_ref_col")]],
                                           refpool_notation = input[[paste0(tab, "_ref_notation")]],
                                           apply_norm = TRUE
        )
        
        if(get_data_scale(omicsData$objQC) == "abundance"){
          applied_norm <- edata_transform(applied_norm, "abundance")
        }
        
        omicsData$objRefnorm <- applied_norm
        
      } else if (tab == "Nmrdata") {
        
        if (is.null(omicsData$objQC$f_data)) {
          col1 <- colnames(omicsData$objQC$e_data)
          rmv_col <- which(
            colnames(omicsData$objQC$e_data) == omicsData$objQC$e_data_cname
          )
          col1 <- col1[-rmv_col]
          
          temp_fdata <- data.frame(
            "Sample" = col1,
            "placeholder",
            check.names = F
          )
          
          applied_norm <- as.nmrData(
            e_data = omicsData$objQC$e_data,
            f_data = temp_fdata,
            e_meta = omicsData$objQC$e_meta,
            edata_cname = get_edata_cname(omicsData$objQC),
            fdata_cname = get_fdata_cname(omicsData$objQC),
            emeta_cname = get_emeta_cname(omicsData$objQC),
            data_scale = get_data_scale(omicsData$objQC),
            is_normalized = get_data_norm(omicsData$objQC),
            check.names = FALSE
          )
        } else {
          applied_norm <- omicsData$objQC
        }
        
        if (input$Nmrdata_reference_source == "Row in Experimental Data (e.g. metabolite)") {
          
          applied_norm <- normalize_nmr(applied_norm,
                                        apply_norm = TRUE,
                                        backtransform = TRUE,
                                        metabolite_name = input$Nmrdata_picker_reference
          )
        } else {
          applied_norm$f_data[[input$Nmrdata_picker_reference]] <-
            as.numeric(applied_norm$f_data[[input$Nmrdata_picker_reference]])
          
          applied_norm <- normalize_nmr(applied_norm,
                                        apply_norm = TRUE,
                                        backtransform = TRUE,
                                        sample_property_cname = input$Nmrdata_picker_reference
          )
        }
      }
      
      omicsData$objRefnorm <- applied_norm
      
      updateBoxCollapse(session = session,
                        id = "references_collapse_left",
                        close = "columnids")
      
      updateTabsetPanel(session, paste0(tab, "_ref_out_tabset"),
                        selected = "Reference Normalized"
      )
      updateTabsetPanel(session, paste0(tab, "_ref_preview_tables"),
                        selected = paste0("Reference Normalized Data File")
      )
    },
    error = function(e) {
        shinyalert(
          paste0("Something went wrong: ", e$message),
          type = "error"
        )
    })
  })

observeEvent(input$refnorm_complete, {
  req(str_to_title(class(omicsData$objQC)[[1]]) == "Isobaricpepdata" ||
        (str_to_title(class(omicsData$objQC)[[1]]) == "Nmrdata" && input$Nmrdata_reference_choice == "Yes"))
  omicsData$objQC <- auto_remove_na(omicsData$objRefnorm)
  
})
