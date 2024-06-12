## Reference tab UI

assign_ref_uploads <- function(tabname) {
  
  ### Inputs ###
  # Sidebars for NMR vs. Isobaric
  if (tabname == "Nmrdata") {
    output[["Nmrdata_reference_choice_UI"]] <- renderUI({
        return(
          radioGroupButtons("Nmrdata_reference_choice",
                            label = "Apply reference normalization to data?",
                            choices = c("Yes", "No"),
                            selected = character(0)
          )
        )
      # }
    })
    
    output[["Nmrdata_reference_source_UI"]] <- renderUI({
        
        if (!is.null(input$Nmrdata_reference_choice) && 
                 input$Nmrdata_reference_choice == "Yes") {
        return(
          radioButtons(
            "Nmrdata_reference_source",
            label = "Select reference source:",
            choices = c(
              "Row in Experimental Data (e.g. metabolite)",
              "Column in Sample Data (e.g. sample concentration)"#,
              # "Upload Reference File with column (e.g. sample concentration)"
            ),
            selected = character(0)
          )
        )
      } else {
        return(
          hidden(
            radioButtons(
              "Nmrdata_reference_source",
              label = "Select reference source:",
              choices = c(
                "Row in Experimental Data (e.g. metabolite)",
                "Column in Sample Data (e.g. sample concentration)"#,
                # "Upload Reference File with column (e.g. sample concentration)"
              ),
              selected = character(0)
            )
          )
        )
      }
    })
    
    ## Specify Values
    output[["Nmrdata_ref_id_UI"]] <- renderUI({
        
      if (is.null(input$Nmrdata_reference_choice) || input$Nmrdata_reference_choice == "No") {
        return(
          disabled(pickerInput(
            "Nmrdata_picker_reference",
            choices = c(
              "Only applicable with reference normalization",
              "placeholder"
            )
          ))
        )
      } else if (is.null(input$Nmrdata_reference_source)) {
        return(
          disabled(pickerInput(
            "Nmrdata_picker_reference",
            choices = c(
              "Please select a reference source",
              "placeholder"
            )
          ))
        )
      } else if (
        input$Nmrdata_reference_source == 
        # "Upload Reference File with column (e.g. sample concentration)" 
        "Column in Sample Data (e.g. sample concentration)" &&
                 is.null(omicsData$objQC$f_data)) {
        return(
          disabled(pickerInput(
            "Nmrdata_picker_reference",
            choices = c(
              "Please upload reference file",
              "placeholder"
            )
          ))
        )
      } else if (input$Nmrdata_reference_source == "Column in Sample Data (e.g. sample concentration)"
                 # "Upload Reference File with column (e.g. sample concentration)"
                 ) {
        df <- omicsData$objQC$f_data
        # Only colplete, numeric columns are valid
        disabled <- unlist(apply(df, 2, function(col) any(is.na(as.numeric(col)))))
        
        out <- pickerInput(
          "Nmrdata_picker_reference",
          div(
            "Select reference column:",
            div(
              style = "color:red;display:inline-block",
              add_prompt(
                icon("question-sign", lib = "glyphicon"),
                message = ttext[["REFERENCE_DISABLED_COL"]]
              )
            )
          ),
          choices = colnames(df),
          options = pickerOptions(maxOptions = 1),
          choicesOpt = list(disabled = disabled),
          selected = isolate(input$Nmrdata_picker_reference),
          multiple = TRUE
        )
        
        return(
          out
        )
      } else {
        
        # Only complete references are valid; post-transformation in case negatives exist
        rmv_col <- which(colnames(omicsData$objQC$e_data) == get_edata_cname(omicsData$objQC))
        choices <- omicsData$objQC$e_data[[rmv_col]]
        datascale <- get_data_scale(omicsData$objQC)
        transform <- ifelse(datascale == "abundance", "log2", datascale)
        plot_data <- omicsData$objQC$e_data[-rmv_col]
        
        # transform if different
        if (datascale != transform && transform != "No transformation") {
          
          # Undo log transformation if needed
          if (datascale != "abundance") {
            if (datascale == "log2") {
              plot_data$value <- 2^plot_data
            } else if (datascale == "log10") {
              plot_data$value <- 10^plot_data
            } else {
              plot_data$value <- exp(plot_data)
            }
          }
          
          ## Apply new transformation
          if (transform == "log2") {
            plot_data$value <- log2(plot_data)
          } else if (transform == "log10") {
            plot_data$value <- log10(plot_data)
          } else {
            plot_data$value <- log(plot_data)
          }
        }
        
        disabled <- apply(is.na(plot_data), 1, any)
        
        out <- pickerInput(
          "Nmrdata_picker_reference",
          div(
            "Select reference standard identifier:",
            div(
              style = "color:deepskyblue;display:inline-block",
              add_prompt(
                icon("question-sign", lib = "glyphicon"),
                message = ttext[["REFERENCE_DISABLED_ROW"]]
              )
            )
          ),
          choices = as.character(choices),
          options = pickerOptions(maxOptions = 1),
          choicesOpt = list(disabled = disabled),
          selected = isolate(input$Nmrdata_picker_reference),
          multiple = TRUE
        )
        
        return(
          out
        )
      }
    })
    
    output[["Nmrdata_ref_done_idcols_UI"]] <- renderUI({
      input[["Nmrdata_clear_fdata"]]
      text <- ifelse(is.null(omicsData$objQC$e_data_normalized),
                     "Apply reference normalization",
                     "Update reference normalization"
      )
      
      if (#input$use_example ||
          is.null(input$Nmrdata_picker_reference) ||
          input$Nmrdata_picker_reference %in% c(
            "Only applicable with reference normalization",
            "Please select a reference source",
            "Please upload reference file"
          )) {
        return(
          disabled(
            actionButton(
              "Nmrdata_ref_done_idcols",
              div(
                "Apply reference normalization",
                icon("ok-sign", lib = "glyphicon")
              )
            )
          )
        )
      } else {
        return(
          actionButton(
            "Nmrdata_ref_done_idcols",
            div(
              "Apply reference normalization",
              icon("ok-sign", lib = "glyphicon")
            )
          )
        )
      }
    })
  } else {
    ## Upload
    output[["Isobaricpepdata_fdata_upload_UI"]] <- renderUI({
      input[[paste0(tabname, "_clear_fdata")]]
      
      if (input$use_example) {
        out <- list(
          strong("Note: Specific inputs are set for Example data."),
          
          br(),
          br(),
          
          disabled(
            radioGroupButtons(
              paste0(tabname, "_ref_samps"),
              "Does intensity data contain reference samples?",
              choices = c("Yes", "No"),
              selected = "Yes"
            )
          ),
          
          uiOutput(sprintf("%s_reference_warning", tabname))
        )
      } else {
        out <- list(
          radioGroupButtons(
            paste0(tabname, "_ref_samps"),
            "Does intensity data contain reference samples?",
            choices = c("Yes", "No"),
            selected = character(0)
          ),
          
          conditionalPanel(paste0("input.", tabname, "_ref_samps == 'Yes'"), {
            div(
              id = sprintf("js_%s_file_fdata", tabname),
              br(),
              uiOutput(sprintf("%s_reference_warning", tabname))
            )
          })
        )
      }
      
      do.call(tagList, out)
    })
    output[["Isobaricpepdata_fdata_cname_UI"]] <- renderUI({
      input[[paste0(tabname, "_clear_fdata")]]
      
      if (is.null(omicsData$objQC$f_data)) {
        return(
          disabled(pickerInput(paste0(tabname, "_fdata_cname"),
                               label = "Select column designating sample identifiers",
                               choices = c("No data has been loaded", "placeholder")
          ))
        )
      } else if (input$use_example) {
        return(
          pickerInput(paste0(tabname, "_fdata_cname"),
                      label = "Select column designating sample identifiers",
                      choices = colnames(omicsData$objQC$f_data),
                      multiple = TRUE,
                      selected = "Sample",
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(omicsData$objQC$f_data) != "Sample"
                      )
          )
        )
      } else {
        return(
          pickerInput(paste0(tabname, "_fdata_cname"),
                      label = "Select column designating sample identifiers",
                      choices = colnames(omicsData$objQC$f_data),
                      selected = input[[paste0(tabname, "_fdata_cname")]],
                      multiple = TRUE,
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(omicsData$objQC$f_data) %in% c(
                          input[[paste0(tabname, "_ref_group")]],
                          input[[paste0(tabname, "_ref_col")]]
                        )
                      )
          )
        )
      }
    })
    output[["Isobaricpepdata_ref_group_UI"]] <- renderUI({
      input[[paste0(tabname, "_clear_fdata")]]

      if (is.null(omicsData$objQC$f_data)) {
        return(
          disabled(pickerInput(paste0(tabname, "_ref_group"),
                               label = "Select reference group column (one reference must be present for each group)",
                               choices = c("No data has been loaded", "placeholder")
          ))
        )
      } else if (input$use_example) {
        return(
          pickerInput(paste0(tabname, "_ref_group"),
                      label = "Select reference group column (one reference must be present for each group)",
                      choices = colnames(omicsData$objQC$f_data),
                      multiple = TRUE,
                      selected = "Plex",
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(omicsData$objQC$f_data) != "Plex"
                        # disabled = rep(TRUE, length(colnames(omicsData$objQC$f_data)))
                      )
          )
        )
      } else {
        return(
          pickerInput(paste0(tabname, "_ref_group"),
                      label = "Select reference group column (one reference must be present for each group)",
                      choices = colnames(omicsData$objQC$f_data),
                      multiple = TRUE,
                      selected = input[[paste0(tabname, "_ref_group")]],
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(omicsData$objQC$f_data) %in% c(
                          get_fdata_cname(omicsData$objQC),
                          input[[paste0(tabname, "_ref_col")]]
                        )
                      )
          )
        )
      }
    })
    output[["Isobaricpepdata_ref_col_UI"]] <- renderUI({
      input[[paste0(tabname, "_clear_fdata")]]
      
      if (is.null(omicsData$objQC$f_data)
      ) {
        return(
          disabled(pickerInput(paste0(tabname, "_ref_col"),
                               label = "Select column designating reference samples",
                               choices = c("No data has been loaded", "placeholder")
          ))
        )
      } else if (input$use_example) {
        return(
          pickerInput(paste0(tabname, "_ref_col"),
                      label = "Select column designating reference samples",
                      choices = colnames(omicsData$objQC$f_data),
                      multiple = TRUE,
                      selected = "Replicate",
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(omicsData$objQC$f_data) != "Replicate"
                        # disabled = rep(TRUE, length(colnames(omicsData$objQC$f_data)))
                      )
          )
        )
      } else {
        return(
          pickerInput(paste0(tabname, "_ref_col"),
                      label = "Select column designating reference samples",
                      choices = colnames(omicsData$objQC$f_data),
                      multiple = TRUE,
                      selected = input[[paste0(tabname, "_ref_col")]],
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = colnames(omicsData$objQC$f_data) %in% c(
                          get_fdata_cname(omicsData$objQC),
                          input[[paste0(tabname, "_ref_group")]]
                        )
                      )
          )
        )
      }
    })
    output[["Isobaricpepdata_ref_notation_UI"]] <- renderUI({
      input[[paste0(tabname, "_clear_fdata")]]
      
      if (input$use_example) {
        
        req(!is.null(input[[paste0(tabname, "_ref_col")]]))
        
        return(
          pickerInput(paste0(tabname, "_ref_notation"),
                      label = "Select reference sample notation",
                      choices = unique(
                        as.character(
                          omicsData$objQC$f_data[[input[[paste0(tabname, "_ref_col")]]]]
                        )
                      ), # Loads from pre-rendered data
                      selected = "Pool",
                      multiple = TRUE,
                      options = pickerOptions(maxOptions = 1),
                      choicesOpt = list(
                        disabled = unique(
                          as.character(
                            omicsData$objQC$f_data[[input$Isobaricpepdata_ref_col]]
                          )
                        ) != "Pool"
                      )
          )
        )
      } else if (is.null(omicsData$objQC$f_data) ||
                 is.null(input[[paste0(tabname, "_ref_col")]])
      ) {
        return(
          disabled(pickerInput(paste0(tabname, "_ref_notation"),
                               label = "Select reference sample notation",
                               choices = c("Reference column required", "placeholder")
          ))
        )
      } else {
        return(
          pickerInput(paste0(tabname, "_ref_notation"),
                      label = "Select reference sample notation",
                      choices = unique(
                        as.character(
                          omicsData$objQC$f_data[[input[[paste0(tabname, "_ref_col")]]]]
                        )
                      ), # Loads from selected column
                      multiple = TRUE,
                      selected = input[[paste0(tabname, "_ref_notation")]],
                      options = pickerOptions(maxOptions = 1)
          )
        )
      }
    })
    output[["Isobaricpepdata_ref_done_idcols_UI"]] <- renderUI({
      input[[paste0(tabname, "_clear_fdata")]]
      
      # Enable done button when all values are filled
      if (any(map_lgl(
            list(
              input[[paste0(tabname, "_ref_group")]],
              input[[paste0(tabname, "_ref_col")]],
              input[[paste0(tabname, "_ref_notation")]],
              omicsData$objQC$e_data,
              omicsData$objQC$f_data
            ),
            is.null
          )) ||
          any(map_lgl(
            list(
              input[[paste0(tabname, "_ref_group")]],
              input[[paste0(tabname, "_ref_col")]]
            ),
            function(input) input == "No data has been loaded"
          )) ||
          input[[paste0(tabname, "_ref_notation")]] == "Reference column required"
      ) {
        return(
          disabled(
            actionButton(
              paste0(tabname, "_ref_done_idcols"),
              div(
                "Apply reference normalization",
                icon("ok-sign", lib = "glyphicon")
              )
            )
          )
        )
      } else {
        return(
          actionButton(
            paste0(tabname, "_ref_done_idcols"),
            div(
              "Apply reference normalization",
              icon("ok-sign", lib = "glyphicon")
            )
          )
        )
      }
    })
  }
  
  ### Outputs ###
  
  ## Data preview
  # Fdata
  output[[paste0(tabname, "_ref_head_fdata_UI")]] <- renderUI({
    input[[paste0(tabname, "_clear_fdata")]]
    
    if (is.null(omicsData$objQC$f_data)) {
      return(textOutput(paste0(tabname, "_no_fdata")))
    } else {
      return(DTOutput(paste0(tabname, "_head_fdata")))
    }
  })
  
  output[[paste0(tabname, "_head_fdata")]] <- renderDT(
    {
      input[[paste0(tabname, "_clear_fdata")]]
      
      omicsData$objQC$f_data
    },
    options = list(dom = "tip", scrollX = TRUE),
    selection = "none"
  )
  
  output[[paste0(tabname, "_no_fdata")]] <- renderText("No Data File loaded")
  
  # Edata
  output[[paste0(tabname, "_ref_head_edata_UI")]] <- renderUI({
    input[[paste0(tabname, "_clear_fdata")]]
    
    if (is.null(omicsData$objQC$e_data)) {
      return(textOutput(paste0(tabname, "_ref_no_edata")))
    } else {
      return(DTOutput(paste0(tabname, "_ref_head_edata")))
    }
  })
  
  output[[paste0(tabname, "_ref_head_edata")]] <- renderDT(
    {
      input[[paste0(tabname, "_clear_fdata")]]
      
      omicsData$objQC$e_data
    },
    options = list(dom = "tip", scrollX = TRUE),
    selection = "none"
  )
  
  output[[paste0(tabname, "_ref_no_edata")]] <- renderText("No Data File loaded")
  
  # Norm Edata
  output[[paste0(tabname, "_norm_edata_UI")]] <- renderUI({
    input[[paste0(tabname, "_clear_fdata")]]
    
    if (is.null(omicsData$objRefnorm)) {
      return(textOutput(paste0(tabname, "_no_norm")))
    } else {
      return(DTOutput(paste0(tabname, "_norm_edata")))
    }
  })
  
  output[[paste0(tabname, "_norm_edata")]] <- renderDT(
    {
      input[[paste0(tabname, "_clear_fdata")]]
      
      omicsData$objRefnorm$e_data
    },
    options = list(dom = "tip", scrollX = TRUE),
    selection = "none"
  )
  
  output[[paste0(tabname, "_no_norm")]] <- renderText("Normalization has not been applied")
  
  ## Box plots
  
  # Prior to normalization
  output[[paste0(tabname, "_ref_upload_bp")]] <- renderUI({
    if (is.null(omicsData$objQC$e_data)) {
      out <- list(
        textOutput(paste0(tabname, "_ref_no_bp_prior")),
      )
    } else {
      out <- list(
        withSpinner(plotlyOutput(paste0(tabname, "_ref_upload_boxplots_prior")))
      )
    }
    
    do.call(tagList, out)
  })
  
  output[[paste0(tabname, "_ref_no_bp_prior")]] <- renderText("No data loaded")
  
  # Slightly chubs cause it renders differently based on reference standard selection
  output[[paste0(tabname, "_ref_upload_boxplots_prior")]] <- renderPlotly({
    
    e_data <- omicsData$obj$e_data
    e_data_cname <- get_edata_cname(omicsData$objQC)
    plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)
    
    datascale <- get_data_scale(omicsData$objQC)
    transform <- ifelse(datascale == "abundance", "log2", datascale)
    
    title <- paste0("Transformed Upload: ", transform)
    
    # transform if different
    if (datascale != transform && transform != "No transformation") {
      
      # Undo log transformation if needed
      if (datascale != "abundance") {
        if (datascale == "log2") {
          plot_data$value <- 2^plot_data$value
        } else if (datascale == "log10") {
          plot_data$value <- 10^plot_data$value
        } else {
          plot_data$value <- exp(plot_data$value)
        }
      }
      
      ## Apply new transformation
      if (transform == "log2") {
        plot_data$value <- log2(plot_data$value)
      } else if (transform == "log10") {
        plot_data$value <- log10(plot_data$value)
      } else {
        plot_data$value <- log(plot_data$value)
      }
    }
    
    p <- plot_ly(
      data = plot_data,
      x = plot_data$variable,
      y = plot_data$value,
      # colors = I("deepskyblue1"),
      type = "box",
      name = paste0(tabname, " Data")
    ) %>%
      layout(
        title = title,
        xaxis = list(title = "Samples"),
        yaxis = list(title = "Values")
      )
    
    ## Generate plots w/ references
    if (tabname == "Nmrdata" &&
        !is.null(input$Nmrdata_reference_choice) &&
        input$Nmrdata_reference_choice == "Yes" &&
        !is.null(input$Nmrdata_picker_reference) &&
        input$Nmrdata_picker_reference != "Please upload reference file"
    ) {
      if (input$Nmrdata_reference_source == "Column in Sample Data (e.g. sample concentration)"
          # "Upload Reference File with column (e.g. sample concentration)"
          ) {
        expected_cols <- colnames(omicsData$objQC$e_data)[
          !(colnames(omicsData$objQC$e_data) %in%
              get_edata_cname(omicsData$objQC))
        ]
        
        sampleid <- get_fdata_cname(omicsData$objQC)
        
        reference_vals <- omicsData$objQC$f_data[c(input$Nmrdata_picker_reference, sampleid)]
        
        # Set range of extra points to be between quantiles
        axis_1_75 <- as.numeric(quantile(plot_data$value, .75, na.rm = T))
        axis_1_25 <- as.numeric(quantile(plot_data$value, .25, na.rm = T))
        axis_1_range <- range(plot_data$value, na.rm = T)
        diff_low <- (axis_1_25 - axis_1_range[1]) / diff(axis_1_range)
        diff_high <- (axis_1_range[2] - axis_1_75) / diff(axis_1_range)
        diff_mid <- diff(c(axis_1_25, axis_1_75)) / diff(axis_1_range)
        
        axis_2_var_range <- range(reference_vals[[1]], na.rm = T)
        axis_2_range_diff <- diff(axis_2_var_range) * 1 / diff_mid # should equal diff mid in percent of total plot
        size_y2 <- c(
          axis_2_var_range[1] - diff_low * axis_2_range_diff,
          diff_high * axis_2_range_diff + axis_2_var_range[2]
        )
        
        # range_per <- (median(plot_data$value) - range(plot_data$value)[1])/(range(plot_data$value)[2] - range(plot_data$value)[1])
        #
        # size_y2 <- median(reference_vals[[1]]) + median(reference_vals[[1]])*c(range_per*-1, (1-range_per)*.95)
        #
        ay <- list(
          tickfont = list(color = "black"),
          overlaying = "y",
          side = "right",
          title = input$Nmrdata_picker_reference,
          range = size_y2
        )
        
        p <- p %>% add_markers(
          data = reference_vals,
          y = as.numeric(reference_vals[[1]]),
          x = reference_vals[[2]],
          name = input$Nmrdata_picker_reference,
          yaxis = "y2"
        ) %>%
          layout(
            yaxis2 = ay,
            legend = list(
              xanchor = "center", # use center of legend as anchor
              x = 1.25
            )
          )
        
        updateTabsetPanel(session, paste0(tabname, "_ref_out_tabset"), selected = "prior")
      } else {
        reference_vals <- plot_data[plot_data[[1]] == input$Nmrdata_picker_reference, ]
        
        p <- p %>%
          add_markers(
            data = reference_vals,
            y = as.numeric(reference_vals$value),
            x = reference_vals$variable,
            name = input$Nmrdata_picker_reference
          ) %>%
          layout(legend = list(
            xanchor = "center", # use center of legend as anchor
            x = 1.25
          ))
      }
    } else if (tabname == "Isobaricpepdata" &&
               !is.null(input$Isobaricpepdata_ref_col) &&
               !is.null(input$Isobaricpepdata_ref_group) &&
               !is.null(input$Isobaricpepdata_ref_notation)
    ) {
      plot_data <- left_join(
        plot_data, omicsData$objQC$f_data,
        c("variable" = get_fdata_cname(omicsData$objQC))
      )
      
      plot_data <- plot_data[order(plot_data[[input$Isobaricpepdata_ref_col]]), ]
      plot_data <- plot_data[order(plot_data[[input$Isobaricpepdata_ref_group]]), ]
      plot_data$variable <- factor(plot_data$variable, levels = unique(plot_data$variable))
      
      non_ref <- plot_data[[input$Isobaricpepdata_ref_col]] != input$Isobaricpepdata_ref_notation
      plot_data[[input$Isobaricpepdata_ref_col]][non_ref] <- ""
      plot_data[[input$Isobaricpepdata_ref_col]][!non_ref] <- "Reference"
      
      if(all(is.na(plot_data[[input$Isobaricpepdata_ref_col]]))){
        fillcolr <- paste(plot_data[[input$Isobaricpepdata_ref_group]])
      } else {
        fillcolr <- paste(plot_data[[input$Isobaricpepdata_ref_group]], 
                          plot_data[[input$Isobaricpepdata_ref_col]])
      }
      
      p <- plot_ly(
        data = plot_data,
        x = plot_data$variable,
        y = plot_data$value,
        fillcolor = gsub(".+ Reference$", "Reference", fillcolr),
        type = "box"
      ) %>%
        layout(
          title = title,
          xaxis = list(title = "Samples"),
          yaxis = list(title = "Values")
        )
    }
    
    isolate(plots[[tabname]][[paste0(tabname, "_Reference_upload_boxplots_prior")]] <- p)
    
    
    return(p)
  })
  
  # Reference Normalized
  output[[paste0(tabname, "_ref_norm_bp")]] <- renderUI({
    input[[paste0(tabname, "_clear_fdata")]]
    
    if (is.null(omicsData$objRefnorm)) {
      out <- list(
        textOutput(paste0(tabname, "_ref_no_bp_post"))
      )
    } else {
      out <- list(
        withSpinner(plotlyOutput(paste0(tabname, "_ref_upload_boxplots_post")))
      )
    }
    
    do.call(tagList, out)
  })
  
  output[[paste0(tabname, "_ref_no_bp_post")]] <- renderText("Normalization has not been applied")
  
  output[[paste0(tabname, "_ref_upload_boxplots_post")]] <- renderPlotly({
    input[[paste0(tabname, "_clear_fdata")]]
    
    if(get_data_scale(omicsData$objRefnorm) == "abundance"){
      e_data <- edata_transform(omicsData$objRefnorm, "log2")$e_data
    } else {
      e_data <- omicsData$objRefnorm$e_data
    }
    e_data_cname <- get_edata_cname(omicsData$objQC)
    plot_data <- melt(e_data, id = e_data_cname, na.rm = TRUE)
    datascale <- get_data_scale(omicsData$objRefnorm)
    transform <- ifelse(datascale == "abundance", "log2", datascale)
    title <- paste0("Reference Normalized Upload: ", transform)
    
    fillcolr <- omicsData$objQC$f_data[[input$Isobaricpepdata_ref_group]][
      plot_data$variable
    ]
    
    p <- plot_ly(
      data = plot_data,
      x = plot_data$variable,
      y = plot_data$value,
      # colors = I("deepskyblue1"),
      fillcolor = gsub(".+ Reference$", "Reference", fillcolr),
      type = "box"
    ) %>%
      layout(
        title = title,
        xaxis = list(title = "Samples"),
        yaxis = list(title = "Values")
      )
    
    updateTabsetPanel(session, paste0(tabname, "_ref_out_tabset"), selected = "post")
    isolate(plots[[tabname]][[paste0(tabname, "_Reference_upload_boxplots_post")]] <- p)
    
    
    return(p)
  })
}

# Load output after
observeEvent(input$check_group_cols, priority = -3, {
  
  req(input$check_group_cols > 0)
  
  tabname <- isolate(str_to_title(class(omicsData$objQC)[[1]]))
  
  output$ref_tab <- renderUI({
    upload_reference(tabname)
  })
  
  assign_ref_uploads(tabname)

})

# 
observe({
  req((!is.null(input$Nmrdata_reference_choice) && 
        input$Nmrdata_reference_choice == "No") || !is.null(omicsData$objRefnorm))
  shinyjs::show("refnorm_complete")
})

## Reference observers

### Reset isobaric fdata on click or source data change ####
observeEvent(input[[paste0(str_to_title(class(omicsData$objQC)[[1]]), "_clear_fdata")]],
             {
               tab <- paste0(str_to_title(class(omicsData$objQC)[[1]]))

               req(input[[paste0(tab, "_clear_fdata")]] >= 1)

               # Fdata values
               if (length(omicsData$objQC) > length(data_info)) {
                 omicsData$objQC[(length(data_info) + 1):(length(data_info) + length(ref_info))] <- ref_info
               }
             },
             priority = 1
)

#### Observe if reference data is present in data ####
observeEvent(input[[paste0(str_to_title(class(omicsData$objQC)[[1]]), "_ref_samps")]],
             {
               tab <- paste0(str_to_title(class(omicsData$objQC)[[1]]))
               
               req(!is.null(input[[paste0(tab, "_ref_samps")]]) &&
                     !is.na(input[[paste0(tab, "_ref_samps")]]))
               
               # Set all isobaric info to normal list if no references, include new variables where yes
               if (input[[paste0(tab, "_ref_samps")]] == "No") {
                 omicsData$objQC <- omicsData$objQC[1:length(data_info)]
                 
                 # Else include new variables
               } else if (!all(names(ref_info) %in% names(omicsData$objQC))) {
                 if (input$sourcedata == "Uploaded data") {
                   omicsData$objQC <- c(omicsData$objQC, ref_info)
                 }
               }
             },
             priority = 1
)

### Load Reference tooltips
observe({
  tab <- str_to_title(class(omicsData$objQC)[[1]])
  
  req(tab %in% c("Isobaricpepdata", "Nmrdata"))
  
  show_add_tooltip(
    session,
    paste0(tab, "_fdata_reference_icon"),
    is.null(input[[paste0(tab, "_ref_samps")]]) ||
      (is.null(omicsData$objQC$f_data) &&
         !is.null(input[[paste0(tab, "_ref_samps")]]) &&
         input[[paste0(tab, "_ref_samps")]] == "Yes"),
    "Specify reference sample presence in Data File and populate Reference Data File if applicable"
  )
  
  show_add_tooltip(
    session,
    paste0(tab, "_reference_input_icon"),
    is.null(omicsData$objRefnorm) &&
      !is.null(input[[paste0(tab, "_ref_samps")]]) &&
      input[[paste0(tab, "_ref_samps")]] == "Yes",
    "Please specify all inputs and click 'Apply reference normalization' button"
  )
})
