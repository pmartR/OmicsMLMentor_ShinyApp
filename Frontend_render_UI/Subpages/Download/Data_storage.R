## Data storage module

plot_table_current <- reactiveValues(
  table = list(
    Upload__boxplot = NULL,
    #Upload__grouping = NULL, ## only applicable if a group variable is selected down the line? Whatever ones the user generates?
    
    QC__single_obs = NULL,
    QC__rmd_overall = NULL,
    QC__rmd_outliers = NULL,
    QC__missing_samples = NULL,
    QC__missing_features = NULL,
    
    PP__transform = NULL,
    # PP__filters__* added via code
    PP__normalization__pre = NULL,
    PP__normalization__post = NULL,
    PP__bias__location = NULL,
    PP__bias__scale = NULL,
    PP__SPANS = NULL,
    # RM__training_structure = NULL,
    
    RM__rec_folds = NULL,
    RM__param_optim = NULL,
    RM__training_structure = NULL,
    RM__model_eval__full__roc_curve = NULL,
    RM__model_eval__full__roc_curve = NULL,
    RM__model_eval__full__confidence_bar = NULL,
    RM__model_eval__full__prediction_bar = NULL,
    RM__model_eval__full__confusion_heatmap = NULL,
    RM__model_eval__full__confidence_scatter = NULL,
    RM__model_eval__reduced__roc_curve = NULL,
    RM__model_eval__reduced__confidence_bar = NULL,
    RM__model_eval__reduced__prediction_bar = NULL,
    RM__model_eval__reduced__confusion_heatmap = NULL,
    RM__model_eval__reduced__confidence_scatter = NULL,
    RM__variable_importance__full = NULL,
    RM__variable_importance__reduced = NULL
  ),
  
  names = list(
    Upload__boxplot = "Data boxplots",
    #Upload__grouping = NULL,
    
    QC__single_obs = "Single feature observations",
    QC__rmd_overall = "Outlier plot: All",
    QC__rmd_outliers = "Outlier plot: Selected",
    QC__missing_samples = "Missingness by sample",
    QC__missing_features = "Missingness handling thresholds",
    
    PP__transform = "Transformed boxplots",
    # PP__filters__* added via code
    PP__normalization__pre = "Normalization: Pre",
    PP__normalization__post = "Normalization: Post",
    PP__bias__location = "Normalization: bias location",
    PP__bias__scale = "Normalization: bias scale",
    PP__SPANS = "SPANS (Normalization recommendations)",
    
    RM__rec_folds = "Fold recommendation",
    RM__param_optim = "Parameter optimization",
    RM__training_structure = "Training structure",
    RM__model_eval__full__roc_curve = "Model evaluation: roc curve (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: roc curve (full)",
    RM__model_eval__full__confidence_bar = "Model evaluation: confidence bar (full)",
    RM__model_eval__full__prediction_bar = "Model evaluation: prediction bar (full)",
    RM__model_eval__full__confusion_heatmap = "Model evaluation: confusion heatmap (full)",
    RM__model_eval__full__confidence_scatter = "Model evaluation: confidence scatter (full)",
    RM__model_eval__reduced__roc_curve = "Model evaluation: roc curve (reduced)",
    RM__model_eval__reduced__confidence_bar = "Model evaluation: confidence bar (reduced)",
    RM__model_eval__reduced__prediction_bar = "Model evaluation: prediction bar (reduced)",
    RM__model_eval__reduced__confusion_heatmap = "Model evaluation: confusion heatmap (reduced)",
    RM__model_eval__reduced__confidence_scatter = "Model evaluation: confidence scatter (reduced)",
    RM__variable_importance__full = "Variable importance (full)",
    RM__variable_importance__reduced = "Variable importance (reduced)"
  ),
  
  plot_options = list()
)


table_table_current <- reactiveValues(
  table = list(
    Upload__e_data = NULL,
    Upload__e_meta = NULL,
    Upload__f_data = NULL,
    Upload__summary = NULL,
    QC__single_obs = NULL,
    QC__rmd_table = NULL,
    QC__missing_samples = NULL,
    QC__missing_features = NULL,
    MSU__expert_mentor_summary = NULL,
    PP__transform = NULL,
    
    # PP__filters__*
    
    PP__normalization = NULL,
    PP__SPANS = NULL,
    
    RM__training_structure__performance = NULL,
    RM__training_structure__tuning = NULL,
    
    RM__rec_folds = NULL,
    RM__param_optim = NULL,
    
    RM__model_eval = NULL,
    RM__model_eval__full__roc_curve = NULL,
    RM__model_eval__full__confidence_bar = NULL,
    RM__model_eval__full__prediction_bar = NULL,
    RM__model_eval__full__confusion_heatmap = NULL,
    RM__model_eval__full__confidence_scatter = NULL,
    RM__model_eval__reduced__roc_curve = NULL,
    RM__model_eval__reduced__confidence_bar = NULL,
    RM__model_eval__reduced__prediction_bar = NULL,
    RM__model_eval__reduced__confusion_heatmap = NULL,
    RM__model_eval__reduced__confidence_scatter = NULL,
    
    RM__variable_importance__full = NULL,
    RM__variable_importance__reduced = NULL
  ),

  names = list(
    Upload__e_data = "Abundance Data",
    Upload__e_meta = "Biomolecule Information",
    Upload__f_data = "Sample Information",
    Upload__summary = "Data summary",
    QC__single_obs = "N feature observations",
    QC__rmd_table = "Outlier table",
    QC__missing_samples = "Missingness by sample",
    QC__missing_features = "Missingness by feature",
    MSU__expert_mentor_summary = "Expert mentor summary",
    PP__transform = "Transformed data",
    
    # PP__filters__*
    
    PP__normalization = "Normalization",
    PP__SPANS = "SPANS (Normalization recommendations)",
    
    RM__training_structure__performance = "Training structure performance",
    RM__training_structure__tuning = "Training structure tuning",
    
    RM__rec_folds = "Fold recommendation",
    RM__param_optim = "Parameter optimization",
    
    RM__model_eval = "Model evaluation",
    RM__model_eval__full__roc_curve = "Model evaluation: roc curve (full)",
    RM__model_eval__full__confidence_bar = "Model evaluation: confidence bar (full)",
    RM__model_eval__full__prediction_bar = "Model evaluation: prediction bar (full)",
    RM__model_eval__full__confusion_heatmap = "Model evaluation: confusion heatmap (full)",
    RM__model_eval__full__confidence_scatter = "Model evaluation: confidence scatter (full)",
    RM__model_eval__reduced__roc_curve = "Model evaluation: roc curve (reduced)",
    RM__model_eval__reduced__confidence_bar = "Model evaluation: confidence bar (reduced)",
    RM__model_eval__reduced__prediction_bar = "Model evaluation: prediction bar (reduced)",
    RM__model_eval__reduced__confusion_heatmap = "Model evaluation: confusion heatmap (reduced)",
    RM__model_eval__reduced__confidence_scatter = "Model evaluation: confidence scatter (reduced)",
    
    RM__variable_importance__full = "Variable importance (full)",
    RM__variable_importance__reduced = "Variable importance (reduced)"
  )
)

download_preview <- 
  reactiveValues(
    current = NULL,
    plot = F,
    name = NULL
  )


output$preview_selected_dwn_UI <- renderUI({
  
  req(!is.null(download_preview$current))
  if(download_preview$plot){
    
    div(
      uiOutput("preview_selected_dwn_plot"),
      br(),
      uiOutput("edit_plot")
    )
  } else {
    DTOutput("preview_selected_dwn_table")
  }
  
})

output$edit_plot <- renderUI({
  req(download_preview$plot)
  isolate(plot_style <- plot_table_current$plot_options[[download_preview$name]])
  
  collapseBoxGroup(
    id = "download_plot_options_UI",
    collapseBox(
      "Plot Options",
      value = "axes_options",
      tagList(
        splitLayout(textInput(paste0("download_title"), "Title", value = plot_style$title$text, placeholder = "(default)"),
                    numericInput(paste0("download_title_fontsize"), "Title font size", value = 14),
                    sliderInput(paste0("download_title_hjust"), "Title alignment", ticks = FALSE,
                                value = 0, min = 0, max = 1, step = 0.5),
                    numericInput(paste0("download_legend_fontsize"), "Legend font size", value = 14),
                    cellWidths = rep("25%", 4)
        ),
        splitLayout(textInput(paste0("download_xlab"), "X-axis label", value = plot_style$xaxis$title$text, placeholder = "(default)"),
                    textInput(paste0("download_ylab"), "Y-axis label", value = plot_style$yaxis$title$text, placeholder = "(default)"),
                    numericInput(paste0("download_x_fontsize"), "X-axis font size", value = 11),
                    numericInput(paste0("download_y_fontsize"), "Y-axis font size", value = 11),
                    cellWidths = rep("25%", 4)
        ),
        splitLayout(numericInput(paste0("download_xangle"), "X-axis tick angle", value = NULL),
                    numericInput(paste0("download_yangle"), "Y-axis tick angle", value = NULL),
                    numericInput(paste0("download_x_ticksize"), "X-axis tick size", value = NULL),
                    numericInput(paste0("download_y_ticksize"), "Y-axis tick size", value = NULL),
                    cellWidths = rep("25%", 4)
        ),
        ### TODO: Fix the color scaling (issue #84)
        ### - ECG 5/16/2024
        # splitLayout(
        #   prettySwitch("download_use_color_scale", "Use color scaling", value = F, fill = T, status = "primary"),
        #   conditionalPanel(
        #     condition = "input.download_use_color_scale",
        #     prettySwitch("download_color_scale_palette_flip", "Flip gradient direction", value = F, fill = T, status = "primary"),
        #   ),
        #   conditionalPanel(
        #     condition = "input.download_use_color_scale",
        #     tags$b("Palette"),
        #     pickerInput("download_color_scale_palette",
        #                 choices = row.names(brewer.pal.info),
        #                 selected = "Blues")
        #   ),
        #   cellWidths = c("30%", "30%", "40%")
        # ),
        splitLayout(
          div(
            tags$b("Export format"),
            pickerInput(paste0("download_export_format"),
                        choices = c("Loading..."),
                        label = NULL, selected = "png")
          ),
          div(
            br(),
            prettySwitch("download_plot_interactive", "Interactive", value = F, fill = T, status = "primary")
          )
        )
      ),
      
      div(appButton(inputId = "download_apply_style_plot", label = "Update plot"),
          appButton(inputId = "download_reset_style_plot", label = "Reset plot"))
    )
  )
})

observeEvent(c(input$download_plot_interactive, download_preview$current), {
  req(download_preview$plot)
  req(!is.null(download_preview$current))
  req(!is.null(input$download_plot_interactive))
  
  if (inherits(download_preview$current, "plotly")) {
    isolate(updatePrettySwitch(session, "download_plot_interactive", value = T))
    updatePickerInput(session, "download_export_format", choices = c("HTML (interactive)" = "html"), selected = "html")
    shinyjs::disable("download_plot_interactive")
    shinyjs::disable("download_export_format")
    return()
  }
  
  if (input$download_plot_interactive > 0) {
    updatePickerInput(session, "download_export_format", choices = c("HTML (interactive)" = "html"), selected = "html")
    shinyjs::disable("download_export_format")
  } else {
    updatePickerInput(session, "download_export_format", choices = c("PNG" = "png", "JPEG" = "jpg", "SVG" = "svg"))
    shinyjs::enable("download_export_format")
  }
})

observeEvent(input$download_apply_style_plot, {
  req(!is.null(download_preview$current))
  req(!is.null(download_preview$name))
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  
  if (is.null(plot_style)) {
    adjustments <- list(
      xaxis = list(
        title = list(
          font = list()
        ),
        tickfont = list()
      ),
      yaxis = list(
        title = list(
          font = list()
        ),
        tickfont = list()
      ),
      title = list(
        font = list()
      )
    )
  } else {
    adjustments <- plot_style
  }
  
  if (!is.null(input$download_xlab) && input$download_xlab != "") {
    adjustments$xaxis$title$text <- input$download_xlab
  } else {
    adjustments$xaxis$title$text <- NULL
  }
  
  if (!is.null(input$download_ylab) && input$download_ylab != "") {
    adjustments$yaxis$title$text <- input$download_ylab
  } else {
    adjustments$yaxis$title$text <- NULL
  }
  
  if (!is.na(input$download_x_fontsize)) {
    adjustments$xaxis$title$font$size <- input$download_x_fontsize
  } else {
    adjustments$xaxis$title$font$size <- NULL
  }
  
  if (!is.na(input$download_y_fontsize)) {
    adjustments$yaxis$title$font$size <- input$download_y_fontsize
  } else {
    adjustments$yaxis$title$font$size <- NULL
  }
  
  if (!is.na(input$download_xangle)) {
    adjustments$xaxis$tickangle <- input$download_xangle
  } else {
    adjustments$xaxis$tickangle <- NULL
  }
  
  if (!is.na(input$download_yangle)) {
    adjustments$yaxis$tickangle <- input$download_yangle
  } else {
    adjustments$yaxis$tickangle <- NULL
  }
  
  if (!is.na(input$download_x_ticksize)) {
    adjustments$xaxis$tickfont$size <- input$download_x_ticksize
  } else {
    adjustments$xaxis$tickfont$size <- NULL
  }
  
  if (!is.na(input$download_y_ticksize)) {
    adjustments$yaxis$tickfont$size <- input$download_y_ticksize
  } else {
    adjustments$yaxis$tickfont$size <- NULL
  }
  
  if (input$download_title != "") {
    adjustments$title$text <- input$download_title
  } else {
    adjustments$title$text <- NULL
  }
  
  if (!is.na(input$download_title_fontsize)) {
    adjustments$title$font$size <- input$download_title_fontsize
  } else {
    adjustments$title$font$size <- NULL
  }
  
  if (!is.na(input$download_title_hjust)) {
    adjustments$title$x <- input$download_title_hjust
  } else {
    adjustments$title$x <- NULL
  }
  
  if (!is.na(input$download_legend_fontsize)) {
    adjustments$legend$font$size <- input$download_legend_fontsize
  } else {
    adjustments$legend$font$size <- NULL
  }
  
  if (!is.null(input$download_color_scale_palette) && input$download_use_color_scale) {
    adjustments$colorway <- brewer.pal(
      nrow(layer_data(download_preview$current, 1)),
      input$download_color_scale_palette
    )
    if (input$download_color_scale_palette_flip) {
      adjustments$colorway <- rev(adjustments$colorway)
    }
  } else {
    adjustments$colorway <- NULL
  }
  
  adjustments$meta <- c(input$download_export_format)
  
  plot_table_current$plot_options[[download_preview$name]] <- adjustments
})

observeEvent(input$download_reset_style_plot, {
  req(input$download_reset_style_plot > 0)
  req(download_preview$plot)
  plot_table_current$plot_options[[download_preview$name]] <- NULL
})

output$preview_selected_dwn_plot <- renderUI({
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  tagList(
    if (inherits(download_preview$current, "plotly") || 
        (!is.null(plot_style) && plot_style$meta[1] == "html")) {
      plotlyOutput("preview_selected_dwn_plot_plotly")
    } else {
      plotOutput("preview_selected_dwn_plot_ggplot")
    },
    if (inherits(download_preview$current, "plotly")) {
      tagList("Export format: ", tags$b("HTML"))
    } else if (!is.null(plot_style)) {
      tagList("Export format: ", tags$b(str_to_upper(plot_style$meta[1])))
    } else {
      tagList("Export format: ", tags$b("PNG"))
    }
  )
})

apply_plot_style <- function(p, plot_style) {
  if (is.null(plot_style))
    return(p)
  
  if (inherits(p, "plotly")) {
    args <- plot_style
    args$p <- p
    return(do.call(plotly::layout, args))
  }
  
  if (is.null(plot_style$colorway)) {
    scale <- theme()
  } else if (inherits(layer_scales(p), "ScaleContinuous")) {
    scale <- scale_fill_gradientn(colors = plot_style$colorway)
  } else {
    scale <- scale_fill_manual(values = plot_style$colorway)
  }
  
  title <- if (is.null(plot_style$title$text))
              ggplot2::waiver() 
            else
              plot_style$title$text
  xtitle <- if (is.null(plot_style$xaxis$title$text))
              ggplot2::waiver() 
            else
              plot_style$xaxis$title$text
  ytitle <- if (is.null(plot_style$yaxis$title$text))
              ggplot2::waiver() 
            else
              plot_style$yaxis$title$text
  
  p + 
    ggtitle(title) +
    xlab(xtitle) +
    ylab(ytitle) +
    scale +
    theme(
      plot.title = element_text(
        size = plot_style$title$font$size,
        hjust = plot_style$title$x
      ),
      axis.title.x = element_text(
        size = plot_style$xaxis$title$font$size
      ),
      axis.title.y = element_text(
        size = plot_style$yaxis$title$font$size
      ),
      axis.text.x = element_text(
        angle = plot_style$xaxis$tickangle,
        size = plot_style$xaxis$tickfont$size
      ),
      axis.text.y = element_text(
        angle = plot_style$yaxis$tickangle,
        size = plot_style$yaxis$tickfont$size
      ),
      legend.text = element_text(
        size = plot_style$legend$font$size
      ),
      legend.title = element_text(
        size = plot_style$legend$font$size
      )
    )
}

output$preview_selected_dwn_plot_ggplot <- renderPlot({
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  
  p <- apply_plot_style(download_preview$current, plot_style)
  
  p
})
output$preview_selected_dwn_plot_plotly <- renderPlotly({
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  
  if (inherits(download_preview$current, "plotly")) {
    warning("DEVELOPER: Please ensure this plot is stored as a ggplot object if possible! Plots can be made interactive with ggplotly() when being displayed, but should be stored as a ggplot.")
    return(apply_plot_style(download_preview$current, plot_style))
  }
  
  p <- apply_plot_style(download_preview$current, plot_style)
  
  ggplotly(p)
})
output$preview_selected_dwn_table <- renderDT(height = "450px",{
  req(!download_preview$plot)
  download_preview$current
}, 
selection = "none",
options = list(dom = "tp", 
               scrollY = "300px"))

## Load up previews

flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) {
    'list' %in% class(xprime) & !('gg' %in% class(xprime))
  })
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

map(c("Upload", "QC", "MSU", "PP", "RM"), function(pg){
  map(c("table", "plot"), function(type){
    
    str <- paste0("download_", type, "_table_", pg, "_cell_clicked")
    
    observeEvent(input[[str]], {
      
      req(length(input[[str]]) > 0)
      
      pull_info <- get(paste0(type, "_table_current"))
      req(!is.null(pull_info))
      index <- input[[str]]$row
      
      pull_table <- tibble(
        rows = pull_info$names[which(startsWith(names(pull_info$table), paste0(pg, "__")))],
        list_el = pull_info$table[which(startsWith(names(pull_info$table), paste0(pg, "__")))] 
      ) %>% filter(!map_lgl(list_el, is.null))
      
      download_preview$current <- pull_table$list_el[[index]]
      download_preview$name <- names(pull_table$list_el)[index]
      download_preview$plot <- type == "plot"
      
    })
    
  })
})


## Table fillings

output$download_plot_table_Upload <- renderDT(height = "450px",{
  
    session$sendCustomMessage("unbind-DT-RR", "download_plot_table_Upload")
    pg <- "Upload"
    plot_table <- tibble(
      rows = sapply(names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))], \(x) plot_table_current$names[[x]]),
      list_el = plot_table_current$table[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Visualization = unname(unlist(plot_table$rows)),
      `Download?` = shinyInput(safeCheckboxInput, length(unlist(plot_table$rows)), value = T, "checkplot_upload", width = "10%"),
      check.names = F
    )
    
  }, options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$download_table_table_Upload <- renderDT(height = "450px",{
  
    session$sendCustomMessage("unbind-DT-RR", "download_table_table_Upload")
    pg <- "Upload"
    table_table <- tibble(
      rows = sapply(names(table_table_current$table)[which(startsWith(names(table_table_current$table), paste0(pg, "__")))], \(x) table_table_current$names[[x]]),
      list_el = table_table_current$table[which(startsWith(names(table_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Table = unname(unlist(table_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(table_table$rows)), value = T, "checktable_upload", width = "10%"),
      check.names = F
    )
  }, 
  options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)


output$download_plot_table_QC <- renderDT(height = "450px",{
  
    session$sendCustomMessage("unbind-DT-RR", "download_plot_table_QC")
    pg <- "QC"
    plot_table <- tibble(
      rows = sapply(names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))], \(x) plot_table_current$names[[x]]),
      list_el = plot_table_current$table[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Visualization = unname(unlist(plot_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(plot_table$rows)), value = T, "checkplot_QC", width = "10%"),
      check.names = F
    )
    
  }, options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$download_table_table_QC <- renderDT(height = "450px",{
  
  
    session$sendCustomMessage("unbind-DT-RR", "download_table_table_QC")
    pg <- "QC"
    table_table <- tibble(
      rows = sapply(names(table_table_current$table)[which(startsWith(names(table_table_current$table), paste0(pg, "__")))], \(x) table_table_current$names[[x]]),
      list_el = table_table_current$table[which(startsWith(names(table_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Table = unname(unlist(table_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(table_table$rows)), value = T, "checktable_QC", width = "10%"),
      check.names = F
    )
  }, 
  options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$download_plot_table_MSU <- renderDT(height = "450px",{
  
    session$sendCustomMessage("unbind-DT-RR", "download_plot_table_MSU")
    pg <- "MSU"
    plot_table <- tibble(
      rows = sapply(names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))], \(x) plot_table_current$names[[x]]),
      list_el = plot_table_current$table[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Visualization = unname(unlist(plot_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(plot_table$rows)), value = T, "checktable_QC", width = "10%"),
      check.names = F
    )
    
  }, options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "none",
  escape = FALSE
)

output$download_table_table_MSU <- renderDT(height = "450px",{
  
  
    session$sendCustomMessage("unbind-DT-RR", "download_table_table_MSU")
    pg <- "MSU"
    table_table <- tibble(
      rows = sapply(names(table_table_current$table)[which(startsWith(names(table_table_current$table), paste0(pg, "__")))], \(x) table_table_current$names[[x]]),
      list_el = table_table_current$table[which(startsWith(names(table_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Table = unname(unlist(table_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(table_table$rows)), value = T, "checktable_MSU", width = "10%"),
      check.names = F
    )
  }, 
  options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$download_plot_table_PP <- renderDT(height = "450px",{
  
    session$sendCustomMessage("unbind-DT-RR", "download_plot_table_PP")
    pg <- "PP"
    plot_table <- tibble(
      rows = sapply(names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))], \(x) plot_table_current$names[[x]]),
      list_el = plot_table_current$table[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Visualization = unname(unlist(plot_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(plot_table$rows)), value = T, 
                               "checkplot_PP", width = "10%"),
      check.names = F
    )
    
  }, options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$download_table_table_PP <- renderDT(height = "450px",{
  
    session$sendCustomMessage("unbind-DT-RR", "download_table_table_PP")
    pg <- "PP"
    table_table <- tibble(
      rows = sapply(names(table_table_current$table)[which(startsWith(names(table_table_current$table), paste0(pg, "__")))], \(x) table_table_current$names[[x]]),
      list_el = table_table_current$table[which(startsWith(names(table_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Table = unname(unlist(table_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(table_table$rows)), value = T,
                               "checktable_PP", width = "10%"),
      check.names = F
    )
  }, 
  options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$download_plot_table_RM <- renderDT(height = "450px",{
  
    session$sendCustomMessage("unbind-DT-RR", "download_plot_table_RM")
    pg <- "RM"
    plot_table <- tibble(
      rows = sapply(names(plot_table_current$table)[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))], \(x) plot_table_current$names[[x]]),
      list_el = plot_table_current$table[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Visualization = unname(unlist(plot_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(plot_table$rows)), value = T, "checkplot_RM", width = "10%"),
      check.names = F
    )
    
  }, options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$download_table_table_RM <- renderDT(height = "450px",{
    
    session$sendCustomMessage("unbind-DT-RR", "download_table_table_RM")
    pg <- "RM"
    table_table <- tibble(
      rows = sapply(names(table_table_current$table)[which(startsWith(names(table_table_current$table), paste0(pg, "__")))], \(x) table_table_current$names[[x]]),
      list_el = table_table_current$table[which(startsWith(names(table_table_current$table), paste0(pg, "__")))] 
    ) %>% filter(!map_lgl(list_el, is.null))
    
    data.frame(
      Table = unname(unlist(table_table$rows)),
      `Download?` = shinyInput(checkboxInput, length(unlist(table_table$rows)), value = T, "checktable_RM", width = "10%"),
      check.names = F
    )
  }, 
  options = list(
    preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
    drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } "),
    dom = "t", 
    scrollY = "300px"
  ),
  selection = "single",
  escape = FALSE
)

output$include_model_UI <- renderUI({
  req(!is.null(omicsData$objPP))
  
  if(!is.null(attr(omicsData$objPP,"data_info")) && 
     attr(omicsData$objPP,"data_info")$norm_info$norm_fn != "zero_to_one"){
    include_mod <- disabled(checkboxInput(label = "Include model as an R object (RDS file)?", 
                  "include_model", value = F))
  } else {
    include_mod <- checkboxInput(label = "Include model as an R object (RDS file)?", 
                  "include_model", value = T)
  }
  
  include_mod
})




# getShinyInput(input, "checkplot_upload")

