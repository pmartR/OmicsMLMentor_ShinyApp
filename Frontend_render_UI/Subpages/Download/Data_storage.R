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
    RM__model_eval__full__roc_curve = "Model evaluation: confidence bar (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: prediction bar (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: confusion heatmap (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: confidence scatter (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: roc curve (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: confidence bar (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: prediction bar (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: confusion heatmap (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: confidence scatter (reduced)",
    RM__variable_importance__full = "Variable importance (full)",
    RM__variable_importance__reduced = "Variable importance (reduced)"
  )
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
    RM__model_eval__full__roc_curve = "Model evaluation: roc curve (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: confidence bar (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: prediction bar (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: confusion heatmap (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: confidence scatter (full)",
    RM__model_eval__full__roc_curve = "Model evaluation: roc curve (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: confidence bar (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: prediction bar (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: confusion heatmap (reduced)",
    RM__model_eval__full__roc_curve = "Model evaluation: confidence scatter (reduced)",
    
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
    RM__model_eval__full__roc_curve = "NULL",
    RM__model_eval__full__confidence_bar = "NULL",
    RM__model_eval__full__prediction_bar = "NULL",
    RM__model_eval__full__confusion_heatmap = "NULL",
    RM__model_eval__full__confidence_scatter = "NULL",
    RM__model_eval__reduced__roc_curve = "NULL",
    RM__model_eval__reduced__confidence_bar = "NULL",
    RM__model_eval__reduced__prediction_bar = "NULL",
    RM__model_eval__reduced__confusion_heatmap = "NULL",
    RM__model_eval__reduced__confidence_scatter = "NULL",
    
    RM__variable_importance__full = "Variable importance (full)",
    RM__variable_importance__reduced = "Variable importance (reduced)"
  )
)

download_preview <- 
  reactiveValues(
    current = NULL,
    plot = F
    )


output$preview_selected_dwn_UI <- renderUI({
  
  req(!is.null(download_preview$current))
  if(download_preview$plot){
    
    div(
      plotlyOutput("preview_selected_dwn_plot"),
      br(),
      actionButton("edit_plot", "Edit plot")
    )
  } else {
    DTOutput("preview_selected_dwn_table")
  }
  
})

output$preview_selected_dwn_plot <- renderPlotly({
  req(download_preview$plot)
  download_preview$current
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
      download_preview$plot <- type == "plot"
      
    })
    
  })
})


## Table fillings

output$download_plot_table_Upload <- renderDT(height = "450px",{
  
  session$sendCustomMessage("unbind-DT-RR", "download_plot_table_Upload")
  pg <- "Upload"
  plot_table <- tibble(
    rows = plot_table_current$names[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))],
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
    rows = table_table_current$names[which(startsWith(names(table_table_current$table), paste0(pg, "__")))],
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
    rows = plot_table_current$names[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))],
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
    rows = table_table_current$names[which(startsWith(names(table_table_current$table), paste0(pg, "__")))],
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
    rows = plot_table_current$names[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))],
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
    rows = table_table_current$names[which(startsWith(names(table_table_current$table), paste0(pg, "__")))],
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
    rows = plot_table_current$names[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))],
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
    rows = table_table_current$names[which(startsWith(names(table_table_current$table), paste0(pg, "__")))],
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
    rows = plot_table_current$names[which(startsWith(names(plot_table_current$table), paste0(pg, "__")))],
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
    rows = table_table_current$names[which(startsWith(names(table_table_current$table), paste0(pg, "__")))],
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





# getShinyInput(input, "checkplot_upload")

