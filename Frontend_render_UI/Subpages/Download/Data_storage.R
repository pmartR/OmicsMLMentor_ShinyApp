## Data storage module

plot_table_current <- reactiveValues(
  Upload = list(
    boxplot = NULL,
    grouping = NULL ## only applicable if a group variable is selected down the line? Whatever ones the user generates?
  ),
  QC = list(
    single_obs = NULL,
    rmd_overall = NULL,
    rmd_outliers = NULL,
    missing_samples = NULL,
    missing_features = NULL
  ),
  MSU = NULL,
  PP = list(
    transform = NULL,
    filters = NULL,
    normalization = NULL,
    bias = NULL,
    SPANS = NULL
  ),
  RM = list(
    # training_structure = NULL,
    rec_folds = NULL,
    param_optim = NULL,
    training_structure = NULL,
    model_eval = NULL,
    variable_importance = NULL
  )
)


table_table_current <- reactiveValues(
  Upload = list(
    e_data = NULL,
    e_meta = NULL,
    f_data = NULL,
    summary = NULL
  ),
  QC = list(
    single_obs = NULL,
    rmd_table = NULL,
    missing_samples = NULL,
    missing_features = NULL
  ),
  MSU = list(
    expert_mentor_summary = NULL
  ),
  PP = list(
    transform = NULL,
    filters = NULL,
    normalization = NULL,
    SPANS = NULL
  ),
  RM = list(
    training_structure = NULL,
    rec_folds = NULL,
    param_optim = NULL,
    model_eval = NULL,
    variable_importance = NULL
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
        
        pull_info <- get(paste0(type, "_table_current"))[[pg]]
        req(!is.null(pull_info))
        index <- input[[str]]$row
        flat_info <- flattenlist(pull_info)
        download_preview$current <- flat_info[!map_lgl(flat_info, is.null)][[index]]
        download_preview$plot <- type == "plot"
      
    })
    
  })
})


## Table fillings

output$download_plot_table_Upload <- renderDT(height = "450px",{
  
  session$sendCustomMessage("unbind-DT-RR", "download_plot_table_Upload")
  list_el <- plot_table_current$Upload
  
  if(!is.null(list_el$grouping)){
    rows <- c("Data boxplots",
              paste0("Group info: ", names(list_el$grouping)))
  } else {
    rows <- c("Data boxplots")
  }

  data.frame(
    Visualization = rows,
    `Download?` = shinyInput(checkboxInput, length(rows), value = T, "checkplot_upload", width = "10%"),
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
  list_el <- table_table_current$Upload
  
  rows <- c("Abundance data",
            "Sample Information",
            "Feature data",
            "Data summary"
  )
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(list_el, is.null)]
  
  data.frame(
    Table = rows,
    `Download?` = shinyInput(checkboxInput, length(rows), value = T, "checktable_upload", width = "10%"),
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
  list_el <- plot_table_current$QC
  
  rows <- c(
    "Single feature observations",
    "Outlier plot: All",
    "Outlier plot: Selected",
    "Missingness by sample",
    "Missingness handling thresholds"
  )
  
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(list_el, is.null)]
  
  data.frame(
    Visualization = rows,
    `Download?` = shinyInput(checkboxInput, length(rows),value = T, "checkplot_QC", width = "10%"),
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
  list_el <- table_table_current$QC
  
  rows <- c(
    "N feature observations",
    "Outlier table",
    "Missingness by sample",
    "Missingness by feature"
  )
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(list_el, is.null)]
  
  data.frame(
    Table = rows,
    `Download?` = shinyInput(checkboxInput, length(rows),value = T, "checktable_QC", width = "10%"),
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
  list_el <- plot_table_current$MSU
  
  data.frame(
    Visualization = character(0),
    `Download?` = character(0),
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
  list_el <- table_table_current$MSU
  
  rows <- c(
    "Expert mentor summary"
  )
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(list_el, is.null)]
  
  data.frame(
    Table = rows,
    `Download?` = shinyInput(checkboxInput, length(rows),value = T, "checktable_MSU", width = "10%"),
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
  list_el <- plot_table_current$PP
  
  rows <- c("Transformed boxplots",
            "SPANS (Normalization recommendations)",
            paste0("Filter info: ", names(list_el$filters)),
            "Normalization: Pre",
            "Normalization: Post",
            paste0("Normalization bias: ", names(list_el$bias))
            )
  
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(flattenlist(list_el), is.null)]
  
  data.frame(
    Visualization = rows,
    `Download?` = shinyInput(checkboxInput, length(rows),value = T, 
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
  list_el <- table_table_current$PP
  
  rows <- c("Transformed data",
            "SPANS (Normalization recommendations)",
            "Normalization",
            paste0("Filter info: ", names(list_el$filters))
  )
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(flattenlist(list_el), is.null)]
  
  data.frame(
    Table = rows,
    `Download?` = shinyInput(checkboxInput, length(rows), value = T,
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
  list_el <- plot_table_current$RM
  
  req(!is.null(list_el))
  
  model_names <- gsub("\\.", " ", names(flattenlist(list_el$model_eval)))
  
  rows <- c(
    "Fold recommendation",
    "Parameter optimization",
    "Training structure",
    paste0("Variable importance: ", names(list_el$variable_importance)),
    paste0("Model evaluation: ", model_names)
  )
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(flattenlist(list_el), is.null)]
  
  data.frame(
    Visualization = rows,
    `Download?` = shinyInput(checkboxInput, length(rows),value = T, "checkplot_RM", width = "10%"),
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
  list_el <- table_table_current$RM
  
  req(!is.null(list_el))
  
  model_names <- gsub("\\.", " ", names(flattenlist(list_el$model_eval)))
  train_names <- names(list_el$training_structure)
  
  rows <- c(
    "Fold recommendation",
    "Parameter optimization",
    ifelse(length(train_names) < 2,"Training structure",
           paste0("Training structure: ", names(list_el$training_structure))),
    paste0("Variable importance: ", names(list_el$variable_importance)),
    paste0("Model evaluation: ", model_names)
  )
  
  req(any(!map_lgl(list_el, is.null)))
  
  rows <- rows[!map_lgl(flattenlist(list_el), is.null)]
  
  data.frame(
    Table = rows,
    `Download?` = shinyInput(checkboxInput, length(rows),value = T, "checktable_RM", width = "10%"),
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

