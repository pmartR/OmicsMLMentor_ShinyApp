output$run_model_plots_UI <- renderUI({

  if(supervised()){
    
    collapseBoxGroup(
      
      collapseBox(
        "Prediction Plots",
        value = "predictions_plots_box",
        collapsed = F,
        
        tabsetPanel(
          tabPanel("Prediction Bar",
                   br(),
                   plotlyOutput("predict_plot_predictBar")),
          tabPanel("Confidence Scatter",
                   br(),
                   plotlyOutput("predict_plot_confidenceScatter"),
                   uiOutput("true_pos_picker_UI")),
          tabPanel("ROC Curve",
                   br(),
                   plotlyOutput("predict_plot_ROC")),
          tabPanel("Confusion Heatmap",
                   br(),
                   plotlyOutput("predict_plot_confusionHeatmap"))
        )
      )
      
    )
    
  } else {
    
    collapseBoxGroup(
      
      collapseBox(
        "Structure Plots",
        value = "predictions_plots_box",
        collapsed = F,
        
        tabsetPanel(
          tabPanel("Sample data comparison",
                   br(),
                   uiOutput("plot_type_UI"),
                   plotOutput("structure_plot"),
                   br(),
                   uiOutput("model_comparison"),
                   uiOutput("structure_plot_style_ui")
                   ),
          
        )
      )
      
    )
    
  }
  
  
})

output$model_comparison <- renderUI({
  
  model_data <- omicsData$model$pp_omics
  new_data <- omicsData$obj_pp
  
  
  no_fdata_new <- all(colnames(new_data$f_data) %in% c("SampleID", "Col1"))
  
  experimental_text <- if(!is.null(model_data$f_data) && !no_fdata_new){
    paste0("Sample Information was provided for both datasets",
           " and can be visualized in the 'Color by' dropdown below. Any",
           " missing fields in experimental group columns were replaced",
           " with a designation for which dataset the sample is sourced from ",
           "(i.e., 'Model data' or 'New data') for the combined plot.")
  } else if (!is.null(model_data$f_data)) {
    paste0("Sample Information was provided for only the model data. ",
           "Provided information can be visualized in the 'Color by' dropdown below. ",
           " New data samples were labeled as 'New data' for the combined plot.")
  } else if(!no_fdata_new){
    paste0("Sample Information was provided for only the new data. ",
           "Provided information can be visualized in the 'Color by' dropdown below. ",
           " Model data samples were labeled as 'Model data' for the combined plot.")
  } else {
    "No Sample Information was provided by either dataset. Only source visualizations are available."
  }
  
  div(
    "Original model data contained ", 
    strong(ncol(model_data$e_data) - 1), 
    " samples. New model data contained ",
    strong(ncol(new_data$e_data) - 1),
    " samples. Samples were plotted as points in the plots above. ",
    " If points in the combined plot are displayed in relatively similar locations",
    " without notable clusters by source, datasets can be considered to have",
    " similar latent data structures.",
  
  br(), br(),
  
  
  experimental_text,
  br(), br(),
  
  )
  
})


