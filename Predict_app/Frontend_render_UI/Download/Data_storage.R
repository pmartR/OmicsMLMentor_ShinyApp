## Data storage module


### Reactive values for plots
plot_table_current <- reactiveValues(
  table = list(
    PP__backfill = NULL,
    PP__transform = NULL,
    PP__normalization__post = NULL,
    PP__rollup__post = NULL,
    RM__model_eval__full__roc_curve = NULL,
    RM__model_eval__full__prediction_bar = NULL,
    RM__model_eval__full__confusion_heatmap = NULL,
    RM__model_eval__full__confidence_scatter = NULL
  ),
  
  names = list(
    PP__backfill = "Backfill plot",
    PP__transform = "Transformed boxplots",
    PP__normalization__post = "Normalization: Post",
    PP__rollup__post = "Protein quantification: Post",
   
    RM__model_eval__full__roc_curve = "Model evaluation: roc curve (full)",
    RM__model_eval__full__prediction_bar = "Model evaluation: prediction bar (full)",
    RM__model_eval__full__confusion_heatmap = "Model evaluation: confusion heatmap (full)",
    RM__model_eval__full__confidence_scatter = "Model evaluation: confidence scatter (full)"
  ),
  
  plot_options = list()
)


table_table_current <- reactiveValues(
  table = list(
    PP__transform = NULL,
    PP__normalization = NULL,
    PP__rollup = NULL,
    RM__model_eval = NULL
  ),

  names = list(
    PP__transform = "Transformed data",
    
    # PP__filters__*
    
    PP__normalization = "Normalization",
    PP__rollup = "Protein Quantification",
    RM__model_eval = "Model predictions"
  )
)

download_preview <- 
  reactiveValues(
    current = NULL,
    plot = F,
    name = NULL
  )

