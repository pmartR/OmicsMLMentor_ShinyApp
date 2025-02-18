
unsup_plot_call <- function(model, slData, method, 
                            color_by, supervised, plot_type,
                            label.vjust = 0.5,
                            label.hjust = 1.5,
                            label_size = 5,
                            component_x = 1,
                            component_y = 2
                            ){
  
  args_use <- attr(model, "args_unsup")
  
  if (inherits(model, "slRes.cluster")) {
    if (!isTruthy(plot_type %in% c("dendro", "pca"))) plot_type <- "pca"
  } else if (inherits(model, "slRes.embed")) {
   if (is.null(plot_type)) plot_type <- "scatter"
  }
  
  # base plot call
  plot_call = rlang::expr(
    plot(
      model, 
      plotType = plot_type
    )
  )
  
  # plot depends on whether we have a cluster or embedding result
  if (inherits(model, "slRes.cluster")) {
    if (plot_type == "dendro") {
      
      if(!is.null(slData$f_data)){
        plot_call = rlang::call_modify(
          plot_call,
          slData = slData,
          label_obs = TRUE,
          k = args_use$num_clusters,
          color_by = color_by,
          label.vjust = label.vjust,
          label.hjust = label.hjust,
          label_size = label_size
        )
      } else {

        slData$f_data <- data.frame(
          SampleID = colnames(slData$e_data)[
            -which(colnames(slData$e_data) %in% get_edata_cname(slData))
          ]
        )
        
        plot_call = rlang::call_modify(
          plot_call,
          slData = slData,
          label_obs = TRUE,
          k = args_use$num_clusters,
          color_by = color_by,
          label.vjust = label.vjust,
          label.hjust = label.hjust,
          label_size = label_size
        )
        
      }
    } else if (plot_type == "pca") {
      
      
      plot_call = rlang::call_modify(
        plot_call, 
        slData = slData,
        color_by = color_by,
        ellipse = TRUE
      )

    }
    
  } else if (inherits(model, "slRes.embed")) {
    if (plot_type == "scatter") {

      plot_call = rlang::call_modify(
        plot_call,
        components = c(component_x, component_y),
        slData = slData,
        color_by = color_by#,
        # alpha = 1,
        # add_stroke = FALSE,
        # pch = 19
      )
    }
    
  }
  
  set.seed(args_use$seed)
  p <- rlang::eval_tidy(plot_call, env = environment())
  
  if(plot_type != "dendro"){
    tbl <- p$data
  } else {
    
    fit_obj <- workflows::extract_fit_engine(model)
    
    fit_obj$labels <- colnames(slData$e_data)[
      -which(colnames(slData$e_data) == get_edata_cname(slData))
    ]
    
    hcdata <- dendro_data_k(fit_obj, k =  args_use$num_clusters)
    
    label_data <- ggdendro::label(hcdata)
    
    tbl <- label_data[3:4]
  }
  
  list(plot = p, table = tbl)
  
}
