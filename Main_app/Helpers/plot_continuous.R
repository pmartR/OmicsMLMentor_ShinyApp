
plot_continuous <- function(omicsData, plot_fn, col){
  
  ylabel <- if (attr(omicsData, "data_info")$data_scale == "abundance") {
      "Abundance"
      } else if (attr(omicsData, "data_info")$data_scale %in% c(
        "log", "log2", "log10")) {
      paste(attr(omicsData, "data_info")$data_scale, "Abundance", 
                   sep = " ")
    } else if (attr(omicsData, "data_info")$data_scale == "counts") {
        "Counts"
    }
  
  p <- plot_fn(omicsData)
  p$data <- dplyr::left_join(
    p$data, omicsData$f_data, 
    by = c("variable" = attr(omicsData, "cnames")$fdata_cname))
  
  p <- ggplot(
    data = p$data, aes(x = variable, y = value, 
                       fill = !!rlang::sym(col))) + 
    geom_boxplot() +
    scale_fill_distiller(palette = "Spectral") +
    labs(x = "Sample", y = ylabel) +
    theme(axis.text.x = element_text(angle = 90))
  
  p
  
}
