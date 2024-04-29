
## This needs to be modified for the overlord tab -- must skip this section


output$rollup_tab <- renderUI({
  nm <- str_to_title(class(omicsData$objPP)[[1]])
  out <- rollup_tab(nm)
  # if(!(nm %in% c("Pepdata", "Isobaricpepdata"))){
  #   out <- div()
  # }
  out
})