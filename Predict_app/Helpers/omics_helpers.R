get_omicsData_type <- function(omicsData) {
  if (length(class(omicsData)) == 1)
    str_to_title(class(omicsData))
  else
    str_to_title(class(omicsData)[-which(class(omicsData) %in% c("slData", "isobaricpepData"))])
} 