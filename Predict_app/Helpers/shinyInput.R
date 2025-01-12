

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}

getShinyInput <- function(input, str){
  
  values <- names(input)[str_detect(names(input), str)]
  res <- map_int(values, function(val) {
    input[[val]]
  })
  return(res)
}
