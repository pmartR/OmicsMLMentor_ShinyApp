
default_factor <- function(df){
  factor_cols <- apply(df, 2, is.factor) ## factor overrides numeric
  cat_cols <- suppressWarnings(apply(is.na(apply(df, 2, as.numeric)), 2, all))
  
  ## Some ids are numbers while others are not => not all NA
  if(!any(factor_cols) && !any(cat_cols)){
    cat_cols <- apply(is.na(apply(df, 2, as.numeric)), 2, sum) != apply(is.na(df), 2, sum)
  }
  
  for(i in 1:ncol(df)){
    to_string <- as.character(df[[i]])
    if(i %in% which(Reduce("|", factor_cols, cat_cols))){
      df[[i]] <- factor(to_string, levels = sort(unique(to_string)))
    } else {
      tryCatch({
        df[[i]] <- as.numeric(to_string)
      }, error = function(e){
        factor_warning <- c(factor_warning, colnames(df)[[i]])
      })
    }
  }
  
  df
}