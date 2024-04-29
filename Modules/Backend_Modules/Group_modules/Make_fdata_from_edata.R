## Module for generating fdata from edata
## requires seperate insert into preview_tabset

## Assumes inputs of 

# e_data_file = reactive(reactive_dataholder$e_data$file),
# e_data_cname = reactive(input$e_data_id_col),
# how_make_fdata = reactive(input$how_make_fdata),
# data_select_val = reactive(input$use_fdata),
# use_example = reactive(input$use_example_fdata)

## No ui

## Assumes reactive value reactive_dataholder to populate

####################

## Observer function for loading into RV and updating preview datatable tabs
MakeFdataFromEdataServer <- function(id, 
                            e_data_file,
                            e_data_cname,
                            how_make_fdata,
                            data_select_val,
                            use_example,
                            preview_tabset
                            ) {

  moduleServer(
    id,
    function(input, output, session) {
      
      file_reactive <- reactiveValues(f_data_file = NULL,
                                      e_data_cname = NULL,
                                      e_data_file = NULL, 
                                      how_make = NULL,
                                      data_select_val= NULL,
                                      use_example = NULL)
      
      warning_reactive <- reactiveValues(bad_regex = NULL)
      
      # parent session (for tabset appending)
      parentSession <- get("session", envir = parent.frame(2))
      
      ## Observers reactive val ##
      
      observeEvent(e_data_file(), {
        val <- e_data_file()
        file_reactive$e_data_file <- val
      })
      
      observeEvent(e_data_cname(), {
        val <- e_data_cname()
        file_reactive$e_data_cname <- val
      })
      
      observeEvent(how_make_fdata(), {
        val <- how_make_fdata()
        file_reactive$how_make <- val
      })
      
      observeEvent(data_select_val(), {
        val <- data_select_val()
        file_reactive$data_select_val <- val
      })
      
      observeEvent(use_example(), {
        val <- use_example()
        file_reactive$use_example <- val
      })
      
      ## Observers ##
      
      observeEvent(c(file_reactive$how_make, file_reactive$data_select_val, 
                     file_reactive$use_example), {
        
        if(!file_reactive$use_example && !AWS){

          removeTab(preview_tabset, "Sample data", session = parentSession)
          
          if("f_data" %in% file_reactive$data_select_val){
          
          req(!is.null(file_reactive$how_make))
            if (file_reactive$how_make == "colnames") {
    
              cnames <- colnames(file_reactive$e_data_file)
              cnames <- cnames[cnames != file_reactive$e_data_cname]
    
              df <- data.frame("SampleID" = cnames)
    
              ## Find seperator with most consistent string splits
              punc_list <- c("!", "'", "#", "%", "&", "'", "(", ")", "*", "+", ",",
                             "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[",
                             "/", "]", "^", "_", "{", "|", "}", "~")
    
              splits_punc <- map(punc_list, function(x) strsplit(cnames, x, fixed = T))
              splits_length <- map(splits_punc, function(x) map_int(x, length))
              non_one_splits <- map_lgl(splits_length, function(x) any(x != 1))
              use_punc <- which.max(map_dbl(splits_length, var)[non_one_splits])
    
              separater <- punc_list[non_one_splits][use_punc]
              length_into <- max(splits_length[non_one_splits][[use_punc]])
              vec_into <- paste0("SD_", 1:length_into)
    
              ## Generate predicted f_data
              df <- separate(df, "SampleID", sep = separater, into = vec_into, remove = F)
    
              file_reactive$f_data_file <- df
              
              ## make sure tab exists
              appendTab(preview_tabset, 
                        select = T,
                        tabPanel("Sample data",
                                 br(),
                                 DTOutput("DT_f_data"),
                                 br()
                        ),
                        session = parentSession
              )
    
            }
            
          } else {
            file_reactive <- reactiveValues(f_data_file = NULL,
                                            e_data_cname = NULL,
                                            e_data_file = NULL, 
                                            how_make = NULL,
                                            data_select_val = NULL,
                                            use_example = NULL)
          }
        }
      })

      return(file_reactive)
    }
  )
}

## set to update reactive_dataholder
make_f_data <- reactiveValues(result = NULL)


observeEvent(reactive_dataholder, {
  req(!is.null(reactive_dataholder))

  make_f_data$result <- MakeFdataFromEdataServer(
    id = "f_data",
    e_data_file = reactive(reactive_dataholder$e_data$file),
    e_data_cname = reactive(input$e_data_id_col),
    how_make_fdata = reactive(input$how_make_fdata),
    data_select_val = reactive(input$use_fdata),
    use_example = reactive(input$use_example_fdata),
    preview_tabset = "preview_data_f_data"
  )

}, once = T)

observeEvent(c(make_f_data$result$f_data_file, input$use_example_fdata), {
  req(!is.null(make_f_data$result$f_data_file) && !input$use_example_fdata)
  reactive_dataholder[["f_data"]]$file <- make_f_data$result$f_data_file
})



