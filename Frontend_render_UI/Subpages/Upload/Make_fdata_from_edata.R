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

file_reactive_make_fdata <- reactiveValues(f_data_file = NULL,
                                e_data_cname = NULL,
                                e_data_file = NULL, 
                                how_make = NULL,
                                data_select_val= NULL,
                                use_example = NULL)

preview_tabset = "preview_data_f_data"

## Observers reactive val ##

observeEvent(reactive_dataholder$e_data$file, {
  val <- reactive_dataholder$e_data$file
  file_reactive_make_fdata$e_data_file <- val
})

observeEvent(input$e_data_id_col, {
  val <- input$e_data_id_col
  file_reactive_make_fdata$e_data_cname <- val
})

observeEvent(input$how_make_fdata, {
  val <- input$how_make_fdata
  file_reactive_make_fdata$how_make <- val
})

observeEvent(input$use_fdata, {
  val <- input$use_fdata
  file_reactive_make_fdata$data_select_val <- val
})

observeEvent(input$use_example_fdata, {
  val <- input$use_example_fdata
  file_reactive_make_fdata$use_example <- val
})

## Observers ##

observeEvent(
  c(file_reactive_make_fdata$how_make, file_reactive_make_fdata$data_select_val, 
    file_reactive_make_fdata$use_example), {
       if(!file_reactive_make_fdata$use_example && !AWS && !data_from_map()){
         
         removeTab(preview_tabset, "Sample Information", session = session)
         
         if("f_data" %in% file_reactive_make_fdata$data_select_val){
           
           req(!is.null(file_reactive_make_fdata$how_make))
           if (file_reactive_make_fdata$how_make == "colnames") {
             
             cnames <- colnames(file_reactive_make_fdata$e_data_file)
             cnames <- cnames[cnames != file_reactive_make_fdata$e_data_cname]
             
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
             
             file_reactive_make_fdata$f_data_file <- df
             
             ## make sure tab exists
             appendTab(preview_tabset, 
                       select = T,
                       tabPanel("Sample Information",
                                br(),
                                DTOutput("DT_f_data"),
                                br()
                       ),
                       session = session
             )
             
           }
           
         } else {
           file_reactive_make_fdata <- reactiveValues(f_data_file = NULL,
                                           e_data_cname = NULL,
                                           e_data_file = NULL, 
                                           how_make = NULL,
                                           data_select_val = NULL,
                                           use_example = NULL)
         }
       }
      })

observeEvent(c(file_reactive_make_fdata$f_data_file, input$use_example_fdata), {
  req(!is.null(file_reactive_make_fdata$f_data_file) && !input$use_example_fdata)
  reactive_dataholder[["f_data"]]$file <- file_reactive_make_fdata$f_data_file
})



