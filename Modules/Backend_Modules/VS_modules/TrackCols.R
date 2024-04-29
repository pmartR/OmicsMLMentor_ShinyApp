

KeepColsModule <- function(id) {
  ns <- NS(id)
  uiOutput(ns("picker_inputs"))
}

## Observer function for loading into RV and updating preview datatable tabs
KeepColsServer <- function(id,
                           f_data_file,
                           f_data_cname,
                           # e_data_cname,
                           # e_data_file,
                           e_meta_cname,
                           e_meta_file,
                           data_type,
                           user_level
                           ) {
  
  moduleServer(
    id,
    function(input, output, session) {

      file_reactive <- reactiveValues(
        # e_data_file = NULL,
        f_data_file_new = NULL,
        f_data_file = NULL,
        f_data_file_new = NULL,
        e_meta_file = NULL,
        e_meta_file_new = NULL,
        f_data_cname = NULL,
        # e_data_cname = NULL,
        e_meta_cname = NULL,
        user_level = NULL
        )

      warning_reactive <- reactiveValues(bad_regex = NULL)

      # parent session (for tabset appending)
      parentSession <- get("session", envir = parent.frame(2))

      ## Observers reactive val ##

      # observeEvent(e_data_file(), {
      #   val <- e_data_file()
      #   file_reactive$e_data_file <- val
      # })
      # 
      # observeEvent(e_data_cname(), {
      #   val <- e_data_cname()
      #   file_reactive$e_data_cname <- val
      # })

      # observeEvent(e_data_cname(), {
      #   val <- e_data_cname()
      #   file_reactive$e_data_cname <- val
      # })
      
      observeEvent(user_level(), {
        val <- user_level()
        file_reactive$user_level <- val
      })

      observeEvent(f_data_cname(), {
        val <- f_data_cname()
        file_reactive$f_data_cname <- val
      })
      
      observeEvent(f_data_file(), {
        val <- f_data_file()
        file_reactive$f_data_file <- val
      })

      observeEvent(e_meta_file(), {
        val <- e_meta_file()
        file_reactive$e_meta_file <- val
      })

      observeEvent(e_meta_cname(), {
        val <- e_meta_cname()
        file_reactive$e_meta_cname <- val
      })

      ## Column tracking inputs
      output$picker_inputs <- renderUI({

        req(#!is.null(file_reactive$e_data_file) ||
              !is.null(file_reactive$f_data_file) ||
              !is.null(file_reactive$e_meta_file))

        #track_e_data <- colnames(file_reactive$e_data_file)
        
        f_data <- file_reactive$f_data_file
        e_meta <- file_reactive$e_meta_file
        
        if(!is.null(f_data) && file_reactive$user_level != "expert"){
          track_f_data <- colnames(f_data[apply(f_data, 2, function(x) 
            var(as.numeric(as.factor(x)), na.rm = T)) > 0])
        } else track_f_data <- colnames(file_reactive$f_data_file)
        
        if(!is.null(e_meta) && file_reactive$user_level != "expert"){
          track_e_meta <- colnames(e_meta[apply(e_meta, 2, function(x) 
            var(as.numeric(as.factor(x)), na.rm = T)) > 0])
        } else track_e_meta <- colnames(file_reactive$e_meta_file)

        # track_e_data <- track_e_data[track_e_data != file_reactive$e_data_cname]
        # track_f_data <- track_f_data[track_f_data != file_reactive$f_data_cname]
        # track_e_meta <- track_e_meta[track_e_meta != file_reactive$e_meta_cname]

        pickers <- map2(
          list(#track_e_data, 
               track_f_data, track_e_meta),
          c(#"e_data", 
            "f_data", "e_meta"), 
          function(x, lab){
                          
            if(length(x) != 0){

              id_col <- file_reactive[[paste0(lab, "_cname")]]
              
              selected <- isolate(
                if(!is.null(input[[paste0(lab, "_track")]])){
                  c(file_reactive[[paste0(lab, "_cname")]], 
                    input[[paste0(lab, "_track")]])
                } else x
              )

              val <- data_type()

              tablabel <- switch(lab,
                                 # e_data = ifelse(val == "RNA-seq",
                                 #                 "Expression data",
                                 #                 "Abundance data"),
                                 f_data = "Sample data",
                                 e_meta = "Biomolecule information"
              )

              pickerInput(session$ns(paste0(lab, "_track")),
                          paste0("Select columns to track in ", tablabel),
                          multiple = T,
                          choices = x,
                          selected = selected,
                          options = list( `live-search` = TRUE, 
                                          `actions-box` = TRUE),
                          choicesOpt = list(disabled = x == id_col)
              )
            }
            })


        div(
          pickers
        )
      })

      map(c(#"e_data", 
            "f_data", "e_meta"), function(lab){

        observeEvent(input[[paste0(lab, "_track")]], {

          cols <- input[[paste0(lab, "_track")]]
          add_col <- file_reactive[[paste0(lab, "_cname")]]

          df <- file_reactive[[paste0(lab, "_file")]]

          if(!is.null(input[[paste0(lab, "_track")]])){
            out <- df[colnames(df) %in% c(add_col, input[[paste0(lab, "_track")]])]
          } else out <- df

          file_reactive[[paste0(lab, "_file_new")]] <- out

        })

      })

      return(file_reactive)

    })
}

keep_cols <- reactiveValues(result = NULL)

observeEvent(omicsData$objMSU, {
  req(!is.null(omicsData$objMSU))

  keep_cols$result <- KeepColsServer(
    id = "keepcols",
    # e_data_file = reactive(omicsData$objMSU$e_data),
    f_data_file = reactive(omicsData$objMSU$f_data),
    e_meta_file = reactive(omicsData$objMSU$e_meta),
    f_data_cname = reactive(input$f_data_id_col),
    # e_data_cname = reactive(pmartR::get_edata_cname(omicsData$objMSU)),
    e_meta_cname = reactive(input$e_meta_id_col),
    data_type = reactive(input$data_type),
    user_level = reactive(input$user_level_pick)
  )

}, once = T)


map(c(#"e_data", 
      "f_data", "e_meta"), function(lab){
  observeEvent(keep_cols$result[[paste0(lab, "_file_new")]], {
    req(!is.null(keep_cols$result[[paste0(lab, "_file_new")]]))
    preview_keep_cols$result[[paste0(lab, "_file")]] <- keep_cols$result[[paste0(lab, "_file_new")]]
  })
})



