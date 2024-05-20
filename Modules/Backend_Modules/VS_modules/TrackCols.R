

KeepColsModule <- function() {
  uiOutput("picker_inputs")
}

## Observer function for loading into RV and updating preview datatable tabs
# KeepColsServer <- function(id,
#                            f_data_file,
#                            f_data_cname,
#                            # e_data_cname,
#                            # e_data_file,
#                            e_meta_cname,
#                            e_meta_file,
#                            data_type,
#                            user_level
#                            ) {
#   
#   moduleServer(
#     id,
#     function(input, output, session) {

      file_reactive_kc <- reactiveValues(
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

      ## Observers reactive val ##
      
      observeEvent(input$user_level_pick, {
        val <- input$user_level_pick
        file_reactive_kc$user_level <- val
      })

      observeEvent(input$f_data_id_col, {
        val <- input$f_data_id_col
        file_reactive_kc$f_data_cname <- val
      })
      
      observeEvent(omicsData$objMSU$f_data, {
        val <- omicsData$objMSU$f_data
        file_reactive_kc$f_data_file <- val
      })

      observeEvent(omicsData$objMSU$e_meta, {
        val <- omicsData$objMSU$e_meta
        file_reactive_kc$e_meta_file <- val
      })

      observeEvent(input$e_meta_id_col, {
        val <- input$e_meta_id_col
        file_reactive_kc$e_meta_cname <- val
      })

      ## Column tracking inputs
      output$picker_inputs <- renderUI({

        if (is.null(file_reactive_kc$f_data_file))
          return("No Sample Information specified. Click Done to continue.")
        
        req(!is.null(file_reactive_kc$f_data_file) ||
              !is.null(file_reactive_kc$e_meta_file))
        
        f_data <- file_reactive_kc$f_data_file
        e_meta <- file_reactive_kc$e_meta_file
        
        if(!is.null(f_data) && file_reactive_kc$user_level != "expert"){
          track_f_data <- colnames(f_data[apply(f_data, 2, function(x) 
            var(as.numeric(as.factor(x)), na.rm = T)) > 0])
        } else track_f_data <- colnames(file_reactive_kc$f_data_file)
        
        if(!is.null(e_meta) && file_reactive_kc$user_level != "expert"){
          track_e_meta <- colnames(e_meta[apply(e_meta, 2, function(x) 
            var(as.numeric(as.factor(x)), na.rm = T)) > 0])
        } else track_e_meta <- colnames(file_reactive_kc$e_meta_file)

        pickers <- map2(
          list(track_f_data, track_e_meta),
          c("f_data", "e_meta"), 
          function(x, lab){
                          
            if(length(x) != 0){

              id_col <- file_reactive_kc[[paste0(lab, "_cname")]]
              
              selected <- isolate(
                if(!is.null(input[[paste0(lab, "_track")]])){
                  c(file_reactive_kc[[paste0(lab, "_cname")]], 
                    input[[paste0(lab, "_track")]])
                } else x
              )

              val <- input$data_type

              tablabel <- switch(lab,
                                 f_data = "Sample Information",
                                 e_meta = "Biomolecule information"
              )

              pickerInput(paste0(lab, "_track"),
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

      map(c("f_data", "e_meta"), function(lab){

        observeEvent(input[[paste0(lab, "_track")]], {

          cols <- input[[paste0(lab, "_track")]]
          add_col <- file_reactive_kc[[paste0(lab, "_cname")]]

          df <- file_reactive_kc[[paste0(lab, "_file")]]

          if(!is.null(input[[paste0(lab, "_track")]])){
            out <- df[colnames(df) %in% c(add_col, input[[paste0(lab, "_track")]])]
          } else out <- df

          file_reactive_kc[[paste0(lab, "_file_new")]] <- out

        })

      })


# observeEvent(omicsData$objMSU, {
#   req(!is.null(omicsData$objMSU))
# 
#   file_reactive_kc$result <- KeepColsServer(
#     id = "keepcols",
#     f_data_file = reactive(omicsData$objMSU$f_data),
#     e_meta_file = reactive(omicsData$objMSU$e_meta),
#     f_data_cname = reactive(input$f_data_id_col),
#     e_meta_cname = reactive(input$e_meta_id_col),
#     data_type = reactive(input$data_type),
#     user_level = reactive(input$user_level_pick)
#   )
# 
# }, once = T)


map(c("f_data", "e_meta"), function(lab){
  observeEvent(file_reactive_kc[[paste0(lab, "_file_new")]], {
    req(!is.null(file_reactive_kc[[paste0(lab, "_file_new")]]))
    preview_keep_cols$result[[paste0(lab, "_file")]] <- file_reactive_kc[[paste0(lab, "_file_new")]]
  })
})



