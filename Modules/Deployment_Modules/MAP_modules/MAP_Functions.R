## Minio Midpoint Upload
#' @details Parse and store header parameters.  If we see the 'map-data' prefix, load that up!
observeEvent(input$`__startup__`, {
  req(!isTruthy(AWS))
  
  on.exit({
    Sys.sleep(1)
    hide("loading-gray-overlay")
  })
  
  query <- parseQueryString(session$clientData$url_search)

  # establish minio connection if we are pulling cloud resources
  if (any(names(query) %in% VALID_HEADER_PARAMS)) {
    isolate({
      # store header params in a reactive variable
      for (key in names(query)) {
        header_params[[key]] <- query[[key]]
        message(sprintf("INFO: stored parameter %s: %s", key, query[[key]]))
      }

      if ('map-data' %in% names(query)) {
        shinyjs::show("loading-gray-overlay")
        html(selector = "#loading-gray-overlay > div", html = "Loading MAP data...")
        
        # get the appropriate minio config for retrieving CoreMS files
        cfg_location = if (Sys.getenv("MINIO_CONFIG_PATH") == "") "./cfg/minio_config.yml" else Sys.getenv("MINIO_CONFIG_PATH")
        minio_con <<- mapDataAccess::map_data_connection(cfg_location)

        res <- store_minio_data(query[['map-data']])
        minio_upload_success <- res[[1]]
        modalmessage <- res[[2]]

        if (minio_upload_success) {
          showModal(minio_upload_success_modal(modalmessage))
        } else {
          showNotification(
            modalmessage,
            duration = NULL,
            type = "error"
          )
        }
      }
    })
  } else if (length(names(query)) > 0) {
    showNotification(
      sprintf("No valid header parameters found in query string.  Found parameters: (%s).  Valid header parameters: (%s).  Presenting manual upload dialog.", paste(names(query), collapse = ", "), paste(VALID_HEADER_PARAMS, collapse = ", ")),
      duration = NULL,
      type = 'error'
    )
    
    launch_tutorial()
  } else {
    launch_tutorial()
  }
}, priority = -10, ignoreNULL = FALSE, once = TRUE)

#' @details store data files in the appropriate reactiveValues and
#' return a success or error message for use in a modal
#'
#' @param uri character vector of file paths, either retrieved using mapDataAccess
#'
#' @return modalmessage character vector of message to be used in a modal
#'
store_minio_data <- function(uri) {
  minio_upload_success <- tryCatch({
    pullData <- mapDataAccess::get_data(minio_con, uri)

    if (class(pullData) == "project omic") {
      minio_upload_data$project_omic <- pullData
    }

    modalmessage <- div(class = "column-scroll-sm",
                        HTML(ttext[["MINIO_IMPORT_SUCCESS"]]))
    TRUE
  }, error = function(e) {
    modalmessage <<- div(class = "column-scroll-sm",
                         HTML(sprintf(ttext[["MINIO_IMPORT_ERROR"]], e)),
                         br(),
                         "Please contact the administrators for assistance.")
    FALSE
  })

  return(list(minio_upload_success, modalmessage))
}

#' @details Modal indicating minio data was successfully uploaded
minio_upload_success_modal <- function(modal_message) {
  modalDialog(
    modal_message, title = "MAP Upload Success",
    footer = tagList(
      # Here would probably be a button to go to upload.
      actionButton("dismiss_minio_success", "Dismiss", class = "btn-primary")
    )
  )
}

#' @details observe behavior for the 'dismiss_minio_success' modal action button
observeEvent(input$dismiss_minio_success, {
  removeModal()
  launch_tutorial()
})

#' @details helper boolean reactive to test if we are pulling data from minio
data_from_map <- reactive({
  !is.null(minio_upload_data$project_omic)
})

### Groups Tab ###
#'@details Disable/modify all inputs on the Upload tab that are pre-determined if a user comes in with data from map:
#' - e_data file input 'e_data_file'
#' - e_meta file input 'e_meta_file'
#' - PickerInput 'data_type' (if DataType is present)
#' - Prettyswitch 'have_emeta' (if e_meta file is present)
observeEvent(c(input$top_page, input$f_data_file, input$use_fdata, input$how_make_fdata), {
  req(input$top_page == "Upload", data_from_map())
  
  updateRadioGroupButtons(session, "use_fdata", selected = "f_data")
  togglestate_add_tooltip(session, "use_fdata_js", condition = FALSE, tooltip_text = "Uploaded from map")
  
  updateRadioGroupButtons(session, "how_make_fdata", selected = "upload")
  togglestate_add_tooltip(session, "how_make_fdata_js", condition = FALSE, tooltip_text = "Uploaded_from_map")
  
}, ignoreNULL = FALSE)

