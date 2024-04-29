## Functionality that was added for MAP 
## Last Updated: 2021_12_29


MAP_ACTIVE <- ifelse(Sys.getenv("MAP_VERSION") == "1", TRUE, FALSE) ## global var?

get_config_variable <- function(cfg, varname, can_be_empty=FALSE) {
  if (!(varname %in% names(cfg))) {
    if (!can_be_empty)
      stop(sprintf("Cannot find '%s' in config file", varname))
    else
      return('')
  }
  value <- cfg[[varname]]
  return(value)
}

## For general ui
# Add UI if MAP is enabled

output$MAP_button <- renderUI({
  if (MAP_ACTIVE) {
    div(id = "js_midpoint", style = "vertical-align:top",
        # bsButton("debug", "Debug"), 
        appButton(inputId = "exportMid", label = "Save and Export Progress", style = "success")
    )
  } else NULL
})

## Server

cfg_path = if(isTruthy(Sys.getenv("MAP_CONFIG")))
  Sys.getenv("MAP_CONFIG") else
    "./cfg/minio_config_local.yml"

## ADDED FOR MAP ##
if (MAP_ACTIVE) {
  # Connect to map data access library
  # library(mapDataAccess)
  
  # Pull data types
  dataTypes <- MapConnect$DataTypes
  
  # Get the appropriate pmart project object and upload 
  map_obj <-
    names(MAP_TO_SLOPE_NAME_MAP[which(MAP_TO_SLOPE_NAME_MAP == tab)]) %>%
    intersect(names(MapConnect$Project$Objects))
  
  MapConnect$pmart_project[[tab]] <- MapConnect$Project$Objects[[map_obj]]
  
  MAP_edata_filename <- "edata.csv"
  if (MapConnect$ProjectBuiltFrom == "pmart projects") {
    MAP_edata_filename <- MapConnect$pmart_project[[tab]]$Data$e_data_filename %>% 
      strsplit("/") %>% unlist() %>% tail(1)
  } 
  disabled(textInput(
    paste0(tab, "_file_edata"), "Expression Data Uploaded in MAP", 
    value = MAP_edata_filename
  ))
  
  if(input$sourcedata == "Uploaded data"){
    
    MAP_emeta_filename <- "emeta.csv"
    if (MapConnect$ProjectBuiltFrom == "pmart projects") {
      MAP_emeta_filename <- MapConnect$pmart_project[[tab]]$Data$e_meta_filename %>% 
        strsplit("/") %>% unlist() %>% tail(1)
    }
    
    ## change UI state
    
    disable(paste0(tab, "_clear_edata"))
    
    out <- list(
      disabled(textInput(paste0(tab, "_file_emeta"), 
                         "Biomolecule Information Data uploaded in MAP",
                         value = MAP_emeta_filename))
    )
    
    
  }
  
  
} else {

  MapConnect <- NULL
  
  cfg <- yaml::read_yaml(cfg_path)
  python_venv <- get_config_variable(cfg, "python_venv")
  
  conda_envs <- tryCatch(
    {
      # reticulate::conda_list()$python
    },
    error = function(e) {
      NULL
    }
  )
  
  is_conda <- any(sapply(conda_envs, function(envpath) {
    grepl(sprintf("^%s", normalizePath(python_venv)), envpath)
  }))
  
  ## add back in later
  
  # if (is_conda) {
  #   reticulate::use_condaenv(python_venv, required = TRUE)
  # } else {
  #   reticulate::use_virtualenv(python_venv, required = TRUE)
  # }
}

##### upload ui #####

# if (isolate(input$sourcedata) == "Uploaded data" & MAP_ACTIVE == FALSE) {
#   out <- list(
#     actionButton(paste0(tab, "_clear_edata"), "Clear Data")
#   )
#   
#   # Example settings
# } else {
#   out <- list(
#     disabled(actionButton(paste0(tab, "_clear_edata"), "Clear Data"))
#   )
# }

# if (input$sourcedata == "Uploaded data") {
#   if (MAP_ACTIVE) {
#     
#     MAP_emeta_filename <- "emeta.csv"
#     if (MapConnect$ProjectBuiltFrom == "pmart projects") {
#       MAP_emeta_filename <- MapConnect$pmart_project[[tab]]$Data$e_meta_filename %>% 
#         strsplit("/") %>% unlist() %>% tail(1)
#     }
#     
#     out <- list(
#       disabled(textInput(paste0(tab, "_file_emeta"), "Biomolecule Information Data uploaded in MAP",
#                          value = MAP_emeta_filename))
#     )
#     
#     
#   } else {
#     
#     out <- list(
#       fileInput(
#         paste0(tab, "_file_emeta"),
#         "Upload CSV Biomolecule Information File",
#         multiple = FALSE,
#         accept = c(
#           "text/csv",
#           "text/comma-separated-values,text/plain",
#           ".csv"
#         )
#       )
#     )
#     
#   }
#   
# } 

# if (isolate(input$sourcedata) == "Uploaded data" && MAP_ACTIVE == FALSE) {
#   out <- list(
#     
#     # actionButton(paste0(tab, '_db'), "DB Generate"),
#     actionButton(paste0(tab, "_clear_emeta"), "Clear Data")
#   )
#   
# }

########

## Groups

#' @description Create the omicsdata objects inplace.  For every set of input
#' data in datatypes_pos$dataholder, pull the relevant inputs and pass them to
#' the appropriate pmartR constructor function.
make_objects <- function() {
  
  if (MAP_ACTIVE == FALSE || MapConnect$ProjectBuiltFrom == "pmart projects") {
    objs <- lapply(names(datatypes_pos$dataholder), function(name) {
      
      funcname <- switch(name,
                         "Protein" = "as.proData",
                         "Label-free" = "as.pepData",
                         "Isobaric" = "as.isobaricpepData",
                         "Negative" = "as.lipidData",
                         "Positive" = "as.lipidData",
                         "GC-MS" = "as.metabData",
                         "NMR" = "as.nmrData",
                         "RNA-seq" = "as.seqData"
      )
      
      func <- get(funcname, envir = asNamespace("pmartR"), mode = "function")
      
      el <- datatypes_pos$dataholder[[name]]
      
      # We should have identified which rows to remove and stored them in this variable (el$to_rmv)
      if (length(el$to_rmv) > 0) {
        tmp_fdata <- datatypes_pos$Fmeta %>% filter(!(!!rlang::sym(el$Fdata_cname) %in% el$to_rmv))
        # subset edata to only values now existing in fdata
        if (name %in% c("Isobaric", "NMR") && !is.null(el$Edata_norm_applied)) {
          tmp_edata <- el$Edata_norm_applied %>% 
            select(one_of(c(el$Edata_cname, as.character(tmp_fdata[, el$Fdata_cname]))))
        } else {
          tmp_edata <- el$Edata %>% select(one_of(c(el$Edata_cname, as.character(tmp_fdata[, el$Fdata_cname]))))
        }
      } else {
        tmp_fdata <- datatypes_pos$Fmeta
        if (name %in% c("Isobaric", "NMR") && !is.null(el$Edata_norm_applied)) {
          tmp_edata <- el$Edata_norm_applied
        } else {
          tmp_edata <- el$Edata
        }
      }
      
      # isobaric takes an extra argument....(way to do this with one function call instead of if-else)
      if (name %in% c("Isobaric", "NMR")) {
        out <- func(
          e_data = tmp_edata, f_data = tmp_fdata, e_meta = el$Emeta,
          edata_cname = el$Edata_cname, fdata_cname = el$Fdata_cname,
          emeta_cname = el$Emeta_cname,
          is_normalized = ifelse(length(el$Normalized) == 0, FALSE, el$Normalized),
          data_scale = ifelse(is.null(el$Ref_applied) || !el$Ref_applied,
                              el$Scale,
                              ifelse(el$Transform == "No transformation",
                                     el$Scale,
                                     el$Transform
                              )
          ),
          isobaric_norm = any(el$Ref_applied, el$Inst_Normalized),
          check.names = FALSE
        )
        
        attr(out, paste0(tolower(name), "_info"))$norm_info$is_normalized <- any(el$Ref_applied, el$Inst_Normalized)
        
        return(out)
      } #### Currently not tracked for nmr
      
      return(func(
        e_data = tmp_edata, f_data = tmp_fdata, e_meta = el$Emeta, edata_cname = el$Edata_cname,
        fdata_cname = el$Fdata_cname, emeta_cname = el$Emeta_cname, data_scale = el$Scale, is_normalized = el$Normalized, check.names = FALSE
      ))
    }) %>% setNames(names(datatypes_pos$dataholder))
    
    return(objs)
  } else if (data_from_midpoint()) {
    # In this situation we assume f_meta is well-defined.  This will have been
    # checked in mapDataAccess.
    
    objs <- lapply(names(MapConnect$Project$Objects), function(name) {
      out_obj <- MapConnect$Project$Objects[[name]]$`Data Objects`$OmicsData
      
      name = MAP_TO_SLOPE_NAME_MAP[[name]]
      el <- datatypes_pos$dataholder[[name]]
      
      # Samples with incomplete information across all datasets are removed via
      # custom filter.  This is the case of mismatched samples.
      f_data_remove = out_obj$f_data %>% 
        filter(!!rlang::sym(get_fdata_cname(out_obj)) %in% el$to_rmv) %>% 
        purrr::pluck(get_fdata_cname(out_obj))
      
      if (length(f_data_remove) > 0) {
        out_obj <- applyFilt(
          custom_filter(out_obj, f_data_remove = f_data_remove),
          out_obj
        )
      }
      
      return(out_obj)
    }) %>% 
      setNames(MAP_TO_SLOPE_NAME_MAP[names(MapConnect$Project$Objects)])
    
    return(objs)
    
  }
}

########
if(MAP_ACTIVE){
  
  observeEvent(input$`__startup__`, {
    on.exit({
      hide("loading-gray-overlay")
    })
    
    req(is.null(MapConnect$MapConnect))
    
    cfg_path = if(Sys.getenv("MAP_CONFIG") == "") {
      "./cfg/minio_config.yml"
    } else Sys.getenv("MAP_CONFIG")
    
    MapConnect$MapConnect = map_data_connection(cfg_path)
    
    # Parse the query string at the url header
    query <- parseQueryString(session$clientData$url_search)
    
    # Set a conditional test. We only care if the "data" parameter exists. 
    cond <- length(query) != 0 && "data" %in% names(query) 
    
    modal_text = NULL
    
    # Trigger browser call if condition is true
    if (cond) {
      html(
        "loading-gray-overlay", 
        "<div class='fadein-out busy relative-centered', style='font-size:xx-large'>Loading saved data...</div>"
      )
      
      MapConnect$query <- query
      # initialize goals as 'complete'
      isolate({goals_changed$status <- F})
      
      # Get the data that was uploaded, and determine whether it is a project object,
      # or a midpoint object.
      pullData <- get_data(MapConnect$MapConnect, query$data)
      MapConnect$data = pullData
      
      # Put the data in its correct place depending on whether it's a project or midpoint
      if (class(pullData) == "project multiomics") {
        
        # Save project
        MapConnect$Project <- pullData
        message("")
        message("")
        message("Loading project data...")
        
        ## Fill out goals page
        
        if (MapConnect$Project$Objects %>% lapply(class) %>% unlist() %>% unique() == "project omic") {
            
          message(">> SLOPE project is constructed from pmart projects <<")
          MapConnect$ProjectBuiltFrom <- "pmart projects"
          
          # The data is uploaded
          updateCheckboxGroupButtons(session, "sourcedata", selected = "Uploaded data")
          
          # Get the loaded data types
          dataTypes <- data.frame("Objects" = names(pullData$Objects), 
                                  "Label" = lapply(pullData$Objects %>% names(), function(name) {
                                    if (name %in% c("Lipidomics-Negative", "Lipidomics-Positive")) {
                                      return("Lipidomics")
                                    } else if (name %in% c("Metabolomics-GC/LC-MS", "Metabolomics-NMR")) {
                                      return("Metabolomics")
                                    } else {return("Proteomics")}
                                  }) %>% unlist()
          )
          MapConnect$DataTypes <- dataTypes
          message(paste0("...data types are ", paste0(dataTypes$Objects, collapse = ", ")))
          updateCheckboxGroupButtons(session, "datatypes", selected = dataTypes$Label %>% unique())
          
          # Determine proteomics type: peptide or protein 
          ProteomicsData <- dataTypes[dataTypes$Label == "Proteomics", "Objects"]
          if (ProteomicsData %in% c("Protein-level Label Free", "Protein-level Isobaric")) {
            ProteinType <- "Protein"
          } else if (ProteomicsData %in% c("Peptide-level Label Free", "Peptide-level Isobaric")) {
            ProteinType <- "Peptide"
          } else {ProteinType <- NULL}
          message(paste0("...proteomics datatype is ", ProteinType))
          updateRadioGroupButtons(session, "peppro_select", selected = ProteinType)
          
          # Determine proteomics types: label-free or isobaric
          if (ProteomicsData %in% c("Peptide-level Label Free", "Protein-level Label Free")) {
            ProteinLabelType <- "Label-free"
          } else if (ProteomicsData %in% c("Peptide-level Isobaric", "Protein-level Isobaric")) {
            ProteinLabelType <- "Isobaric labeled"
          } else {ProteinLabelType <- NULL}
          message(paste0("...proteomics datatype is ", ProteinLabelType))
          updateRadioGroupButtons(session, "pep_type", selected = ProteinLabelType)
          updateRadioGroupButtons(session, "pro_type", selected = ProteinLabelType)
          
          # Determine lipidomics types
          LipidomicsTypes <- c()
          LipidomicsData <- dataTypes[dataTypes$Label == "Lipidomics", "Objects"]
          if ("Lipidomics-Positive" %in% LipidomicsData) {
            LipidomicsTypes <- c(LipidomicsTypes, "Positive")
          } else if ("Lipidomics-Negative" %in% LipidomicsData) {
            LipidomicsTypes <- c(LipidomicsTypes, "Negative")
          }
          message(paste0("...lipidomics datatypes are ", paste0(LipidomicsTypes, sep = ", ")))
          updateCheckboxGroupButtons(session, "lip_ion_type", selected = LipidomicsTypes)
          
          # Determine metabolomics types
          MetabolomicsTypes <- c()
          MetabolomicsData <- dataTypes[dataTypes$Label == "Metabolomics", "Objects"]
          if ("Metabolomics-GC/LC-MS" %in% MetabolomicsData) {
            MetabolomicsTypes <- c(MetabolomicsTypes, "GC-MS")
          }
          
          if ("Metabolomics-NMR" %in% MetabolomicsData) {
            MetabolomicsTypes <- c(MetabolomicsTypes, "NMR")
          }
          message(paste0("...metabolomics datatypes are ", paste0(MetabolomicsTypes, sep = ", ")))
          updateCheckboxGroupButtons(session, "metab_type", selected = MetabolomicsTypes)
          
          # Go to goals tab, click the engage button
          updateTabsetPanel(session, "top_page", selected = "Goals")
          observeEvent(input$engage_goals, once = T, ignoreNULL = FALSE, ignoreInit = T, {
            shinyjs::delay(10, shinyjs::click("engage_goals"))
          })
          
          modal_text = sprintf(ttext[["DATA_IMPORTED"]], "Upload or Goals")
          
        } else if (MapConnect$Project$Objects %>% lapply(class) %>% unlist() %>% unique() == "midpoint pmart") {
          
          message(">> SLOPE project is constructed from pmart midpoints <<")
          MapConnect$ProjectBuiltFrom <- "pmart midpoints"
          
          # Disable goals, upload, and reference tab - all others are already disabled
          js$disableTab("Goals")
          js$disableTab("Upload")
          js$disableTab("Reference")
          
          # Pull data into datatype_pos$dataholder
          for(name in names(MapConnect$Project$Objects)) {
            
            # Get object
            Obj <- MapConnect$Project$Objects[[name]]
            OmicsData <- Obj$`Data Objects`$OmicsData
            
            appname <- MAP_TO_SLOPE_NAME_MAP[[name]]
            
            show_datatype_tabs(appname)
            
            # Load stats
            stat_analysis_results[[appname]]$stats <- (
              MapConnect$Project$Objects[[name]]$`Data Objects`$OmicsStats
            ) 
            
            # Load raw data
            datatypes_pos$dataholder[[appname]] <- list(
              Normalized = TRUE,
              Scale = OmicsData %>% pmartR::get_data_scale(),
              Transform = "No transformation",
              Edata = OmicsData$e_data,
              Edata_cname = OmicsData %>% pmartR::get_edata_cname(),
              Edata_filename = Obj$Tracking$`Original Files`$Data$e_data_filename,
              Emeta = OmicsData$e_meta,
              Emeta_cname = OmicsData %>% pmartR::get_emeta_cname(),
              Emeta_filename = Obj$Tracking$`Original Files`$Data$e_meta_filename,
              NAsym = "NA"
            )
            
            message(paste("...Loaded data for", name))
            
          }
          
          # Enable and navigate to groups tab
          updateNavbarPage(session, "top_page", "Groups")
          
          # Upload fmeta
          if(!is.null(MapConnect$Project$fmeta)) {
            datatypes_pos$Fmeta <- MapConnect$Project$fmeta
    
            # Disable fmeta tag
            js$setFileInput("file_fmeta", "#js_file_fmeta .form-control", "Uploaded from MAP")
            disable("file_fmeta")
            
            # Force table to open 
            updateBoxCollapse(session, id = "fmeta_preview", open = TRUE)
          }
          modal_text = sprintf(ttext[["DATA_IMPORTED"]], "Groups")
        }
        
      } else if (class(pullData) == "midpoint SLOPE") {
        on.exit({
          shinyjs::show("loading-gray-overlay")
          html(
            "loading-gray-overlay", 
            "<div class='relative-centered', style='font-size:large;color:red;'>loading SLOPE midpoints not implemented.</div>"
          )  
        }, add = TRUE)
        
        return()
      }
      else {
        on.exit({
          shinyjs::show("loading-gray-overlay")
          html(
            "loading-gray-overlay", 
            "<div class='relative-centered', style='font-size:large;color:red;'>Specified object was not valid.  Check that object was properly constructed or load data manually. </div>"
          )  
        }, add = TRUE)
        
        return()
      }
      
      if(!is.null(modal_text)) {
        showModal(
          modalDialog(
            title = "Upload Success!",
            modal_text
          )
        ) 
      }
    }
  }, priority = -10, ignoreNULL = FALSE, once = TRUE)


  # Load in fmeta if it exists in the object
  observeEvent(input$upload_engage, {
    
    # Wait like half a second
    Sys.sleep(0.5)
    
    # Upload fmeta
    if(!is.null(MapConnect$Project$fmeta)) {
      datatypes_pos$Fmeta <- MapConnect$Project$fmeta
    
      # Disable fmeta tag
      js$setFileInput("file_fmeta", "#js_file_fmeta .form-control", "Uploaded from MAP")
      disable("file_fmeta")
      
      # Force table to open 
      updateBoxCollapse(session, id = "fmeta_preview", open = TRUE)
    }
  
  }, priority = 1)
  
  # Add an observer to trigger a specific version of Groups tab deployment after
  observe({
    
    if (MapConnect$ProjectBuiltFrom == "pmart midpoints") {
      
      observeEvent(input$apply_grouping, {
        
        # Enable the appropriate tabs
        DataTypes <- names(datatypes_pos$dataholder)
        
        # If any peptide tabs, enable peptide tab
        which_peps = DataTypes %in% c("Label-free", "Isobaric")
        if (any(which_peps)) {
          js$enableTab("Pepstats")
          assign_pepstats_output(c("Label-free", "Isobaric")[which_peps])
          js$enableTab("Roll-up")
          assign_rollup_output(c("Label-free", "Isobaric")[which_peps])
          
        } else {
          # If no stats, then enable statistics
          if (is.null(MapConnect$Project$Objects[[1]]$`Data Objects`$OmicsStats)) {
            js$enableTab("EDA")
            js$enableTab("Statistics")
          } else {
            js$enableTab("Integration")
          }
        } 
        
      })
    }
  })

}
