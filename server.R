options(shiny.maxRequestSize = 250 * 1024^2, ch.dir = TRUE, DT.TOJSON_ARGS = list(na = "string"))

# # internal function: throws error message if variable is not found and can_be_empty==FALSE
# get_config_variable <- function(cfg, varname, can_be_empty=FALSE) {
#   if (!(varname %in% names(cfg))) {
#     if (!can_be_empty)
#       stop(sprintf("Cannot find '%s' in config file", varname))
#     else
#       return('')
#   }
#   value <- cfg[[varname]]
#   return(value)
# }

shinyServer(function(session, input, output) {
  
  file_loads <- c(
    list.files("./Modules/", recursive = T, full.names = T),
    list.files("./Static_UI_renderUI/", recursive = T, full.names = T)
  )
  
  for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)
  
  
  ## GLOBAL VARIABLES ##
  
  # misc reactive values
  # aux_revals <- reactiveValues(warnings_goals = list(), 
  #                              warnings_upload = list(), 
  #                              warnings_fmeta = list(), 
  #                              picker_order = list(), 
  #                              reset_file_emeta = 1)


  # integration_results <- reactiveValues()
  # 
  # stat_analysis_results <- reactiveValues()

  ##########################
  
  # tictoc::tic("RV")
  # ## Reactive values
  # # source("./Reactive_variables/Datatype_revals.R", local = TRUE)
  # # source("./Reactive_variables/groups_revals.R", local = TRUE)
  # # source("./Reactive_variables/filter_revals.R", local = TRUE)
  # # source("./Reactive_variables/norm_revals.R", local = TRUE)
  # # source("./Reactive_variables/stats_all_revals.R", local = TRUE)
  # for (f in Sys.glob("./Reactive_variables/*.R")) source(f, local = TRUE)
  # tictoc::toc()
  # 
  # tictoc::tic("start")
  # ## On start
  # source("./UI_elements/startup.R", local = TRUE)
  # tictoc::toc()
  # 
  # ## outputUI specifications
  # tictoc::tic("goals")
  # source("./UI_elements/goals_UI.R", local = TRUE)
  # tictoc::toc()
  # tictoc::tic("upload")
  # source("./UI_elements/upload_UI.R", local = TRUE)
  # tictoc::toc()
  # source("./UI_elements/groups_UI.R", local = TRUE)
  # source("./UI_elements/filter_UI.R", local = TRUE)
  # 
  # tictoc::tic("norm")
  # source("./UI_elements/EDA_UI.R", local = TRUE)
  # source("./UI_elements/norm_UI.R", local = TRUE)
  # tictoc::toc()
  # tictoc::tic("progress")
  # source("./UI_elements/progress_UI.R", local = TRUE)
  # tictoc::toc()
  # tictoc::tic("reference")
  # source("./UI_elements/reference_UI.R", local = TRUE)
  # tictoc::toc()
  # tictoc::tic("navbar")
  # source("./UI_elements/navbar_UI.R", local = TRUE)
  # tictoc::toc()
  # source("./UI_elements/rollup_UI.R", local = TRUE)
  # source("./UI_elements/Stats_UI.R", local = TRUE)
  # source("./UI_elements/pepstats_UI.R", local = TRUE)
  # source("./UI_elements/integration_UI.R", local = TRUE)
  # source("./UI_elements/download_UI.R", local = TRUE)
  # 
  # tictoc::tic("obs")
  # ## Observers
  # # source("./Observers/goals_observers.R", local = TRUE)
  # # source("./Observers/upload_observers.R", local = TRUE)
  # # source("./Observers/groups_observers.R", local = TRUE)
  # # source("./Observers/norm_observers.R", local = TRUE)
  # # source("./Observers/filter_observers.R", local = TRUE)
  # # source("./Observers/reference_observers.R", local = TRUE)
  # # source("./Observers/stats_observers.R", local = TRUE)
  # # source("./Observers/pepstats_observers.R", local = TRUE)
  # # source("./Observers/rollup_observers.R", local = TRUE)
  # ## Observers - (Tab functionality, disable/reset )
  # for (f in Sys.glob("./Observers/*.R")) source(f, local = TRUE)
  # tictoc::toc()

  #################################

  exportTestValues(
    holder_rv = reactiveValuesToList(datatypes_pos),
    dataholder = {
      res <- datatypes_pos$dataholder
      res2 <- map(res, function(list_items) {
        list_items[!(names(list_items) %in% c("Edata", "Emeta"))] # Doesn't include df
      })
      names(res2) <- names(res)
      res2
    },
    popup_rv = reactiveValuesToList(popup),
    spans_rv = reactiveValuesToList(SPANS_res),
    norm_rv = reactiveValuesToList(norm_settings),
    norm_ui_rv = reactiveValuesToList(normtracker),
    filter_rv = reactiveValuesToList(filters),
    filter_rv = reactiveValuesToList(filter_flags),
    omicsobj_rv = reactiveValuesToList(omicsData_objects),
    Tabs_active = {
      tabs <- c("Goals", "Upload", "Reference", "Groups", "Filter", "Normalization")
      res <- map(tabs, function(tab) {
        js$isTabdisabled(tab)
        out <- input[[paste0("jscatch_disabled_", tab)]]
        !as.logical(out)
      })
      names(res) <- tabs
      res
    },
    icons_active_goals = {
      icons <- c(
        "datselect_icon", "prodat_icon", "lipdat_icon",
        "metabdat_icon", "goals-warning-tooltip"
      )
      res <- map(icons, function(icon) {
        js$isIconhidden(icon)
        out <- input[[paste0("jscatch_icon_", icon)]]
        !as.logical(out)
      })
      names(res) <- icons
      res
    },
    icons_active_upload = {
      tabs <- names(datatypes_pos$dataholder)
      varibs <- c(
        "_edata_icon", "_edata_params_icon", "_meta_collapse_icon",
        "_meta_collapse_icon2"
      )
      icons <- c(
        unlist(map(tabs, paste0, varibs)),
        "upload-warning-tooltip",
        paste0(tolower(tabs), "_upload-warning-tooltip")
      )

      res <- map(icons, function(icon) {
        js$isIconhidden(icon)
        out <- input[[paste0("jscatch_icon_", icon)]]
        !as.logical(out)
      })
      names(res) <- icons
      res
    },
    icons_active_reference = {
      tabs <- names(datatypes_pos$dataholder)
      tabs <- tabs[tabs %in% c("NMR", "Isobaric")]
      varibs <- c("_fdata_reference_icon", "_reference_input_icon")
      icons <- c(
        unlist(map(tabs, paste0, varibs)),
        "reference-warning-tooltip",
        paste0(tolower(tabs), "_reference-warning-tooltip")
      )

      res <- map(icons, function(icon) {
        js$isIconhidden(icon)
        out <- input[[paste0("jscatch_icon_", icon)]]
        !as.logical(out)
      })
      names(res) <- icons
      res
    }
  )

  # # Observe any collapsible panels
  # observeEvent(input$collapseTitleClick, {
  #   req(input$collapseTitleClick)
  #   updateBoxCollapse(session, input$collapseTitleClick$p, toggle = input$collapseTitleClick$id)
  # })
  
  # Allow reconnect on server when disconnected on the server (e.g. network connection issues)
  session$allowReconnect(TRUE)
  # Rstudio version; test-able by closing, then reentering the URL in the rendered window
  # session$allowReconnect("force")
  # options(shiny.launch.browser=FALSE)
  
})
