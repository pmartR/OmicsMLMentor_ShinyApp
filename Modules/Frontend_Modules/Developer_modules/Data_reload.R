## Data reload

# Button to reload data object
# observeEvent(input$Reload, {
#   # reload postmortem data object
#   if (exists("RELOAD_POSTMORTEM_DATA__")) {
#     lapply(names(RELOAD_POSTMORTEM_DATA__), function(x) {
#       datatypes_pos[[x]] <<- RELOAD_POSTMORTEM_DATA__[[x]]
#     })
#   }
#   
#   if (exists("RELOAD_POSTMORTEM_OMICSDATA__")) {
#     lapply(names(RELOAD_POSTMORTEM_OMICSDATA__), function(x) {
#       omicsData_objects[[x]] <<- RELOAD_POSTMORTEM_OMICSDATA__[[x]]
#     })
#   }
#   
#   if (exists("RELOAD_POSTMORTEM_FILTERS__")) {
#     lapply(names(RELOAD_POSTMORTEM_FILTERS__), function(x) {
#       filters[[x]] <<- RELOAD_POSTMORTEM_FILTERS__[[x]]
#     })
#   }
# })

# local file, not tracked by git.  Create one if you would like to perform postmortem debugging
# tryCatch(
#   {
#     source("./untracked/store_postmortem_objects.R", local = TRUE)
#   },
#   error = function(e) message("Not storing postmortem objects")
# )
#
# EXAMPLE store_postmortem_objects.R:
#
# observeEvent(c(objects$omicsData, objects$omicsData_2),{
#   omicsData_postmortem <<- objects$omicsData
#   omicsData_2_postmortem <<- objects$omicsData_2
# })
#
# # postmortem debugging for plots
# observeEvent(plots$allplots,{
#   if(length(plots$allplots) > 0){
#     plots_postmortem <<- reactiveValuesToList(plots)
#   }
# })
#
# # postmortem reactive value debugging
# observeEvent(reactiveValuesToList(objects),{
#   objects_postmortem <<- reactiveValuesToList(objects)
# })


# handy to store all inputs in a reactive object
# all_inputs <- eventReactive(saveoninputs,{
#   x <- isolate(reactiveValuesToList(input))
#   x
# })