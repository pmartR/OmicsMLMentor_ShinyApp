## Bookmarking


# bookmarking logic
# enableBookmarking(store = "server")

# save and restore reactive values on bookmark
# setBookmarkExclude(c('input_name1', 'input_name2'))

# onBookmark(function(state) {
#   state$values$revals <- reactiveValuesToList(revals)
#   state$values$objects <- reactiveValuesToList(objects)
# })

# onRestore(function(state) {
#    for(name in names(state$values$revals)){
#      revals[[name]] <- state$values$revals[[name]]
#    }
#    for(name in names(state$values$objects)){
#      objects[[name]] <- state$values$objects[[name]]
#    }
# })
#