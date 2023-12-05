# 
# ## upload overlord observers
# 
# observeEvent(input$show_transform, ignoreNULL = F, {
#   
#   shinyjs::show(id = "transform_box")
#   shinyjs::hide(id = "filter_box")
#   shinyjs::hide(id = "norm_box")
#   shinyjs::hide(id = "rollup_box")
#   shinyjs::hide(id = "PP_review_selection_box")
#   
# })
# 
# observeEvent(input$show_filters, {
#   
#   shinyjs::hide(id = "transform_box")
#   shinyjs::show(id = "filter_box")
#   shinyjs::hide(id = "norm_box")
#   shinyjs::hide(id = "rollup_box")
#   shinyjs::hide(id = "PP_review_selection_box")
# })
# 
# observeEvent(input$show_normalization, {
#   
#   shinyjs::hide(id = "transform_box")
#   shinyjs::hide(id = "filter_box")
#   shinyjs::show(id = "norm_box")
#   shinyjs::hide(id = "rollup_box")
#   shinyjs::hide(id = "PP_review_selection_box")
# })
# 
# observeEvent(input$show_protein_roll, {
#   
#   shinyjs::hide(id = "transform_box")
#   shinyjs::hide(id = "filter_box")
#   shinyjs::hide(id = "norm_box")
#   shinyjs::show(id = "rollup_box")
#   shinyjs::hide(id = "PP_review_selection_box")
# })
# 
# observeEvent(input$review_PP, {
#   
#   shinyjs::hide(id = "transform_box")
#   shinyjs::hide(id = "filter_box")
#   shinyjs::hide(id = "norm_box")
#   shinyjs::hide(id = "rollup_box")
#   shinyjs::show(id = "PP_review_selection_box")
# })
# 
# observeEvent(input$complete_transform, ignoreInit = T, {
#   
#   if(!is.null(input$complete_transform) && input$complete_transform > 0){
#     enable("show_filters")
#     disable("show_normalization")
#     disable("show_protein_roll")
#     disable("review_PP")
#   }
#   
#   print("doneem_select")
#   
#   shinyalert(title = "Success!", "Continue to next page or review results?",
#              showCancelButton = T, closeOnEsc = F, 
#              confirmButtonText = "Continue",
#              cancelButtonText = "Review results",
#              callbackR = function(value){
#                if(value){
#                  shinyjs::hide(id = "transform_box")
#                  shinyjs::show(id = "filter_box")
#                  shinyjs::hide(id = "norm_box")
#                  shinyjs::hide(id = "rollup_box")
#                  shinyjs::hide(id = "PP_review_selection_box")
#                }
#              })
#   
#   updateProgressBar(session, "transform_done", value = 100)
#   updateProgressBar(session, "filters_done", value = 0)
#   updateProgressBar(session, "norm_done", value = 0)
#   updateProgressBar(session, "rollup_done", value = 0)
#   
# })
# 
# observeEvent(input$complete_filters, ignoreInit = T, {
#   
#   if(!is.null(input$complete_filters) && input$complete_filters > 0){
#     enable("show_normalization")
#     disable("show_protein_roll")
#     disable("review_PP")
#   }
#   
#   
#   shinyalert(title = "Success!", "Continue to next page or review results?",
#              showCancelButton = T, closeOnEsc = F, 
#              confirmButtonText = "Continue",
#              cancelButtonText = "Review results",
#              callbackR = function(value){
#                if(value){
#                  shinyjs::hide(id = "transform_box")
#                  shinyjs::hide(id = "filter_box")
#                  shinyjs::show(id = "norm_box")
#                  shinyjs::hide(id = "rollup_box")
#                  shinyjs::hide(id = "PP_review_selection_box")
#                }
#              })
#   
#   updateProgressBar(session, "filters_done", value = 100)
#   updateProgressBar(session, "norm_done", value = 0)
#   updateProgressBar(session, "rollup_done", value = 0)
#   
# })
# 
# observeEvent(input$complete_norm, ignoreInit = T, {
#   
#   if(!is.null(input$complete_norm) && input$complete_norm > 0){
#     enable("show_protein_roll")
#     disable("review_PP")
#   }
#   
#   
#   shinyalert(title = "Success!", "Continue to next page or review results?",
#              showCancelButton = T, closeOnEsc = F, 
#              confirmButtonText = "Continue",
#              cancelButtonText = "Review results",
#              callbackR = function(value){
#                if(value){
#                  shinyjs::hide(id = "transform_box")
#                  shinyjs::hide(id = "filter_box")
#                  shinyjs::hide(id = "norm_box")
#                  shinyjs::show(id = "rollup_box")
#                  shinyjs::hide(id = "PP_review_selection_box")
#                }
#              })
#   
#   updateProgressBar(session, "norm_done", value = 100)
#   updateProgressBar(session, "rollup_done", value = 0)
#   
# })
# 
# observeEvent(input$complete_rollup, ignoreInit = T, {
#   
#   if(!is.null(input$complete_rollup) && input$complete_rollup > 0){
#     enable("review_PP")
#   }
#   
#   
#   shinyalert(title = "Success!", "Continue to next page or review results?",
#              showCancelButton = T, closeOnEsc = F, 
#              confirmButtonText = "Continue",
#              cancelButtonText = "Review results",
#              callbackR = function(value){
#                if(value){
#                  shinyjs::hide(id = "transform_box")
#                  shinyjs::hide(id = "filter_box")
#                  shinyjs::hide(id = "norm_box")
#                  shinyjs::hide(id = "rollup_box")
#                  shinyjs::show(id = "PP_review_selection_box")
#                }
#              })
#   
#   updateProgressBar(session, "rollup_done", value = 100)
#   
# })
