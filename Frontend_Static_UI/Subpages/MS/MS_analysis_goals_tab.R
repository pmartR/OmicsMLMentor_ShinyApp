

## Examples
ag_text <- c(
  "Figure out which (Biomolecules) best predict a (Sample Information column)", ## starting Q
  "Make accurate predictions",
  "Understand why predictions are made",
  "Figure out which (samples/biomolecules) look like other (samples/biomolecules)", ## starting Q
  "Understand which (samples/biomolecules) are similar",
  "Understand why (samples/biomolecules) are different"
)

files_ag <- list.files("./www/Example_ag")

p1_list <- files_ag[seq(to = length(files_ag), by = 2)]
p2_list <- files_ag[seq(2, to = length(files_ag), by = 2)]

choiceValues_ag <- list(
  "supervised",
  "accuracy",
  "variable importance",
  "unsupervised",
  "clusters",
  "variation source"
)

choiceNames_ag <- pmap(list(ag_text, p1_list, p2_list, choiceValues_ag), 
                    function(text, p1, p2, cv){
  if(cv %in% c("supervised", "unsupervised")){
    width <- 500
    height <- 500
  } else {
    width <- 250
    height <- 150
  }                   
  
  div(id="paneag",
      wellPanel(
        text,
        br(),
        br(),
        splitLayout(
          img(src = paste0("Example_ag/", p1), width = "95%", height = "95%"
              ),
          img(src = paste0("Example_ag/", p2), width = "95%", height = "95%"
              )
        )
      ), tags$style(type="text/css","#paneag{font-size:14px;width:705px;}")
  )
})


## likely div to slap multiple images together with text


analysis_goals <- function() {
  div(
           fluidRow(
             column(
               12,
               collapseBoxGroup(
                 id = "ag_collapse_center",
                 collapseBox(
                   "Specifiy Model Goals",
                   value = "ag_choices",
                   collapsed = F,
                   
                   column(12,
                   column(4, 
                          
                          conditionalPanel("input.user_level_pick == 'expert'",
                            {
                            checkboxInput("skip_ag", "I know what model I want to run.")
                          })
                          ),
                   column(4,
                     
                     uiOutput("pick_model_UI"),
                     
                     uiOutput("pick_model_group_pick_UI")
                     
                   )),
                   
                   
                   conditionalPanel("input.skip_ag == false", {
                     div(
                       strong("What would you like to understand from the data?"),
                       
                       uiOutput("ag_prompt_UI"),

                       conditionalPanel("input.ag_prompts == 'supervised'",
                          uiOutput("f_data_response_picker_UI"),
                          uiOutput("pick_model_response_type_UI")
                       ),
                       
                       conditionalPanel(
                         paste(
                           "input.ag_prompts == 'supervised'",
                           "input.f_data_response_picker != null",
                           "input.f_data_response_picker != ''",
                           "input.f_data_response_picker.length > 0",
                           "input.pick_model_response_type != null",
                           "input.pick_model_response_type != ''",
                           "input.pick_model_response_type.length > 0",
                           sep = " && "
                          ), {
                         div(
                           br(),
                           strong("What would you like to prioritize in your model?"),
                           
                           radioButtons(
                             "ag_prompts_supervised",
                             label = "",
                             choiceNames = choiceNames_ag[2:3],
                             choiceValues = choiceValues_ag[2:3],
                             inline = T,
                             selected = character(0)
                           )
                         )
                       }),
                       
                       conditionalPanel("input.ag_prompts == 'unsupervised'", {
                         div(
                         br(),
                         strong("What would you like to prioritize in your model?"),
                         
                         radioButtons(
                           "ag_prompts_unsupervised",
                           label = "",
                           choiceNames = choiceNames_ag[5:6],
                           choiceValues = choiceValues_ag[5:6],
                           inline = T,
                           selected = character(0)
                         )
                         )
                       })
                       
                     )
                   })
                   
                 )),
               
               actionButton("ag_done", "Confirm Selections")
               
               )
           )
  )
  
}
