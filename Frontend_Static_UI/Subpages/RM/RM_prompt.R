
Prompt_RM_UI <- function() {
  div(
           fluidRow(
             column(
               12,
               collapseBoxGroup(
                 id = "rm_prompt_collapse_center",
                 collapseBox(
                   "Specifiy Model Options",
                   value = "ag_choices",
                   collapsed = F,
                   
                   br(),
                   
                   strong("What about your model is more important to you?"),
                   # textOutput("warn_few_samps_train"),
                   br(),
                   br(),
                   uiOutput("rm_prompt_train_UI"),
                   
                   # tags$head(tags$style("#warn_few_samps_train{color: red;
                   #               font-size: 20px;
                   #               font-style: italic;
                   #               }" )
                   # ),
                   
                   br(),
                   
                   conditionalPanel(
                     "typeof input.rm_prompts_train !== 'undefined' && input.rm_prompts_train.length > 0", {
                     
                     div(
                     strong("What settings would you like to use for your model?"),
                     br(),
                     textOutput("warn_few_samps_settings"),
                     br(),
                     uiOutput("rm_prompt_hp_UI"),
                     tags$head(tags$style("#warn_few_samps_settings{color: red;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
                     )
                     )
                     )
                     
                   })
                   
                 )),
               
               conditionalPanel("typeof input.rm_prompts_hp !== 'undefined' && input.rm_prompts_hp.length > 0", {
               actionButton("complete_RM_prompts", "Confirm Selections")
               })
               
             )
           )
  )
  
}


Prompt_RM_UI_unsup <- function() {
  div(
           fluidRow(
             column(
               12,
               collapseBoxGroup(
                 id = "rm_prompt_collapse_center",
                 collapseBox(
                   "Specifiy Model Options",
                   value = "ag_choices",
                   collapsed = F,
                   
                   br(),
              
                       
                       div(
                         strong("What settings would you like to use for your model?"),
                         br(),
                         # textOutput("warn_few_samps_settings"),
                         # br(),
                         uiOutput("rm_prompt_hp_UI"),
                         tags$head(tags$style("#warn_few_samps_settings{color: red;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
                         )
                         )
                       )
                   
                 )),
               
               conditionalPanel("typeof input.rm_prompts_hp !== 'undefined' && input.rm_prompts_hp.length > 0", {
                 actionButton("complete_RM_prompts", "Confirm Selections")
               })
               
             )
           )
  )
  
}