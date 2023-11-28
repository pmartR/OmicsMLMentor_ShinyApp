
Prompt_RM_UI <- function() {
  tabPanel("Model Options",
           
           class = "collapse_page",
           fluidRow(
             column(
               12,
               collapseBoxGroup(
                 id = "rm_prompt_collapse_center",
                 collapseBox(
                   "Specifiy Model Options",
                   value = "ag_choices",
                   collapsed = F,
                   
                   strong("How would you like to use your model?"),
                   uiOutput("rm_prompt_train_UI"),
                   
                   br(),
                   
                   strong("What settings would you like to use for your model?"),
                   uiOutput("rm_prompt_hp_UI")
                   
                 )),
               
               actionButton("complete_RM_prompts", "Confirm Selections")
               
             )
           )
  )
  
}