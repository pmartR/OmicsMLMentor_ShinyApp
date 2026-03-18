

## Examples
user_level_text <- c(
  HTML("I'm new to statistical learning models! Please show all guidance and remove advanced features."),
  HTML("I have run a few statistical learning models, but I'm not an expert yet! Please show all guidance and guided features."),
  HTML("I'm a statistical learning expert and don't need guidance! Please give me advanced features.")
)

files_user_level <- list.files("./www/User_level")
files_user_level <- files_user_level[c(1,3,2)]

choiceValues_ul <- list(
  "beginner",
  "familiar",
  "expert"
)

choiceNames_ul <- pmap(list(user_level_text, files_user_level), 
                       function(text, p1){
                         
                         div(id="panew",
                             wellPanel(
                               HTML(text),
                               br(),
                               br(),
                               img(src = paste0("User_level/", p1), width = "90%", height = "90%")
                             ), tags$style(type="text/css","#panew{font-size:14px;width:405px;}"))
                       })




welcome_tab <- function(){
  
  fluidRow(
    column(
      12,
      wellPanel(
        
        strong("Welcome to Omics ML Mentor! The buttons below can help you get started:"),
        br(),
        br(),
        actionButton("launch_totoro", "Launch introduction tutorial"),
        actionButton("launch_data_requirements", "Data requirements"),
        actionButton("launch_glossary_models", "Supported models and methods"),
        actionButton("launch_app_info", "Current version and patch notes"),
        
        br()
        
      ),
      br()
    ), # column 4
    column(
      12,
      collapseBoxGroup(
        id = "welcome_collapse", multiple = FALSE,
        collapseBox("Select user experience",
                    value = "user_level",
                    collapsed = F,
                    
                    fluidRow(
                      column(12,
                             radioButtons(
                               "user_level_pick",
                               label = "",
                               choiceNames = choiceNames_ul,
                               choiceValues = choiceValues_ul,
                               inline = T
                             )
                    ))
                    
                    
        )
      )
    ),
    
    column(12, actionButton("welcome_confirm", "Start Omics ML Mentor!"))
  ) # fluidrow
  # )
  
  
}