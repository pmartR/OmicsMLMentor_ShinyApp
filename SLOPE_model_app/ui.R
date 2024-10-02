ui <- function(request) {
  #
  tagList(
    useShinyjs(),
    use_prompt(),
    useShinydashboardPlus(), ## This causes the hovers for picker input, pages
    introjsUI(),
    
    extendShinyjs(script = "./Helpers/shinyui.js", functions = c(
      "isTabdisabled", # For testing purposes
      "isIconhidden", # For testing purposes
      "disableTab", # Disables a tab
      "enableTab", # Enables a tab
      "disableBtn", # Disables a button
      "setFileInput", # Change appearance of fileInput
      "toggleTabInputs" # Toggles state of inputs on a page
    )), # Custom JS code
    
    # start navbarPage
    navbarPage(
      # need to update this to be slope_icon.png
      title = "SLOPE",
      id = "top_page",
      
      tabPanel("Welcome",welcome_tab()),
      tabPanel("Upload",upload_tab_overlord()),
      tabPanel("Pre-processing",preprocessing_tab_overlord()),
      tabPanel("Run Model", RM_tab_overlord()),
      tabPanel("Downloads", downloads_tab_overlord())
    ),
    # end Navbarpage
    actionButton("Browser","whats wrong")
  )
} # shinyUI + shinyjs tagList