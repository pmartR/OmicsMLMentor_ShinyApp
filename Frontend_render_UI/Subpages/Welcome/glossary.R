
img_get <- function(abbr) switch(
  abbr,
  lsvm = "model_images/linear_svm.png",
  psvm = "model_images/poly_svm.png",
  rsvm = "model_images/radial_svm.png",
  multi = "model_images/multilog.png",
  logistic = "model_images/log.png",
  rf = "model_images/rf.png"
)

svm_text <- paste0(
  "Support vector machines (SVM) are designed to find a boundary to seperate two known experimental conditions ",
  "in high dimentional data, where the boundary can be boiled down to an equation. ",
  "This can be useful in determining conditions such as disease state,", 
  " but the model cannot be used for more than one conditon, such as a ",
  "set of possible diseases. \n\n"
)

## should I ask chatGPT to write these?
text_get <- function(abbr) switch(
  abbr,
  lsvm = paste0(svm_text, 
                "Linear SVM search for a flat plane that seperates the two experimental conditions best."),
  psvm = paste0(svm_text, 
                "Polynomial SVM search for a curved plane that seperates the two experimental conditions best."),
  rsvm = paste0(svm_text, 
                "Radial SVM search for a circle-like plane that seperates the two experimental conditions best."),
  multi = paste0("Multinomial logistic models are used to predict the ",
                 "probability that a sample belongs to one or more specific experimental conditions.",
                 "An example would be determining if a patient is healthy or",
                 " afflicted with one of several possible disease states."),
  logistic = paste0("Logistic models are used in specific circumstances to predict the ",
                    "probability that a sample belongs to a one specific group or not.",
                    "This can be useful in determining conditions such as a disease state,",
                    " but the model cannot be used for more than one conditon, such as a ",
                    "set of possible diseases."),
  rf = paste0("Random forest models are based on decision trees, where each predictor",
              " contributes to determining a response category or number.",
              " The consensus of multiple decision trees is the outcome of the random forest model."
              )
)

tags_get <- function(abbr) switch(
  abbr,
  lsvm = paste0(svm_text, 
                "Linear SVM search for a flat plane that seperates the two experimental conditions best."),
  psvm = paste0(svm_text, 
                "Polynomial SVM search for a curved plane that seperates the two experimental conditions best."),
  rsvm = paste0(svm_text, 
                "Radial SVM search for a circle-like plane that seperates the two experimental conditions best."),
  multi = paste0("Multinomial logistic models are used to predict the ",
                 "probability that a sample belongs to one or more specific group.",
                 "An example would be determining if a patient is healthy or",
                 " afflicted with one of several possible disease states."),
  logistic = paste0("Logistic models are used in specific circumstances to predict the ",
                    "probability that a sample belongs to a one specific group or not.",
                    "This can be useful in determining conditions such as a disease state,",
                    " but the model cannot be used for more than one conditon, such as a ",
                    "set of possible diseases."),
  rf = paste0("Random forest models are used to determine either a categorical ",
  "experimental condition, such as a disease state, ")
)

map(names(models_long_name), function(x){
  abbr <- models_long_name[[x]]
  
  output[[paste0("EM_", abbr)]] <- renderUI({
    
    
    # if(input$pick_model_EM == abbr){
    #   status <- "success"
    #   label <- boxLabel("Selected Model", "primary", style = "default")
    # } else if(input$pick_model_EM != abbr) {
    #   status <- "danger"
    #   label <- NULL
    # } else {
      status <- "primary"
      label <- NULL
    # }
    
    box(
      title = gsub("support vector machine \\(SVM\\)", "SVM", x),
      width = 4,
      height = "300px",
      status = status,
      # background = background,
      # solidHeader = solidHeader,
      label = label,
      
      div(
        style = "height: 100%",
        flipBox(
          id = paste0("expert_mentor_", abbr),
          width = 12,
          front = div(
            
            fluidRow(
              column(12,
                     
                img(src = img_get(abbr), width = "95%", height = "250px")
              ))),
          back = div(
            style = "height: 100%",
            fluidRow(
              style = "height: 100%",
              column(
                12,
                style = "height: 100%",
                div(
                  style = "height: 100%; overflow-y: auto",
                  text_get(abbr)
                )
              )))
        )
      )
      )
  })

})


