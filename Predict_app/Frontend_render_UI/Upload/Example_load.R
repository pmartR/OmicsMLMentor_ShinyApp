
#### Loads




observeEvent(input$load_example, {
  
  reactive_dataholder$model <- readRDS("./Predict_app/example/data/Full_model_SLOPE_model_2025_01_13_13_34_31.159147.RDS")
  reactive_dataholder$e_data <- read.csv("./Predict_app/example/data/e_data_short.csv")
  reactive_dataholder$e_meta <- read.csv("./Predict_app/example/data/e_meta.csv")
  reactive_dataholder$f_data <- read.csv("./Predict_app/example/data/f_data_short.csv")
  
})

