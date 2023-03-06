## Data storage module

plots <- reactiveValues(plot_table = data.frame("Select a plot" = character(0), 
                                                "Download?" = character(0), 
                                                check.names = F, stringsAsFactors = F))
plot_save_options <- reactiveValues()

# tables of results and other things (intentionally have plot_table in plots$... reactive list)
# +1000 points for variable called tables_table, which is accessed by calling tables$tables_table
tables <- reactiveValues(tables_table = data.frame('Table' = c('val1', 'val2'), 'Download?' = dt_checkmark, stringsAsFactors = FALSE, check.names = FALSE))
