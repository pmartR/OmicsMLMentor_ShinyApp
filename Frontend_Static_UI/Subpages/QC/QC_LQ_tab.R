

LQ_mols <- function(){
  
  div(
  #   "Low Occurance Molecules",
    
    column(
      4,
      collapseBoxGroup(
        id = "QC_LQ_sidebar",
        # biomolecule filters
        
        collapseBox(
          collapsed = F,
          title = "Filter Out Biomolecules",
              br(),
              
              "By default, biomolecules observed only once across all samples ",
              "are removed, as these rarely have meaningful contributions to models.",
              " Remaining biomolecules are depicted as the dashed line in the plot on the right.",
          
              uiOutput("QC_LQ_Advanced_UI")
            
        )
        
      ), # ,# parent collapse
      actionButton("LQ_done", "Confirm Selections")
    ),
    
    column(
      8,
      collapseBoxGroup(
        id = "QC_LQ_plots",
        collapseBox(
          "Visualize Remaining Biomolecules",
          value = "single_obs_plots",
          collapsed = F,
          plotlyOutput("QC_single_mol_plot")
        )
      )
    ) # column 8
    
    )
  
}
