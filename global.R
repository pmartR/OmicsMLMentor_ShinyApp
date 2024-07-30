suppressPackageStartupMessages({
  
  ## R stuff
  library(pmartR)
  library(pmartRdata)
  # devtools::install_github("pmartR/pmartRdata")
  library(readr)
  library(reshape2)
  library(stringr)
  library(tidyr)
  library(V8)
  library(future)
  # devtools::install("../sloper", repos = NULL, type="source")
  library(forcats)
  library(Amelia)
  
  ## Plot/table stuff
  library(plotly)
  library(ggplot2)
  library(DT)
  library(RColorBrewer)
  library(patchwork)
  library(ggdendro)
  library(dendextend)
  
  ## Reporting
  library(yaml)
  library(devtools)
  library(markdown)
  
  ## Shiny stuff
  library(shiny)
  library(shinyjs)
  library(shinyBS)
  library(shinyWidgets)
  library(shinycssloaders)
  library(shinyalert)
  library(prompter)
  library(shinydashboardPlus)
  library(shinyFiles)
  library(shinyjqui)
  library(rintrojs)
  library(shinybusy)
  library(shiny.blueprint)
  
  ## Model stuff
  library(mixOmics)
  # BiocManager::install('mixOmics')
  library(factoextra)
  library(xgboost)
  library(kernlab)
  library(tidyclust)
  library(randomForest)
  library(parsnip)
  library(tune)
  library(yardstick)
  library(glmnetUtils)
  library(glmnet)
  library(plsmod)
  library(embed)
  
  ## Don't let these get masked by other functions
  library(dplyr)
  library(purrr)
  library(slopeR) ## Must be installed manually
})


options(renv.config.sandbox.enabled = FALSE)

## GLOBAL VARIABLES ##
MAP_ACTIVE <- ifelse(Sys.getenv("MAP_VERSION") == "1", TRUE, FALSE)
AWS <- ifelse(Sys.getenv("AWS_VERSION") == "1", TRUE, FALSE)
# AWS <- T

# Load files
file_loads <- c(
  list.files("./Helpers", recursive = T, full.names = T),
  list.files("./Frontend_Static_UI", recursive = T, full.names = T)
)

for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)

### To do: if these are only used in one or two modules, take as many outta here as possible
######## GLOBAL VALUES ##########

# Labels and input choices
# ALL_DATATYPE_NAMES <- c(
#   "Protein", 
#   "Label-free", 
#   "Isobaric", 
#   "Negative", 
#   "Positive", 
#   "NMR", 
#   "GC-MS", 
#   "RNA-seq"
#   )

ALL_DATATYPE_NAMES <- c(
  "Peptide-level Label Free" = "Label-free",
  "Peptide-level Isobaric" = "Isobaric",
  "Protein-level Label Free" = "Protein",
  "Protein-level Isobaric" = "ProteinTMT", 
  "Lipidomics-Negative" = "Negative",
  "Lipidomics-Positive" = "Positive",
  "Metabolomics-GC/LC-MS" = "GC-MS",
  "Metabolomics-NMR" = "NMR",
  "RNA-seq" = "RNA-seq" ####### whatever this should be
  )

FILTER_TAGS <- c(
  "molfilt", 
  "fdata_customfilt", 
  "edata_customfilt", 
  "cvfilt", 
  "imdanovafilt", 
  "rmdfilt", 
  "profilt"
  )

# filter name mapping
FNAME_MAP <- list(
  "molfilt" = "Molecule", "cvfilt" = "Coefficient of Variation", 
  "imdanovafilt" = "iMd-ANOVA", "rmdfilt" = "Mahalanobis Distance", 
  "profilt" = "Proteomics", "fdata_customfilt" = "Custom (Sample)", 
  "edata_customfilt" = "Custom (Biomolecule)", "totalCountFilt" = "Total Count",
  "NZfilt" = "Non-zero", "Libfilt" = "Library size"
)

TABS_CHANGE_STATE <- c(
  "Upload", "Quality Control", "Model Set-Up",
  "Pre-Processing", "Run Model", "Download"
)

# models <- names(algo_rules)

models_long_name <- c(
  `Linear support vector machine` = "lsvm",
  `Polynomial support vector machine` = "psvm",
  `Radial basis support vector machine` = "rsvm",
  `Multinomial regression` = "multi",
  `Multinomial regression with LASSO` = "multilasso",
  `Logistic regression` = "logistic",
  `Logistic regression with LASSO` = "loglasso",
  `Random forest` = "rf",
  `K-means clustering` = "kmeans",
  `Hierarchical clustering` = "hclust",
  `Principal Components Analysis` = "pca",
  `Probabilistic Principal Components Analysis` = "ppca",
  `Uniform Manifold Approximation and Projection (UMAP)` = "umap",
  `Gradient boosted tree` = "gbtree",
  `Partial least squares` = "pls",
  `K-nearest neighbors` = "knn",
  `Linear Regression` = "lr",
  `Linear Discriminant Analysis` = "lda",
  `Quadratic Discriminant Analysis` = "qda",
  `Naive Bayes Classifier` = "nb"
)

sup_designation <- 
  map_lgl(models_long_name, 
          function(x){
            if(is.null(algo_rules[[x]]$hard$supervised)){
              F
            } else algo_rules[[x]]$hard$supervised
          }
  )

models_supervised <- models_long_name[sup_designation]

models_unsupervised <- models_long_name[!sup_designation]

#' #'@details Statistical integration methods and corresponding tooltip
#' VALID_STATSINTEG_ANALYSES = list(
#'   "SPLS" = "spls",
#'   "Correlation-based" = "correlation"
#' )

#'@details Used for contitional UI's, for example n-way analyses usually use 
#' multi-pickers vs two single pickers for two-way analyses
# TWO_WAY_ANALYSES = c("spls")
# N_WAY_ANALYSES = c("correlation")
# 
# STATSINTEG_ANALYSIS_INFO = list(
#   "SPLS" = gsub("\n", "",
#   "SPLS - Sparse partial least squares attempts to find a hidden
#   structure in the data that maximizes covariance between datasets.  This can
#   be used to find relationships between biomolecules that have high correlation
#   in that hidden structure."),
#   "Correlation-based" = gsub("\n", "",
#   "Investigate the within dataset and cross dataset correlation structure with
#   heatmaps, density plots, and other summary statistics.")
# )
# VALID_STATS_ANALYSES = list(
#   "iMd-ANOVA" = 'iMd_ANOVA',
#   "SPLS-DA" = "spls-da",
#   "DESeq2" = "DESeq2",
#   "limma-voom" = "voom",
#   "edgeR" = "edgeR"
#   )
# VALID_BIOINTEG_ANALYSES = list("PALLID" = 1)


NULLSELECT_ = "__nullselect__"

# choices for select inputs
# CHOICES <- list(
#   UNIPROT_CATEGORIES = list(
#     "Accession Number" = "ACC",
#     "UniProtKB Name" = "ID",
#     "UniParc" = "UPARC",
#     "Gene name" = "GENENAME",
#     "CRC64" = "CRC64"
#   ),
#   SPLS_PLOT_TYPES = list(
#     "Correlation circle" = "corcirc", 
#     "Loadings barchart" = "loadings"
#   ),
#   CORRINTEG_PLOT_TYPES = list(
#     "Correlation heatmaps" = "cheatmaps",
#     "Correlation density" = "cdensity"
#   ),
#   RMD_FILTER_CHOICES = list(
#     "Median Absolute Deviation"="MAD", 
#     "Kurtosis", 
#     "Skewness", 
#     "Correlation", 
#     "Proportion Missing" = "Proportion_Missing"
#   )
# )

### Please move to individual modules
# tooltip/popover/modal text
ttext <- list(
  ALIGN_METHOD = "Choose to either pick out sections of the column values by splitting them and choosing particular sections of the values, or use a regex search to pick out values.",
  ALIGNED_COLUMNS = "Is the sample referenced by the n-th row the same for all datasets?  If not, click here to rearrange them.  If they are, continue on to making new columns and grouping.",
  AUTO_FIND = "Auto-find columns of F-meta that have row values matching your data files column names",
  BAD_ALIGN_INPUTS = "The extraction method you specified is incomplete or did not result in any common values across all columns.",
  BAD_COMPARISONS = "The following comparisons are confounded in one or more datasets, please remove one or more of these columns from your grouping selections: ",
  BIOINTEG_WHY_FEW_CHOICES = "You will probably only see only one choice for Protein and at most two for Lipids unless you have uploaded from a MAP midpoint file.",
  BIOMOL_CUSTOMFILT_NONE_REMOVED = "The filter must remove at least one biomolecule, please change your search term/search criteria.",
  BIOMOL_CUSTOMFILT_ALL_REMOVED = "The filter must keep at least one biomolecule, please change your search term/search criteria.",
  BPQUANT_FILTER_BAD_ANOVA = "When you run bpquant below, your data will first have biomolecules that do not have at least 3 observations for ALL comparisons removed.",
  COVARIATE_TYPE_INFO = "Suppose your covariate has values [1,1,2,2,3,3].  By default, it is treated as a factor variable, but you may want to specify it is numeric.",
  COVARIATES_NO_EFFECT = "Keeping samples that have missing values for a covariate will cause that covariate to be ignored in most subsequent analyses.",
  CV_THRESH_TOO_LOW = "Maximum CV must be between 1 and the maximum data CV: ",
  DATA_IMPORTED = "Your data has been uploaded from a SLOPE project, continue by completing the remainder of the tab: ",
  DOWNLOAD_OPTS_DISABLED = "Currently selected parameters are the same as the ones currently stored for this plot",
  GTEST_TOO_FEW = "All groups must have at least 3 nonmissing observations for at least one biomolecule to perform the G-test, the following groups did not: ",
  GOALS_NOT_COMPLETE = "Data source and at least two datatypes must be specified and confirmed.  (Separate lipids count as one)",
  HANDLE_MISALIGNED_STATS_INFO = "&quot;Remove&quot; will remove these samples from the dataset, they will not be able to be used in other parts of the app.  &quot;Track&quot; will keep the samples, but they will be prevented from being used in analyses that require identical comparisons across groups.",
  IMDANOVA_BOTH_NA = "Must choose at least one minimum observed threshold (gtest or anova)",
  IMDANOVA_NONE_FILTERED = "The chosen minimum values for the iMd filter do not remove any biomolecules, please revise.",
  IMDANOVA_SINGLE_GROUP = "iMd-Anova filter requires at least two non-singleton groups.",
  IMDANOVA_VALUES_OUT_OF_RANGE = "The specified minimum number of observations per group must be less than or equal to the minimum group size: ", 
  IMDANOVA_VALUES_LIMITS = "<br/> Minimum g-test must be >= 3.  Minimum ANOVA must be >=2.",
  INTEGRATION_LOW_SAMPLES = "Warning: Though SPLS claims to be able to handle low numbers of samples, our independent experiments show instability under 30 samples, proceed with caution.",
  ISOBARIC_NO_GTEST_COMBINED = "Combined and g-test options are disabled for isobaric labeled peptide data",
  NEWCOL_NO_SPLIT_CHOSEN = "In the field above this one, pick a value that each entry in the chosen column will be split by, then choose which parts of the result to keep",
  NO_BOGEY_COVARIATES = "No samples were removed due exclusively to a missing covariate.",
  PAIRING_INFO = "If your samples are paired, three pieces of information are needed:  The column specifying which pairs of samples go together, the column specifying which group or side of the pairing each sample is in, and which group is to be subtracted from the other.",
  REARR_NOT_UNIQUE = "The columns resulting from rearranging your original values do not contain unique values",
  REARRANGE_FMETA = "Replace your F-meta file with another that uses the extracted, rearranged values",
  REGRESSION_VS_CANONICAL = "Regression performs the familiar task of predicting Y from X.  Canonical mode considers a more symmetric relationship between X and Y.",
  RMDFILT_INSUF_EDATA = "Must have at least 100 biomolecules to apply a rMd filter, current data has ",
  RMDFILT_NO_VARIANCE = "Low variability metric detected across samples. Please re-add the filter without the folowing metric to continue: ",
  RMD_WARN_PROP_MISSING = "We advise against using proportion missing as a metric in datasets that typically have a low proportion of missing data.",
  SELECT_MAIN_EFFECT = "Please select at least one main effect.",
  SELECT_ALL_FMETA_IDS = "Please select an ID column for all datasets",
  TOO_FEW_NONSINGLETONS = "The following main effects or interaction effects had less than two non-singleton groups: ",
  TOO_MANY_SAMPS_RMV = "Too many samples were removed due to mismatched elements (require at least 2), make sure your Fmeta file is consistent with samples",
  UPLOAD_FMETA = "Either upload an fmeta file from disk, or create it from your data file column names",
  REFERENCE_DISABLED_ROW = "Disabled entries contain missing values in some samples. These might be due to NAs generated in log transformation or replacement of values less than or equal to zero.",
  REFERENCE_DISABLED_COL = "Disabled entries are non-numeric.",
  BPQUANT = "This option is only available if at least 3 different levels of main effects exist and IMD/ANOVA peptide statistics are computed",
  BPQUANT_APPLY = "Must compute isoforms before use.",
  MOL_THRESH_INVALID = "Molecule filter must be between ",
  NORM_BUTTON_DISABLED = "Select all applicable fields for normalization.",
  STATS_BUTTON_DISABLED = "Requires group comparison and test method selections to apply."
)
