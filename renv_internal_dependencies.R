#'This script is never actually run, it is for packages that need to be captured
#'in a snapshot() but we dont actually want to call library() for in the app.
#'
#'For example, processx is used by plotly::orca, but is not required by plotly
#'(and so wont be installed on a fresh restore) or loaded specifically in the 
#'app. (and so wont be captured in a snapshot)
#'

library(processx)
library(markdown)
