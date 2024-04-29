#'@description  Reactive variables containing objects pulled from or relating to
#'storage on minio.
#'

# Create a reactive value to hold MAP-specific objects
MapConnect <-
  reactiveValues(
    MapConnect = NULL,
    Project = NULL,
    Midpoint = NULL,
    DataTypes = NULL,
    pmart_project = list(),
    ProjectBuiltFrom = "None",
    query = NULL,
    data = NULL
  )
