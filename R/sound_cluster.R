#' Cluster sounds
#'
#' @param x a soundPyramide object
#' @param grid_dim the dimensions of the clustering grid
#' @param ... extra arugments
#' @export
sound_cluster <- function(x, grid_dim = c(8, 10), ...) {
  UseMethod("sound_cluster", x)
}

#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom kohonen somgrid supersom
sound_cluster.soundPyramid <- function(x, grid_dim = c(8, 10), ...) {
  assert_that(
    length(grid_dim) == 2,
    is.count(grid_dim[1]),
    is.count(grid_dim[2])
  )
  if (prod(grid_dim) > nrow(x@Pyramid) / 10) {
    stop("Only ", nrow(x@Pyramid), " pulses. Use a smaller grid")
  }
  sg <- somgrid(
    xdim = grid_dim[1], ydim = grid_dim[2],
    topo = "rectangular", neighbourhood.fct = "bubble",
    toroidal = FALSE
  )
  new(
    "soundCluster",
    Network = supersom(
      data = list(x@PulseMeta, x@Pyramid),
      grid = sg,
      rlen = 100,
      user.weights = c(10, 1),
      normalizeDataLayers = FALSE,
      mode = "pbatch"
    ),
    Scaling = x@Scaling
  )
}

#' @export
sound_cluster.soundDatabase <- function(x, grid_dim = c(8, 10), ...) {
  validObject(x)

  message("Reading data from database")
  pyramids <- sound_pyramid(x, ...)

  message("Clustering")
  cluster <- sound_cluster(pyramids, grid_dim = grid_dim)

  message("Storing cluster")

  dbWriteTable(
    x@Connection,
    name = "model",
    data.frame(
      grid_x = cluster@Network$grid$xdim,
      grid_y = cluster@Network$grid$ydim
    ),
    append = TRUE
  )
  model_id <- dbGetQuery(x@Connection, "SELECT max(id) AS id FROM model")$id

  dbWriteTable(
    x@Connection,
    "staging_variable",
    data.frame(
      id = NA_integer_,
      name = rownames(cluster@Scaling),
      stringsAsFactors = FALSE
    )
  )
  res <- dbSendQuery(
    x@Connection,
    "INSERT OR IGNORE INTO model_variable (name)
    SELECT name FROM staging_variable"
  )
  dbClearResult(res)
  model_variable <- dbGetQuery(
    x@Connection,
    "SELECT mv.id AS variable, mv.name
    FROM model_variable AS mv
    INNER JOIN staging_variable AS sv ON mv.name = sv.name"
  )
  dbRemoveTable(x@Connection, "staging_variable")
  rownames(model_variable) <- model_variable$name

  scaling <- as.data.frame(cluster@Scaling)
  scaling$name <- rownames(cluster@Scaling)
  scaling <- merge(scaling, model_variable, by = "name")
  scaling$name <- NULL
  scaling$model <- model_id
  dbWriteTable(x@Connection, "scaling", scaling, append = TRUE)

  dbWriteTable(
    x@Connection,
    name = "layer",
    data.frame(
      model = model_id,
      weight = cluster@Network$user.weights
    ),
    append = TRUE
  )
  layer_id <- dbGetQuery(
    x@Connection,
    sprintf(
      "SELECT id FROM layer WHERE model = %s",
      dbQuoteLiteral(x@Connection, model_id)
    )
  )$id

  node_id <- as.integer(gsub("V", "", rownames(cluster@Network$codes[[1]])))
  for (i in seq_along(layer_id)) {
    dbWriteTable(
      x@Connection,
      "node",
      data.frame(
        layer = layer_id[i],
        node = node_id,
        variable = model_variable[
          colnames(cluster@Network$codes[[i]]), "variable"
        ],
        value = as.vector(cluster@Network$codes[[i]]),
        stringsAsFactors = FALSE
      ),
      append = TRUE
    )
  }

  return(invisible(NULL))
}
