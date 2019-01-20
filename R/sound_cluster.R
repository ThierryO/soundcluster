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
#' @importFrom methods validObject
#' @importFrom pool poolCheckout poolReturn
#' @importFrom RSQLite dbWriteTable dbGetQuery dbSendQuery dbClearResult dbRemoveTable
sound_cluster.soundDatabase <- function(x, grid_dim = c(8, 10), ...) {
  validObject(x)
  connection <- poolCheckout(x@Connection)

  message("Reading data from database")
  pyramids <- sound_pyramid(x, ...)

  message("Clustering")
  cluster <- sound_cluster(pyramids, grid_dim = grid_dim)

  message("Storing cluster")

  dbWriteTable(
    connection,
    name = "model",
    data.frame(
      grid_x = cluster@Network$grid$xdim,
      grid_y = cluster@Network$grid$ydim
    ),
    append = TRUE
  )
  model_id <- dbGetQuery(connection, "SELECT max(id) AS id FROM model")$id

  dbWriteTable(
    connection,
    "staging_variable",
    data.frame(
      name = rownames(cluster@Scaling),
      stringsAsFactors = FALSE
    )
  )
  res <- dbSendQuery(
    connection,
    "INSERT OR IGNORE INTO model_variable (name)
    SELECT name FROM staging_variable"
  )
  dbClearResult(res)
  model_variable <- dbGetQuery(
    connection,
    "SELECT mv.id AS variable, mv.name
    FROM model_variable AS mv
    INNER JOIN staging_variable AS sv ON mv.name = sv.name"
  )
  dbRemoveTable(connection, "staging_variable")
  rownames(model_variable) <- model_variable$name

  dbWriteTable(
    connection,
    "staging_model_pulse",
    data.frame(
      model = model_id,
      fingerprint = rownames(cluster@Network$data[[1]]),
      stringsAsFactors = FALSE
    )
  )
  res <- dbSendQuery(
    connection,
    "INSERT OR IGNORE INTO model_pulse (pulse, model)
    SELECT p.id AS pulse, smp.model
    FROM pulse AS p
    INNER JOIN staging_model_pulse AS smp ON p.fingerprint = smp.fingerprint
    ORDER BY p.id"
  )
  dbClearResult(res)
  dbRemoveTable(connection, "staging_model_pulse")

  scaling <- as.data.frame(cluster@Scaling)
  scaling$name <- rownames(cluster@Scaling)
  scaling <- merge(scaling, model_variable, by = "name")
  scaling$name <- NULL
  scaling$model <- model_id
  dbWriteTable(connection, "scaling", scaling, append = TRUE)

  dbWriteTable(
    connection,
    name = "layer",
    data.frame(
      model = model_id,
      weight = cluster@Network$user.weights
    ),
    append = TRUE
  )
  layer_id <- dbGetQuery(
    connection,
    sprintf(
      "SELECT id FROM layer WHERE model = %s",
      dbQuoteLiteral(connection, model_id)
    )
  )$id

  node_id <- as.integer(gsub("V", "", rownames(cluster@Network$codes[[1]])))
  dbWriteTable(
    connection,
    "staging_node",
    data.frame(
      model = model_id,
      x = (node_id - 1) %/% cluster@Network$grid$xdim,
      y = (node_id - 1) %% cluster@Network$grid$xdim
    )
  )
  res <- dbSendQuery(
    connection,
    "INSERT OR IGNORE INTO node (model, x, y)
    SELECT model, x, y
    FROM staging_node"
  )
  dbClearResult(res)
  dbRemoveTable(connection, "staging_node")
  node_id <- dbGetQuery(
    connection,
    sprintf(
      "SELECT id FROM node WHERE model = %s ORDER BY x, y",
      dbQuoteLiteral(connection, model_id)
    )
  )$id

  for (i in seq_along(layer_id)) {
    dbWriteTable(
      connection,
      "node_value",
      data.frame(
        layer = layer_id[i],
        node = rep(node_id, ncol(cluster@Network$codes[[i]])),
        variable = rep(
          model_variable[
            colnames(cluster@Network$codes[[i]]), "variable"
          ],
          each = length(node_id)
        ),
        value = as.vector(cluster@Network$codes[[i]])
      ),
      append = TRUE
    )
  }

  dbWriteTable(
    connection,
    "staging_prediction",
    data.frame(
      fingerprint = rownames(cluster@Network$data[[1]]),
      node = node_id[cluster@Network$unit.classif],
      distance = cluster@Network$distances,
      stringsAsFactors = FALSE
    )
  )
  res <- dbSendQuery(
    connection,
    "INSERT OR IGNORE INTO prediction (pulse, node, distance)
    SELECT p.id AS pulse, sp.node, sp.distance
    FROM staging_prediction AS sp
    INNER JOIN pulse AS p ON sp.fingerprint = p.fingerprint
    ORDER BY p.id"
  )
  dbClearResult(res)
  dbRemoveTable(connection, "staging_prediction")

  poolReturn(connection)

  return(invisible(NULL))
}
