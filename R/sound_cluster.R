#' Cluster sounds
#'
#' @param x a soundPyramide object
#' @param grid_dim the dimensions of the clustering grid
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom kohonen somgrid supersom
sound_cluster <- function(x, grid_dim = c(8, 10)) {
  assert_that(
    inherits(x, "soundPyramid"),
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
