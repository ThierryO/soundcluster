#' Cluster sounds
#'
#' @param x a soundPyramide object
#' @param n_class the number of classes
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom kohonen somgrid som
cluster <- function(x, n_class = 49) {
  assert_that(
    inherits(x, "soundPyramid"),
    is.count(n_class)
  )
  grid_dim <- pmin(
    floor(sqrt(n_class)),
    2 ^ floor(log(nrow(x@Pyramid) / 10, base = 4))
  )
  sg <- somgrid(
    xdim = grid_dim, ydim = grid_dim,
    topo = "rectangular", neighbourhood.fct = "bubble",
    toroidal = FALSE
  )
  new(
    "soundCluster",
    Network = som(x@Pyramid, grid = sg, rlen = 100, mode = "pbatch"),
    Scaling = x@Scaling,
    Pulse = x@Pulse,
    Recording = x@Recording,
    Spectrogram = x@Spectrogram
  )
}
