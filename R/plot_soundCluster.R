#' plot a soundCluster object
#'
#' Displays the reconstructed average sound for each node
#' @param x a soundCluster object
#' @param y currently ignored
#' @param ... currently ignored
#' @method plot soundCluster
#' @export
#' @importFrom utils browseURL
#' @importFrom grDevices pdf dev.off
#' @importFrom graphics par plot
#' @importMethodsFrom raster plot
plot.soundCluster <- function(x, y, ...) {
  validObject(x)

  counts <- table(x@Network$unit.classif)
  titles <- sprintf("cluster %s: n = %i", names(counts), counts)
  z <- reconstruct(x)
  target <- tempfile(fileext = ".pdf")
  pdf(
    target,
    width = 1.5 * x@Network$grid$xdim,
    height = 1.5 * x@Network$grid$ydim
  )
  par(mfcol = c(x@Network$grid$ydim, x@Network$grid$xdim), mar = c(2, 2, 1, 0))
  mapply(plot, x = z, main = titles, MoreArgs = list(legend = FALSE))
  dev.off()
  if (interactive()) {
    browseURL(target)
  }
  return(target)
}
