#' Read the soundPulse objects from rds files
#'
#' @inheritParams base::list.files
#' @importFrom assertthat assert_that is.string is.flag
#' @export
read_soundpulse <- function(path, recursive = TRUE) {
  assert_that(
    is.string(path),
    is.flag(recursive)
  )
  rds <- list.files(
    path = path, pattern = "\\.rds$", recursive = recursive, full.names = TRUE
  )
  sp <- lapply(rds, readRDS)
  do.call(rbind, sp)
}
