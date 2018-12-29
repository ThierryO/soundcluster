#' Calculate the Variance Inflation Factor of a data.frame
#'
#' @param x a data.frame with numeric variables
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stats as.formula lm summary.lm
vif <- function(x) {
  assert_that(inherits(x, "data.frame"))
  if (any(!sapply(x, inherits, "numeric"))) {
    stop("all variables must be numeric")
  }

  vif_score <- sapply(
    colnames(x),
    function(variable) {
      model <- lm(as.formula(paste(variable, " ~ .")), data = x)
      1 / (1 - summary(model)$r.squared)
    }
  )
  sort(vif_score, decreasing = TRUE)
}
