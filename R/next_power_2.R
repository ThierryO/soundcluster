#' Calculate the next power of 2
#'
#' Handles numeric of integer vector. `x` values smaller or equal than 1 return `NA`
#' @param x the number of which to calculate the next power of 2
#' @export
#' @examples
#' next_power_2(3)
#' next_power_2(12345L)
#' next_power_2(c(7, 9))
next_power_2 <- function(x){
  assert_that(
    inherits(x, c("numeric", "integer"))
  )
  x[x <= 1] <- NA
  2 ^ ceiling(log2(x))
}
