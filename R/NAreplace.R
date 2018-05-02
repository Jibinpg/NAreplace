#' @title Replace NA with previous value
#'
#' @description This package helps to replace missing values in a vector with previous value
#'
#' @examples NAreplace(c(NA, 1, 2, NA, NA, 3, NA, NA, 4, NA))
#'
#' @return NULL
#'
#' @export NAreplace
#'
#' @param x Input vector
#'
NAreplace <-function(x) {
  i <- cumprod(is.na(x))
  x[!!i] <- x[which.min(i)]
  if (length(x) > 0L) {
    non.na.idx <- which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
}
