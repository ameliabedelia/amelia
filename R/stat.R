#' Standard error
#'
#' This function calculates the standard error of all the values in `x`.
#' @keywords standard error
#'
#' @param x a numeric vector
#' @param na.rm logical. Whether missing values should be removed.
#' @export
#' @examples
#'
#'
#'
se <- function(x, na.rm = TRUE){
     sd(x, na.rm = na.rm)/sqrt(length(x[na.omit(x)]))
}
