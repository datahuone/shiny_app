#' Title
#'
#' @param column
#' @param dom
#' @param n
#'
#' @return
#' @export
#'
#' @examples
check_dom <- function(column, dom = 0.75, n = 1) {
  # tarkistaa dominanssin, perus skenaario on (1,75)
  dom > (max(column, na.rm = T) / (1 + sum(column, na.rm = T)))
}
