#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
checkUpdateCondition <- function(x) {
  # This function updates the data specified if an hour has lapsed since the last update

  if (x < (Sys.time() - lubridate::hours(2))) {
    return(TRUE) #This should be TRUE.
  } else {
    return(FALSE) #This should be FALSE. Set it TRUE for testing
  }

}
