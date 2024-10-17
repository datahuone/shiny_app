#' Title
#'
#' @param data
#' @param haettava_kunta
#'
#' @return
#' @export
#'
#' @examples
hae_kunta <- function(data, haettava_kunta){

  data %>%
    filter(kunnan_nimi == haettava_kunta) %>%
    select(-c(kunnan_nimi, maakunta))

}
