
#'Suomenkieliset kuukaudet
#'
#'Hakee readr-paketista suomenkielisen kuukauden nimen päivämäärälle.
#'
#' @param date
#' @return str suomenkielinen kuukausi
kuukaudet_suom <- function(date){
  locaali <- readr::date_names_lang("fi")
  gsub('.{2}$','',locaali$mon)[lubridate::month(date)]
}
