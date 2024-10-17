#' Formatoikuukausien nimi kuvaajiin
#'
#' Lisää
formatoi_kuukaudet_plot <- function(date){
  return(paste0(kuukaudet_suom(date), format(date, " %y")))
}
