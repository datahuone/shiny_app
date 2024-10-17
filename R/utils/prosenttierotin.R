#' Prosenttierotin
#'
#' Formatoi prosenttiluvut oikeaoppisesti. Lisäämällä välin luvun sekä % väliin.
#'
#' @param x Luku 0,1 väliltä
#' @return str

prosenttierotin <- function(x){
  paste0(tuhaterotin(x*100), " %")
}
