
#' Tuhaterotin
#'
#' Formatoi numerot oikeaoppisesti. Käyttämällä välilyöntiä tuhaerottimena
#' sekä pilkkua pisteen sijaan desimaalimerkkinä
#'
#' @param x numero
#' @return str
#'
tuhaterotin <- function(x){
  format(x, big.mark= " ",
         decimal.mark = ",",
         scientific = FALSE)
}
