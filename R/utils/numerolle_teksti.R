#' Title
#'
#' @param numero
#'
#' @return
#' @export
#'
#' @examples
numerolle_teksti <- function(numero) {

  switch(as.character(numero), #Switch tekee tässä käytännössä saman asian kuin dict (vrt. fingrid_sanakirja())
         "1" = "Yksi",
         "2" = "Kaksi",
         "3" = "Kolme",
         "4" = "Neljä",
         "5" = "Viisi",
         "6" = "Kuusi",
         "7" = "Seitsemän",
         "8" = "Kahdeksan")

}
