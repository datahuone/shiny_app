#' Title
#'
#' @param kansion_nimi
#' @param kuukaudet
#'
#' @return
#' @export
#'
#' @examples
lataa_data <- function(kansion_nimi, kuukaudet){

  #boxplotit <- parallel::parLapply(
  # lataa_data_cluster, kuukaudet, function(x) feather::read_feather(paste0("data/",kansion_nimi,"/data",x,".feather")))

  boxplotit <- lapply(
    kuukaudet, function(x) feather::read_feather(paste0("data/",kansion_nimi,"/data",x,".feather")))

  names(boxplotit) <- kuukaudet

  return(boxplotit)
}
