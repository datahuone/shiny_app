#' Title
#'
#' @param alkuTime
#' @param loppuTime
#'
#' @return
#' @export
#'
#' @examples
lataa_tuotanto_ja_kulutus_fingridistä <- function(alkuTime = Sys.time()-lubridate::weeks(1),
                                                  loppuTime = Sys.time()){

  lataa_aikasarja_fingrid("reaali kokonaiskulutus", alku = loppuTime, loppu = alkuTime) %>%
    rename(kulutus = value) %>%
    left_join(
      lataa_aikasarja_fingrid("reaali kokonaistuotanto") %>%
        rename(tuotanto = value)
    )
}
