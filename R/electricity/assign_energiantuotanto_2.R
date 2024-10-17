#' Title
#'
#' @param time
#' @param nimi_energialahde
#'
#' @return
#' @export
#'
#' @examples
assign_energiantuotanto_2 <- function(time, nimi_energialahde) {
  print(nimi_energialahde)
  assign(paste0("energiantuotanto_", gsub("reaali ", "", nimi_energialahde)), lataa_aikasarja_fingrid(nimi_energialahde, alku = as.POSIXct(time) - lubridate::weeks(5),
                                                                                                      loppu = as.POSIXct(time)) %>%
           arrange(desc(time)) %>%
           slice(which(row_number() %% 20 == 1)) %>%
           mutate(time = lubridate::ymd_hms(time)) %>%
           mutate(time = lubridate::floor_date(time, unit = "hours")))

}
