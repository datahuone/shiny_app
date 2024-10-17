assign_energiantuotanto <- function(nimi_energialahde, startTime, endTime) {
  print(nimi_energialahde)
  assign(paste0("energiantuotanto_", gsub("reaali ", "", nimi_energialahde)), lataa_aikasarja_fingrid(nimi_energialahde, alku = as.POSIXct(startTime),
                                                                                                      loppu = as.POSIXct(endTime)) %>%
           arrange(desc(time)) %>%
           slice(which(row_number() %% 20 == 1)) %>%
           mutate(time = lubridate::ymd_hms(time)) %>%
           mutate(time = lubridate::floor_date(time, unit = "hours")))

}
