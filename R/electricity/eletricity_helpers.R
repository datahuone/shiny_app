#' Title
#'
#' @param arvo
#'
#' @return
#' @export
#'
#' @examples
lataa_viimeisin_fingrid <- function(arvo){

  arvot <- fingrid_sanakirja()
  if(!arvo %in% names(arvot)){
    stop("Syötä oikea arvo")
  }

  variableID <- arvot[arvo]
  path <- paste0("https://api.fingrid.fi/v1/variable/",variableID,"/event/json")

  return(ota_yhteys_fingrid_api(path) %>% select(value) %>% pull())
}


#' Title
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
ota_yhteys_fingrid_api <- function(url){

  if (!exists('api_key')) {

    api_key <- feather::read_feather('fingrid_api_2.feather') %>%
      pull()

  }

  res = GET(url,
            query = list(`x-api-key` = api_key))

  result_tibble <- as_tibble(fromJSON(rawToChar(res$content)))

  print(result_tibble)

  return(result_tibble %>%
           select(-end_time) %>%
           rename(time = start_time))

}



#' Title
#'
#'  TODO: Convert to internal data
#'
#' @return
#' @export
#'
#' @examples
fingrid_sanakirja <- function(){
  return(
    c("reaali kokonaistuotanto" = 192,
      "reaali tuulivoima" = 181,
      "reaali vesivoima" = 191,
      "reaali tehoreservi" = 183,
      "reaali yhteistuotanto kaukolämpö" = 201,
      "reaali yhteistuotanto teollisuus" = 202,
      "reaali pientuotanto" = 205,
      "reaali ydinvoima" = 188,
      "reaali kokonaiskulutus" = 193,
      "reaali vienti" = 194#,
      # "reaali sähköpula" = 336
    )
  )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
lataa_kaikki <- function(){

  as_tibble(sapply(names(fingrid_sanakirja()), lataa_viimeisin_fingrid)) %>%
    mutate(name = names(fingrid_sanakirja()))

}

#' Title
#'
#' @param arvo
#' @param alku
#' @param loppu
#' @param muuta_nimi
#'
#' @return
#' @export
#'
#' @examples
lataa_aikasarja_fingrid <- function(arvo,
                                    alku = Sys.time()-lubridate::weeks(1),
                                    loppu = Sys.time(),
                                    muuta_nimi = NULL){
  print("loading data from Fingrid")

  arvot <- fingrid_sanakirja()

  if(!arvo %in% names(arvot)){
    stop("Syötä oikea arvo")
  }

  variableID <- arvot[arvo]
  alku <- format(alku, "%Y-%m-%dT%H:%M:%SZ")
  loppu <- format(loppu, "%Y-%m-%dT%H:%M:%SZ")
  path <- paste0("https://api.fingrid.fi/v1/variable/",
                 variableID,"/events/json?start_time=",alku,"&end_time=",loppu)

  return(ota_yhteys_fingrid_api(path))

}

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

#' Title
#'
#' @return
#' @export
#'
#' @examples
fetch_energialahteet <- function() {
  return(c("reaali tuulivoima",
           "reaali vesivoima",
           "reaali tehoreservi",
           "reaali yhteistuotanto kaukolämpö",
           "reaali yhteistuotanto teollisuus",
           "reaali pientuotanto",
           "reaali ydinvoima",
           "reaali kokonaiskulutus",
           "reaali vienti",
           "reaali kokonaistuotanto"))
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
louhi_dataa_FG <- function() {

  energialahteet <<- fetch_energialahteet()
  #energialahteet <- c("reaali kokonaiskulutus", "reaali kokonaistuotanto")
  #energialahteet <- c("reaali tuulivoima")

  date_vec <<- c(as.character(Sys.time()), "2024-05-22", "2024-05-01", "2024-04-01", "2024-03-01", "2024-02-01", "2024-01-01", "2023-12-01", "2023-01-01")

  for (i in 1:length(energialahteet)) {
    print(energialahteet[i])
    print(i/length(energialahteet))
    #energiantuotanto_list <- lapply(energialahteet, assign_energiantuotanto, startTime = date_vec[i+1], endTime = date_vec[i])
    #energiantuotanto_data_frame <- do.call(data.frame, lapply(date_vec, assign_energiantuotanto_2,
    #                                                          nimi = energialahteet[i]))

    #assign(paste("energiantuotanto_data_frame", energialahteet[i], sep = "_"), do.call(rbind, lapply(date_vec, assign_energiantuotanto_2,
    #                                                                                                      nimi = energialahteet[i])), .GlobalEnv)

    assign("aurinko_data", do.call(rbind, lapply(date_vec, assign_energiantuotanto_2, nimi = energialahteet[i])), .GlobalEnv)

    #assign(paste("energiantuotanto_data_frame", energialahteet[i], sep = "_"),
    #       energiantuotanto_data_frame)

    #filevec <<- c(filevec, paste("energiantuotanto_data_frame", date_vec[i], date_vec[i+1], sep = "_"))

  }

}
