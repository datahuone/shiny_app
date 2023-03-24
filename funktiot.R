tuhaterotin <- function(x){
  format(x, big.mark= " ",
         decimal.mark = ",",
         scientific = FALSE)
}

prosenttierotin <- function(x){
  paste0(tuhaterotin(x*100), " %")
}

kuukaudet_suom <- function(date){
  locaali <- readr::date_names_lang("fi")
  gsub('.{2}$','',locaali$mon)[lubridate::month(date)]
}

formatoi_kuukaudet_plot <- function(date){
  return(paste0(kuukaudet_suom(date), format(date, " %y")))
}


lataa_data <- function(kansion_nimi, kuukaudet){
  boxplotit <- lapply(
    kuukaudet, function(x) feather::read_feather(paste0("data/",kansion_nimi,"/data",x,".feather")))

  names(boxplotit) <- kuukaudet

  return(boxplotit)
}


summarise_to_boxplot <- function(data, column, conf_int = 0.95){

  low_bound <- (1-conf_int)/2
  high_bound <- 1-low_bound

  data %>%
    rename(data_column = !!column) %>%
    summarise(
      y_min = quantile(data_column, low_bound, na.rm=T),
      y_25 = quantile(data_column, 0.25, na.rm=T),
      y_median = median(data_column, na.rm=T),
      y_75 = quantile(data_column, 0.75, na.rm=T),
      y_max = quantile(data_column, high_bound, na.rm=T),
      y_mean = mean(data_column, na.rm = T),
      n = n()
    )
}

check_dom <- function(column, dom = 0.75, n = 1) {
  # tarkistaa dominanssin, perus skenaario on (1,75)
  dom > (max(column, na.rm = T) / (1 + sum(column, na.rm = T)))
}



hae_kunta <- function(data, haettava_kunta){

  data %>%
    filter(kunnan_nimi == haettava_kunta) %>%
    select(-c(kunnan_nimi, maakunta))

}

# Fingrid ========================================================

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
      "reaali vienti" = 194
      )
  )
}

lataa_viimeisin_fingrid <- function(arvo){

  arvot <- fingrid_sanakirja()
  if(!arvo %in% names(arvot)){
    stop("Syötä oikea arvo")
  }

  variableID <- arvot[arvo]
  path <- paste0("https://api.fingrid.fi/v1/variable/",variableID,"/event/json")

  return(ota_yhteys_fingrid_api(path) %>% select(value) %>% pull())
}

lataa_aikasarja_fingrid <- function(arvo,
                                    alku = Sys.time()-lubridate::weeks(1),
                                    loppu = Sys.time(),
                                    muuta_nimi = NULL){
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

lataa_viikko_fingridistä <- function(){
  lataa_aikasarja_fingrid("reaali kokonaiskulutus") %>%
    rename(kulutus = value) %>%
    left_join(
      lataa_aikasarja_fingrid("reaali kokonaistuotanto") %>%
        rename(tuotanto = value)
    )
}

ota_yhteys_fingrid_api <- function(url){

    if (!exists('api_key')) {

      api_key <- feather::read_feather('fingrid_api.feather') %>%
        pull()

    }

    res = GET(url,
              query = list(`x-api-key` = api_key))

    result_tibble <- as_tibble(fromJSON(rawToChar(res$content)))

    return(result_tibble %>%
             select(-end_time) %>%
             rename(time = start_time))


}

lataa_kaikki <- function(){

  as_tibble(sapply(names(fingrid_sanakirja()), lataa_viimeisin_fingrid)) %>%
    mutate(name = names(fingrid_sanakirja()))

}
