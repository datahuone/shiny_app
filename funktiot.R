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
      "reaali kokonaiskulutus" = 193)
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

lataa_aikasarja_fingrid <- function(arvo, alku, loppu){
  return(NULL)
}

  ota_yhteys_fingrid_api <- function(url){

    if (!exists(api_key)) {

      api_key <- feather::read_feather('rsconnect/fingrid_api.feather') %>% pull()

    }

    res = GET(url,
              query = list(`x-api-key` = api_key))

    return(as_tibble(fromJSON(rawToChar(res$content))) %>%
             select(-end_time) %>%
             rename(time = start_time))


  }
