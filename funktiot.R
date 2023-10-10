# Apufunktiot ===============================================

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


#' Prosenttierotin
#'
#' Formatoi prosenttiluvut oikeaoppisesti. Lisäämällä välin luvun sekä % väliin.
#'
#' @param x Luku 0,1 väliltä
#' @return str

prosenttierotin <- function(x){
  paste0(tuhaterotin(x*100), " %")
}

#'Suomenkieliset kuukaudet
#'
#'Hakee readr-paketista suomenkielisen kuukauden nimen päivämäärälle.
#'
#' @param date
#' @return str suomenkielinen kuukausi
kuukaudet_suom <- function(date){
  locaali <- readr::date_names_lang("fi")
  gsub('.{2}$','',locaali$mon)[lubridate::month(date)]
}

#' Formatoikuukausien nimi kuvaajiin
#'
#' Lisää
formatoi_kuukaudet_plot <- function(date){
  return(paste0(kuukaudet_suom(date), format(date, " %y")))
}


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
      "reaali vienti" = 194#,
      # "reaali sähköpula" = 336
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

lataa_tuotanto_ja_kulutus_fingridistä <- function(alkuTime = Sys.time()-lubridate::weeks(1),
                                                  loppuTime = Sys.time()){

  lataa_aikasarja_fingrid("reaali kokonaiskulutus", alku = loppuTime, loppu = alkuTime) %>%
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

    print(result_tibble)

    return(result_tibble %>%
             select(-end_time) %>%
             rename(time = start_time))

}

lataa_kaikki <- function(){

  as_tibble(sapply(names(fingrid_sanakirja()), lataa_viimeisin_fingrid)) %>%
    mutate(name = names(fingrid_sanakirja()))

}

# Ukraina ========================================================

dark_green <- "#234721"
light_green <- "#AED136"
dark_blue <- "#393594"
light_blue <- "#8482BD"
dark_red <- "#721D41"
light_red <- "#CC8EA0"
yellow <- "#FBE802"
orange <- "#F16C13"
light_orange <- "#FFF1E0"

colors <- c(dark_green, light_green, dark_blue, light_blue, dark_red, light_red, yellow, orange, light_orange)



Ukraina_kuvaaja <- function(data, jaottelu, osuus, variable, grouping, ylabtxt, alpha_u, font_size) {
  #function(data.frame, str, boolean, str, str, str)

  data <- Ukraina_aggregaattori(data, jaottelu)
  Ukraina_colours <- colors
  col_pos <- "stack"

  if (osuus) {

    data$n <- round((data$n/data$n_total)*100, digits = 1)

  }

  if (jaottelu == "none") {
    lookup = c("lukumäärä"="n_total")
    Ukraina_colours <- colors[8]
  } else if (grouping == "ikäryhmä" & osuus == FALSE) {
    lookup = c("lukumäärä"="n", "ikäryhmä"="age_group")
  } else if (grouping == "sukupuoli" & osuus == FALSE) {
    lookup = c("lukumäärä"="n")
    col_pos <- "dodge"
    Ukraina_colours <- c(colors[4], colors[8])
  } else if (grouping == "ikäryhmä" & osuus == TRUE) {
    lookup = c("prosenttia"="n", "ikäryhmä"="age_group")
  } else if (grouping == "sukupuoli" & osuus == TRUE) {
    lookup = c( "prosenttia"="n")
    #col_pos <- "dodge"
    Ukraina_colours <- c(colors[4], colors[8])
  } else if (grouping == "ala" & osuus == FALSE & jaottelu != "ammatti") {
    lookup = c( "lukumäärä"="n") #, "ala" = "toimiala"
  } else if (jaottelu == "ammatti" & osuus == FALSE) {
    lookup = c("lukumäärä"="n") #, "ala" = "nimi_fi"
  }

  p <- data %>%
    rename(any_of(lookup)) %>%
    ggplot(aes(x = aika))
  if (jaottelu == "none") {
    p <- p + geom_col(aes_string(y = variable, fill = grouping), fill = colors[8], alpha = alpha_u, position = col_pos)
  } else {
    p <- p + geom_col(aes_string(y = variable, fill = grouping), alpha = alpha_u, position = col_pos)
  }
    p <- p + scale_x_date(name = "", date_breaks = "1 month", date_labels = "%m/%Y") +
    scale_fill_manual(values = Ukraina_colours) +
    scale_y_continuous(name = ylabtxt, labels = tuhaterotin) +
    theme_light() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(size = font_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 13),
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotlyp <- ggplotly(p) %>%
    layout(legend = list(orientation = "h", x = 0.5, y = -0.5, xanchor = 'center', title=list(text='')))
}

Ukraina_aggregaattori <- function(data, param) {

  switch(param,
         none = distinct(data, aika, n_total),
         ikäryhmä = data %>%
           group_by(aika, n_total, age_group) %>%
           summarise(n = sum(n)),
         sukupuoli = data %>%
           group_by(aika, n_total, sukupuoli) %>%
           summarise(n = sum(n)),
         toimiala = data %>%
           filter(aika > ymd("2022-04-01")) %>%
           filter(toimiala_nimi %in% top) %>%
           mutate(ala = toimiala_nimi ),
         ammatti = data %>%
           filter(aika > ymd("2022-04-01")) %>%
           filter(t3_nimi %in% top) %>%
           mutate(ala = t3_nimi)
        )

}

assign_energiantuotanto <- function(nimi_energialahde, startTime, endTime) {
  print(nimi_energialahde)
  assign(paste0("energiantuotanto_", gsub("reaali ", "", nimi_energialahde)), lataa_aikasarja_fingrid(nimi_energialahde, alku = as.POSIXct(startTime),
                                                                                                      loppu = as.POSIXct(endTime)) %>%
           arrange(desc(time)) %>%
           slice(which(row_number() %% 20 == 1)) %>%
           mutate(time = lubridate::ymd_hms(time)) %>%
           mutate(time = lubridate::floor_date(time, unit = "hours")))

}

assign_energiantuotanto_2 <- function(time, nimi_energialahde) {
  print(nimi_energialahde)
  assign(paste0("energiantuotanto_", gsub("reaali ", "", nimi_energialahde)), lataa_aikasarja_fingrid(nimi_energialahde, alku = as.POSIXct(time) - lubridate::weeks(5),
                                                                                                      loppu = as.POSIXct(time)) %>%
           arrange(desc(time)) %>%
           slice(which(row_number() %% 20 == 1)) %>%
           mutate(time = lubridate::ymd_hms(time)) %>%
           mutate(time = lubridate::floor_date(time, unit = "hours")))

}

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

louhi_dataa_FG <- function() {

  energialahteet <- fetch_energialahteet()
  energialahteet <- c("reaali kokonaiskulutus", "reaali kokonaistuotanto")
  energialahteet <- c("ennuste aurinko")

  date_vec <<- c(as.character(Sys.time()), "2023-08-01", "2023-07-01", "2023-06-01", "2023-05-01", "2023-04-01", "2023-03-01", "2023-02-01", "2023-01-01",
                "2022-12-01", "2022-11-01" ,"2022-10-01", "2022-09-01", "2022-08-01", "2022-07-01", "2022-06-01", "2022-05-01", "2022-04-01", "2022-03-01", "2022-02-01", "2022-01-01",
                "2021-12-01", "2021-11-01", "2021-10-01", "2021-09-01", "2021-08-01", "2021-07-01", "2021-06-01", "2021-05-01", "2021-04-01", "2021-03-01", "2021-02-01", "2021-01-01",
                "2020-12-01", "2020-11-01", "2020-10-01", "2020-09-01", "2020-08-01", "2020-07-01", "2020-06-01", "2020-05-01", "2020-04-01", "2020-03-01", "2020-02-01", "2020-01-01"
                )

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

checkUpdateCondition <- function(x) {
  # This function updates the data specified if an hour has lapsed since the last update

  if (x < (Sys.time() - lubridate::hours(2))) {
    return(TRUE) #This should be TRUE.
  } else {
    return(FALSE) #This should be FALSE. Set it TRUE for testing
  }

}
