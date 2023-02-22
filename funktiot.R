tuhaterotin <- function(x){
  format(x, big.mark= " ",
         decimal.mark = ",",
         scientific = FALSE)
}

prosenttierotin <- function(x){
  paste0(x*100, " %")
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

tee_temput <- function(data){
  #Muuttaa datan sopimusperustaisesta asuntokuntaperustaiseksi
  #
  #samalla valitsee sopimustyypin & lämmitystyypin
  #suurimman sopimuksen mukaan

  data %>%
    group_by(srnro,shuontuvtj, ak_id) %>%
    mutate(sum_quantity = sum(QUANTITY, na.rm = T)) %>%
    slice(which.max(QUANTITY)) %>%
    select(shnro, srnro, shuontuvtj, askunta, ak_koko,
           sum_quantity, svatva_ak, IsFixedTermAgreement,
           desiili_new2, IsHeatingDependentOnElectricity,
           UserGroup, PostalCode, taajama_k2) %>%
    ungroup()
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

hae_fdh_kansiot <- function(overwrite = T){

  #Hakee ja tulostaa ready-madesta löytyvät FDH kansiot

  if(R.version$os == "linux-gnu"){

    #tarkistaa käyttöjärjestelmän
    cat("Aja tämä cain windowsilla")
    return(NULL)

  } else{

    path_preamble_data <- "D:/"
  }

  fdh_kansiot <- list.files(paste0(path_preamble_data,"ready-made/CONTINUOUS/DR_STAT_FGDATAHUB"))

  pvm <- lapply(fdh_kansiot,
                function(x) as.character(
                  paste0('1.',substr(x,1,nchar(x)-2),'.',substr(x,nchar(x)-1,nchar(x)))
                )
  )

  if (overwrite) {

    tibble::tibble(
      kuukaudet = lubridate::dmy(pvm, locale = 'Finnish')
    ) %>%
      feather::write_feather("data/kuukaudet_saatavilla.feather")
    return(NULL)

  }

  return(lubridate::dmy(pvm, locale = 'Finnish'))

}



load_fdh <- function(date = lubridate::dmy("01.10.2022"),
                     tulomuuttujat = c("srnro",
                                       "shuontuvtj",
                                       "shnro",
                                       "askunta",
                                       "ak_id",
                                       "ak_koko",
                                       "desiili_new2")){
  require(tidyverse, warn.conflicts = F)


  if(R.version$os == "linux-gnu"){

    #tarkistaa käyttöjärjestelmän
    path_preamble_data <- "/mnt/"
    path_preamble_work <- "/mnt/work/"
    locaali <- readr::date_names_lang('fi')

    #päivämäärätemppuja koska locale ei toiminnassa linuxilla atm:
    kuu <- gsub('.{2}$','',locaali$mon)[lubridate::month(date)]
    vuosi <- format(date, "%y")
    subfolder <- paste0(kuu,vuosi)

  } else {

    Sys.setlocale(locale = 'Finnish_Finland')

    path_preamble_data <- "D:/"
    path_preamble_work <- "W:/"

    subfolder <- format(date, "%B%y") %>% tolower()
  }

  file_end <- format(date, "%m%y")
  path <- paste0(path_preamble_data,"ready-made/CONTINUOUS/DR_STAT_FGDATAHUB/", subfolder)

  if (!dir.exists(path)){
    cat("Ei ole vielä dataa kuukaudelta", format(date, "%B %y"))
  }

  setwd(path)

  #metered_data <- data.table::fread(paste0("smdsf_",file_end,".csv")) %>% janitor::clean_names()
  metered_data_clean <- vroom::vroom(
    paste0("smdsf_",file_end,".csv"),
    col_select = c(gsrn_s, quantity),
    col_types = c(gsrn_s = 'c',
                  quantity = 'd'),
    .name_repair = janitor::make_clean_names
  )

  #sagr_cust <- data.table::fread(paste0("sacrc_",file_end,".csv")) %>% janitor::clean_names()

  sagr_cust_clean <- vroom::vroom(
    paste0("sacrc_",file_end,".csv"),
    col_select = c(metering_point_ean_s,
                   shnro,
                   market_role,
                   agreement_type),
    col_types = c(metering_point_ean_s = 'c',
                  shnro = 'c',
                  market_role ='f',
                  agreement_type = 'i'),
    .name_repair = janitor::make_clean_names
  ) %>%
    filter(
      agreement_type == 11,
      market_role == 'DSO',
      shnro != '') %>%
    select(shnro,
           metering_point_ean_s)

  #sagr_inv <- data.table::fread(paste0("saaiac_",file_end,".csv")) %>% janitor::clean_names()

  sagr_inv_clean <- vroom::vroom(
    paste0("saaiac_",file_end,".csv"),
    col_select = c(metering_point_ean_s,
                   agreement_type,
                   is_fixed_term_agreement,
                   fixed_term_start_date,
                   fixed_term_end_date),
    col_types = c(metering_point_ean_s = 'c',
                  agreement_type = 'i',
                  is_fixed_term_agreement = 'i',
                  fixed_term_start_date = '?',
                  fixed_term_end_date = '?'),
    .name_repair = janitor::make_clean_names
  ) %>%
    filter(agreement_type == 12) %>%
    select(-agreement_type)

  #kuluttaja <- data.table::fread(paste0("skuluttajat_",file_end,".csv")) %>% janitor::clean_names()
  kuluttaja_clean <- vroom::vroom(
    paste0("skuluttajat_",file_end,".csv"),
    col_select = c(customer_identification_s, shnro),
    col_types = c(customer_identification_s = 'c',
                  shnro ='c'),
    .name_repair = janitor::make_clean_names
  ) %>%
    filter(shnro != '')


  metering_point_contrl <- vroom::vroom(
    paste0("smpc_",file_end,".csv"),
    col_select = c(metering_point_ean_s,
                   fuse_size,
                   is_heating_dependent_on_electricity,
                   user_group),
    col_types = c(metering_point_ean_s = 'c',
                  fuse_size = 'f',
                  is_heating_dependent_on_electricity = 'l',
                  user_group = 'f'),
    .name_repair = janitor::make_clean_names
  )


  kunnat <- readxl::read_xlsx(
    paste0(path_preamble_data,"metadata/classifications/region/alueryhmittely_posnro_2021_fi.xlsx"),
    skip = 2 ) %>%
    janitor::clean_names() %>%
    distinct(kunta,.keep_all = T) %>%
    select(kunta, maakunta, kuntaryhma, seutukunta, suuralue) %>%
    mutate(kunta = as.numeric(kunta))



  postinumero <- vroom::vroom(
    paste0("smpac_",file_end,".csv"),
    col_select = c(metering_point_ean_s,
                   postal_code),
    col_types = c(metering_point_ean_s = 'c',
                  postal_code = 'f'),
    .name_repair = janitor::make_clean_names) %>%
    distinct(metering_point_ean_s, .keep_all = T)



  total_data <- sagr_cust_clean %>%
    left_join(metered_data_clean, by = c("metering_point_ean_s"="gsrn_s")) %>%
    left_join(sagr_inv_clean, by = "metering_point_ean_s") %>%
    group_by(metering_point_ean_s) %>%
    filter(n() > 1)

  polku <-  paste0(path_preamble_work,"Alhola/data/asuntokunnat/asuntokunnat_",file_end,"_tulot.csv")
  tulot <- vroom::vroom(polku,
                        col_select = all_of(tulomuuttujat),
                        show_col_types = F)

  tulot_ja_kulut <- total_data %>%
    left_join(tulot, by=c("shnro" = "shnro"))%>%
    left_join(metering_point_contrl,
              by = c("metering_point_ean_s"= "metering_point_ean_s")) %>%
    #filter(UserGroup %in%c("BE01","BE02","BE03","BE04","BE05","BE06","BE14")) %>%
    left_join(kunnat,
              by=c("askunta"="kunta")) %>%
    left_join(postinumero,
              by = "metering_point_ean_s")

  rm(postinumero)
  rm(total_data)
  rm(metering_point_contrl)
  rm(tulot)

  return(tulot_ja_kulut)
}


load_fdh_dt <- function(date = lubridate::dmy("01.12.2022")){

  require(tidyverse)


  if(R.version$os == "linux-gnu"){

    #tarkistaa käyttöjärjestelmän
    path_preamble_data <- "/mnt/"
    path_preamble_work <- "/mnt/work/"
    locaali <- readr::date_names_lang('fi')

    #päivämäärätemppuja koska locale ei toiminnassa linuxilla atm:
    kuu <- gsub('.{2}$','',locaali$mon)[lubridate::month(date)]
    vuosi <- format(date, "%y")
    subfolder <- paste0(kuu,vuosi)

  } else {

    Sys.setlocale(locale = 'Finnish_Finland')

    path_preamble_data <- "D:/"
    path_preamble_work <- "W:/"

    subfolder <- format(date, "%B%y") %>% tolower()
  }

  file_end <- format(date, "%m%y")

  path <- paste0(path_preamble_data,"ready-made/CONTINUOUS/DR_STAT_FGDATAHUB/", subfolder)

  if (!dir.exists(path)){
    cat("Ei ole viel? dataa kuukaudelta", format(date, "%B %y"))
  }

  setwd(path)

  data.table::setDTthreads(1)

  if(file_end == "1222"){

    metered_data_clean <- data.table::fread(
      paste0("smdsf_",file_end,".csv"),
      select=c('GSRN_s', 'QUANTITY')
    ) %>%
      rename('gsrn_s'='GSRN_s')

  } else {
    metered_data_clean <- data.table::fread(
      paste0("smdsf_",file_end,".csv"),
      select=c('gsrn_s', 'QUANTITY')
    )
  }

  sagr_cust_clean <- data.table::fread(
    paste0("sacrc_",file_end,".csv"),
    select = c('meteringPointEAN_s','AgreementType','MarketRole','shnro')
  ) %>%
    filter(AgreementType == 11,
           MarketRole == 'DSO',
           shnro != '') %>%
    select(shnro, meteringPointEAN_s)

  sagr_inv_clean <- data.table::fread(
    paste0("saaiac_",file_end,".csv"),
    select = c('AgreementType',
               'meteringPointEAN_s',
               'IsFixedTermAgreement',
               'FixedTermStartDate',
               'FixedTermEndDate'))%>%
    filter(AgreementType == '12')


  kuluttaja_clean <- data.table::fread(
    paste0("skuluttajat_",file_end,".csv"),
    select = c('customerIdentification_s', 'shnro')
  )%>%
    filter(shnro != '')

  metering_point_contrl <- data.table::fread(
    paste0("smpc_",file_end,".csv"),
    select = c('MeteringPointEAN_s',
               'FuseSize',
               'IsHeatingDependentOnElectricity',
               'UserGroup')
  )

  kunnat <- readxl::read_xlsx(
    paste0(path_preamble_data,"metadata/classifications/region/alueryhmittely_posnro_2021_fi.xlsx"),
    skip = 2
  )%>%
    distinct(Kunta,.keep_all = T) %>%
    select(Kunta, Maakunta, Kuntaryhmä, Seutukunta, Suuralue) %>%
    mutate(Kunta = as.numeric(Kunta))

  postinumero <- data.table::fread(paste0("smpac_",file_end,".csv"))

  total_data <- sagr_cust_clean %>%
    left_join(metered_data_clean, by = c("meteringPointEAN_s"="gsrn_s")) %>%
    left_join(sagr_inv_clean) %>%
    distinct(meteringPointEAN_s, .keep_all = T)

  polku <-  paste0(path_preamble_work,"Alhola/data/asuntokunnat/asuntokunnat_",file_end,"_tulot.csv")

  tulot <- data.table::fread(polku)

  tulot_ja_kulut <- total_data %>%
    left_join(tulot, by=c("shnro" = "shnro"))%>%
    left_join(metering_point_contrl,
              by = c("meteringPointEAN_s"= "MeteringPointEAN_s")) %>%
    #filter(UserGroup %in%c("BE01","BE02","BE03","BE04","BE05","BE06","BE14")) %>%
    left_join(kunnat,
              by=c("askunta"="Kunta")) %>%
    left_join(postinumero %>%
                distinct(MeteringPointEAN_s,PostalCode),
              by = c("meteringPointEAN_s"= "MeteringPointEAN_s"))

  rm(total_data)
  rm(metering_point_contrl)
  rm(tulot)

  return(tulot_ja_kulut)
}

hae_kunta <- function(data, haettava_kunta){

  data %>%
    filter(kunnan_nimi == haettava_kunta) %>%
    select(-c(kunnan_nimi, maakunta))

}

