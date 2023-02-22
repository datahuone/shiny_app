library(tidyverse, warn.conflicts = F)
library(parallel)

Sys.setlocale(locale = 'Finnish')
source("funktiot.R")



cl <- parallel::makeCluster(detectCores())

fdh_kansiot <- feather::read_feather("data/kuukaudet_saatavilla.feather")
fdh_lista <- parallel::parLapply(cl,fdh_kansiot$kuukaudet,load_fdh_dt)

fdh_sopimuksittain <- lapply(fdh_lista, function(x) summarise_to_boxplot(
  data = dplyr::group_by(x, desiili_new2),
  column = "QUANTITY",
  conf_int = 0.9) %>%
    tidyr::drop_na() %>%
    collect())

for (i in c(1:length(fdh_kansiot$kuukaudet))) {
  path <- paste0("data/sopimuksittain_boxplotit/data",fdh_kansiot$kuukaudet[i],".feather")
  feather::write_feather(fdh_sopimuksittain[[i]], path = path)
}




fdh_asuntokunnittain <- lapply(fdh_lista, tee_temput)

stopCluster(cl)

fdh_boxplotit <- lapply(fdh_asuntokunnittain, function(x) summarise_to_boxplot(
  data = dplyr::group_by(x, desiili_new2),
  column = "sum_quantity",
  conf_int = 0.9) %>%
    tidyr::drop_na() %>%
    collect())

for (i in c(1:length(fdh_kansiot$kuukaudet))) {
  path <- paste0("data/asuntokunnittain_boxplotit/data",fdh_kansiot$kuukaudet[i],".feather")
  feather::write_feather(fdh_boxplotit[[i]], path = path)
}



fdh_asuntokunnittain[[1]] %>% colnames()


muuttujat <- c("IsFixedTermAgreement", "IsHeatingDependentOnElectricity")
kansio <- c("asuntokunnittain_maaraaik_boxplotit","asuntokunnittain_lammitysmuoto_boxplotit")

for (i in c(1:length(muuttujat))) {

  fdh_grouped <- lapply(fdh_asuntokunnittain, function(x) summarise_to_boxplot(
    data = dplyr::group_by(x, desiili_new2, !!!syms(muuttujat[i])),
    column = "sum_quantity",
    conf_int = 0.9) %>%
      tidyr::drop_na() %>%
      collect())

  for (j in c(1:length(fdh_kansiot$kuukaudet))) {
    path <- paste0("data/",kansio[i],"/data",fdh_kansiot$kuukaudet[j],".feather")
    feather::write_feather(fdh_grouped[[j]], path = path)
  }

}











































