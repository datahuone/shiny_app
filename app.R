# Description:  Run the Datahuone Kojelauta Shiny Application
# Usage:        Run from source in R
# Output:       R Shiny Application Datahuoneen Kojelauta
# Author:       Meeri Seppä, Annakaisa Ritala
# Date:         2024-08-27
#
# Dependencies ----

library(shiny, warn.conflicts = F)
library(shinydashboard, warn.conflicts = F)
library(shinyWidgets, warn.conflicts = F)
library(tidyverse, warn.conflicts = F) #TODO: Select individual dependencies
library(ggplot2, warn.conflicts = F)
library(markdown, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(gghighlight, warn.conflicts = F)
library(httr, warn.conflicts = F)
library(jsonlite, warn.conflicts = F)
library(plotly, warn.conflicts = F)
library(shiny.router)
library(plotly, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)

helpers_R <- list.files(path = "R",
                        full.names = TRUE,
                        recursive = TRUE) |>
  str_subset("electricity|labourmarket|utils")
sapply(X = helpers_R,
       FUN = source,
       encoding = "UTF-8",
       chdir = TRUE)

# Load data related to electricity consumption

#kuntien nimet
kunnat <- feather::read_feather("data/kunnat.feather")
Kunnan_nimet <- kunnat %>% distinct(kunnan_nimi)

#jostain syystä tätä ei voi pitää observe eventin sisällä muut datat ladataan sertrverissä
boxplotit_asuntokunnat <- lataa_data("asuntokunnittain_boxplotit", kuukaudet)
aikasarja_data_raw <- feather::read_feather("data/aikasarjat/kulutus_kk.feather")
kunnat_data_raw <- feather::read_feather("data/kunta_kvantiilit/kunnat_kaikki.feather")

# Load data related to labour markets

## ukraina datan lataaminen
kotikunta <-  read_csv("./data/summaries/sex_month_pop.csv")
ei_kotikuntaa <-  read_csv("./data/summaries/age_sex_month_nonpop.csv")
ikajakauma <- read_csv("./data/summaries/age_gender.csv")
toimialat <- read_csv("./data/summaries/industry.csv")
ammatit <- read_csv("./data/summaries/occupations.csv")
employed  <- read_csv("./data/summaries/employed_age_gender.csv")
employed_kotikunta <- read_csv("./data/summaries/employed_age_pop.csv")

## age group to factor
levels <- c("alle 15","15-19", "20-24", "25-54", "55-64", "yli 64")
ei_kotikuntaa <- ei_kotikuntaa %>% mutate(age_group = factor(age_group, levels = levels))

levels <- ikajakauma %>% distinct(age_group) %>% pull()
ikajakauma <- ikajakauma %>% mutate(age_group = factor(age_group, levels = levels))

## industry to factor
levels <- toimialat %>% distinct(toimiala_nimi) %>% pull()
toimialat <- toimialat %>% mutate(toimiala_nimi = factor(toimiala_nimi, levels = levels))

## profession to factor
levels <- ammatit %>% distinct(t3_nimi) %>% pull()
ammatit <- ammatit %>% mutate(t3_nimi = factor(t3_nimi, levels = levels))

#testi miten orgasnisaatiot toimii

# Local attributes ----

# Labour market settings
alpha_u <- 0.8
font_size <- 15

# Run ----

# User interface (UI)
source(file = file.path("R", "ui.R"))

# Server
source(file = file.path("R", "server.R"))

# Run the application
shinyApp(
  ui = ui,
  server = server
  )

# Clean-up ----
# rm(list = ls())
# gc()
