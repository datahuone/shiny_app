# Koodit datahuoneen kojelautaan.



library(shiny, warn.conflicts = F)
library(shinydashboard, warn.conflicts = F)
library(shinyWidgets, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
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

source("funktiot.R", encoding = 'UTF-8')

lisaa_logo <- F #lisää datahuonelogong yläoikealle

### ladataan data ----------------

#haetaan kuukaudet mistä sähkönkäyttöädataa
kuukaudet <- feather::read_feather("data/kuukaudet_saatavilla.feather") %>%
  arrange(kuukaudet) %>%
  pull()


#kuntien nimet
kunnat <- feather::read_feather("data/kunnat.feather")
Kunnan_nimet <- kunnat %>% distinct(kunnan_nimi)

#jostain syystä tätä ei voi pitää observe eventin sisällä muut datat ladataan sertrverissä
boxplotit_asuntokunnat <- lataa_data("asuntokunnittain_boxplotit", kuukaudet)
aikasarja_data_raw <- feather::read_feather("data/aikasarjat/kulutus_kk.feather")

# URL osoitteet -----------------------------------------

#url-juuri sahkokaytto sivuille
sahk_etusivu_url <- "sahkonkulutus"

#url-lehdet sivuille (mahdollisesti ei toimienää)
sah_kokonaiskulutus <- paste0(sahk_etusivu_url,"/kokonaiskulutus")
sah_desiili_url <- paste0(sahk_etusivu_url,"/sosioekonomiset")
sah_reaaliaikainen_url <- paste0(sahk_etusivu_url,"/reaaliaikainen")
sah_tausta_url <- paste0(sahk_etusivu_url,"/tausta")

#url-juuri tyomarkkinoille
tyomarkkinat_etusivu_url <- "tyomarkkinat"

#url-lehdet tyomarkkinoille (tämäkin saattaa olla toimimatta kun siirryttiin tabsetpanel-rakenteeseen)
tyomarkkinat_ukrainat_url <- paste0(tyomarkkinat_etusivu_url, "/ukrainalaiset")


# ladataan ukrainadata ----------------------------------------------------

## ukraina datan lataaminen
kotikunta <-  read_csv("./data/summaries/sex_month_pop.csv")
ei_kotikuntaa <-  read_csv("./data/summaries/age_sex_month_nonpop.csv")
ikajakauma <- read_csv("./data/summaries/age_gender.csv")
toimialat <- read_csv("./data/summaries/industry.csv")
ammatit <- read_csv("./data/summaries/occupations.csv")
employed  <- read_csv("./data/summaries/employed_age_sex.csv")
employed_kotikunta <- read_csv("./data/summaries/employed_age_sex_pop.csv")

## age group to factor
levels <- c("alle 15","15–19", "20–24", "25–54", "55–64", "yli 64")
ei_kotikuntaa <- ei_kotikuntaa %>% mutate(age_group = factor(age_group, levels = levels))

levels <- ikajakauma %>% distinct(age_group) %>% pull()
ikajakauma <- ikajakauma %>% mutate(age_group = factor(age_group, levels = levels))

## industry to factor
levels <- toimialat %>% distinct(toimiala) %>% pull()
toimialat <- toimialat %>% mutate(toimiala = factor(toimiala, levels = levels))

## profession to factor
levels <- ammatit %>% distinct(nimi_fi) %>% pull()
ammatit <- ammatit %>% mutate(nimi_fi = factor(nimi_fi, levels = levels))

#testi miten orgasnisaatiot toimii

## ukraina kuva-asetukset
alpha_u <- 0.8
font_size <- 15

# UI -------------------------------------
ui <- navbarPage(

  tags$head(
    tags$link(rel = "icon",
              type = "image/png",
              sizes = "32x32",
              href = "DH_pikkulogo.png"),

    if(lisaa_logo){
      tags$style(
        HTML("
          .navbar-nav > li > a, .navbar-brand {
                     padding-top:15px !important;
                     padding-bottom:0 !important;
                     height: 55px;
          }
          .navbar-brand {
                     margin-top: -10px;
                     margin-bottom: 0;
          }

          .navbar {min-height:45px !important;}

          .custom-label {
            font-size: 50px;
          }

          "
          )
        )
      }
    ),


    # Application title
      if(lisaa_logo){
        title =  div(img(src="Datahuone_graafi_littee.png", height = 50))
      },

      windowTitle = "Datahuone",
      id = "navbarID",




  tabPanel(

# Etusivu -----------------------------------------------
    title = "Etusivu",
    icon = icon('house'),
    value = 'etusivu', #valueta käyteteään url muodostamiseen
    fluidPage(
      fluidRow(
        includeMarkdown("tekstit/etusivu.md")
        ),
      tags$br(), #lisätään väli etusivun ja ikonien väliin
      fluidRow( #luodaaan rivi
        column(width = 1),
        column(
          tags$div(
            id = "btn_ymp",
            class = "btn btn-default action-button",
            #nappulan taustaväri ja välin suuruus nappulan ja tekstin välissä
            style = "background-color: #AED136; margin-bottom: 20px;",
            tags$div(
              style = "width: 100px; height: 100px;",
              HTML('<img src="Ikoni_ympäristö.svg" width="100%" height="100%"/>')
            ),

            # alla oleva div tekee label nappiin
            tags$div(
              class="custom-label",
              style = "font-size: 125%",
              HTML("<b>Sähkönkäyttö</b> - Tietoa suomalaisten"),
              tags$br(), # tags$br() muuttaa kahdeksi riviksi
              "kotitalouksien sähkönkäytöstä")
          ),
          width = 5
        ),
        column(
          tags$div(
            id = "btn_tyo",
            class = "btn btn-default action-button",
            #nappulan taustaväri ja välin suuruus nappulan ja tekstin välissä
            style = "background-color: #8482BD; margin-bottom: 20px;",
            tags$div(
              style = "width: 100px; height: 100px;",
              HTML('<img src="Ikoni_työmarkkinat.svg" width="100%" height="100%"/>')
            ),
            # alla oleva div tekee label nappiin
            tags$div(
              class="custom-label",
              style = "font-size: 125%",
              HTML(
              '<b>Työmarkkinat</b> - Tietoa tilapäisen'),
              tags$br(), # tags$br() muuttaa kahdeksi riviksi
              'suojelun piirissä olevista ukrainalaisista')
            ),

          width = 5
          ),

        column(width = 1)
        )
      )
    ),

# sähköjutut ---------------------------
  navbarMenu(
    title = "Ympäristö & energia",
    icon = img(src="Ikoni_ympäristö.svg", height = 25),
    tabPanel(
      title = 'Kotitalouksien sähkönkäyttö',
      value = sahk_etusivu_url,
    tabsetPanel(
    ### sivu ----------------------
    tabPanel(
      title = "Etusivu",
      value = sahk_etusivu_url,  #valueta käyteteään url muodostamiseen
      fluidPage(
        fluidRow(
          column(
            h1("Kotitalouksien sähkönkulutus - Fingrid Datahubin tilastotietojen tarkastelu"),
            includeMarkdown("tekstit/sahko_leipateksti.md"),
           width = 6),
          column(
            plotOutput("piirakkaplot"),
            width = 6)
          ))
    ),
    ### aikasarjapaneeli ----------------------
    tabPanel(
      title = "Kokonaiskulutuksen trendit",
      value = sah_kokonaiskulutus,  #valueta käyteteään url muodostamiseen

      sidebarLayout(
        sidebarPanel(
          dateRangeInput(
            inputId = 'aikasarja',
            label = "Tarkasteluajanjakso",
            start = min(kuukaudet),
            end = max(kuukaudet) %m+% months(1) %m-% days(1),
            min = min(kuukaudet),
            max = max(kuukaudet) %m+% months(1) %m-% days(1), #viimeisimmän kuun viimeinen päivä
            format = "d.m.yyyy",
            separator = '-'
          ),
          selectInput(
            inputId = 'tarktaso',
            label = "Tarkastelutaso",
            choices = c('Koko maa',
                        "Maakunnittain",
                        "Kunnittain"),
            selected = 'Koko maa'
          ),
          selectInput(
            inputId = 'valitut',
            label = "Korosta",
            selected = "Suomi",
            choices = c("Suomi"),
            multiple = T
          ),
          selectInput(
            inputId = "suure",
            label = NULL,
            selected = "Kokonaiskulutus",
            choices = c("Kokonaiskulutus",
                        "per capita")
          ),
          p("Voit vaikuttaa kuvaajaan muuttamalla yllä olevia valintoja")
        ),
        mainPanel(
          fluidRow(h1("Yksityishenkilöiden yhteenlaskettu sähkönkäyttö")),
          fluidRow(
            column(plotOutput("aikasarjaplot"), width = 11),
            ),
          fluidRow(
            downloadButton("download_aikasarja", "Lataa csv")
          )
          )
        )
      ),
## desiilipaneeli --------------------------------
    tabPanel(
      title = "Sosioekonomisten muuttujat",
      value = sah_desiili_url,  #valueta käyteteään url muodostamiseen
      sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = 'kk',
              label = 'Tarkastelukuukausi',
              choices = sort(kuukaudet),
              selected = max(kuukaudet)
              ),
            selectInput(
              inputId = 'soptyyp',
              label = 'Tulokymmenyksien jaottelu',
              choices = c("-" ,
                          "sopimuksien lukumäärä" ,
                          "määräaikaiset sopimukset",
                          "lämmitys riippuvainen sähköstä" ,
                          "asuu taajama-alueella" ,
                          "asuu kerrostalossa" ,
                          "asuntokunnan koko"),
              selected = "-"
              ),
            checkboxInput(
              inputId = 'mean',
              label = 'Lisää kuvaajaan desiilien keskiarvot',
              value = TRUE
            ),
            checkboxInput(
              inputId = 'error',
              label = 'Lisää kuvaajaan desiilien jakaumaviivat',
              value = TRUE
            ),
            checkboxInput(
              inputId = 'locked_scale',
              label = 'Lukitse kuvaajan y-akselin skaala',
              value = TRUE
            ),
            p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon.")
          ),

          mainPanel(
            fluidRow(
              h2(
                textOutput('boxplot_otsikko')
                )
              ),
            fluidRow(
              column(width = 1),
              column(
                plotOutput("boxplot"),
                width = 10
                ),
              column(width = 1)
              ),
            fluidRow(
              valueBoxOutput("askumaarat", width = 4),
              valueBoxOutput("dessuhde", width = 8)
              ),
            fluidRow(
              downloadButton("download", "Lataa csv")
            ),
            fluidRow(
              h3(textOutput("taustaotsikko"))
              ),
            fluidRow(
              column(width = 1),
              column(
                plotOutput("tausta"),
                width = 10
              ),
              column(width = 1)
              )
            )
          )
        ),
 # reaaliaikainen ----------------------------------------------
     tabPanel(

       shinyjs::useShinyjs(),

       title = "Reaaliaikainen sähkönkäyttötilanne",
       value = sah_reaaliaikainen_url,
       sidebarLayout(
         sidebarPanel(
           dateRangeInput(
             "sahkoDate", "Valitse Aikaväli:",
             start = Sys.time()-lubridate::weeks(1),
             end = Sys.time(),
             min = lubridate::as_datetime("27-11-2019", format = "%d-%m-%Y"),
             max = Sys.time(),
             separator = "-"
           ),

           checkboxGroupInput("reaaliaikaKuvaajaAsetus", "Muokkaa esitysmuotoa",
                              c("Näytä kuvaaja aina tunneittain"), selected = NA),

           p("Voit muokata esitysmuotoa yllä olevilla asetuksilla. Kuvaajan oletusasetus on muuttaa tarkasteluaikaväli päiviin, kun valittu aikaväli on pidempi kuin kuukausi.
             Tätä asetusta voi muuttaa, mutta kuvaaja saattaa tällöin latautua hitaasti. Ladattavaan dataan vaikuttaa ainoastaan valittu aikaväli."),

           actionButton("resetSahko", "Palauta oletusasetukset")
         ),

         mainPanel(
           fluidRow(
             h1("Reaaliaikainen sähkönkäyttötilanne")
           ),
           fluidRow(
             valueBoxOutput("kokonaiskulutus", width = 4),
             valueBoxOutput("kokonaistuotanto", width = 4),
             valueBoxOutput("tuulisuhde", width = 4)
           ),
           fluidRow(
             valueBoxOutput("muutoskulutus", width = 4),
             valueBoxOutput("muutostuotanto", width = 4),
             valueBoxOutput("nettovienti", width = 4)
           ),
           fluidRow(h2("Sähkön kulutus sekä tuotanto viimeisen viikon aikana")),
           fluidRow(
             column(plotlyOutput("viikkoplot"), width = 10)
           ),
           fluidRow(
             column(plotlyOutput("viikkoplot_dekomponoitu"), width = 10)
           ),
           fluidRow(
             column(
               p("Lähde: Fingridin avoin data -verkkopalvelu"),width = 4
             )
           ),

           fluidRow(
             downloadButton("download_dekomponoitu", "Lataa csv")
           )
         )


       )

     ),

    tabPanel(
      title = "Taustaa datasta",
      #value = ,  #valueta käyteteään url muodostamiseen
      fluidPage(
        fluidRow(includeMarkdown("tekstit/dataselite.md")
        ))
    )
 ))),

# työmarkkinat ----------------------------------------------------

 ## ukrainalaiset ----------------------------------------------
 navbarMenu(


   title = "Työmarkkinat",
   icon = img(src="Ikoni_työmarkkinat.svg", height = 25),


   tabPanel(
     title = "Ukrainalaiset Suomessa",
     value = tyomarkkinat_ukrainat_url,
     tabsetPanel(
       tabPanel("Ukrainalaiset Suomessa",
        fluidPage(
         column(includeMarkdown("tekstit/ukraina_etusivu.md"), width = 6),
         column( h3("Tilapäisen suojelun piirissä olevien ukrainalaisten ikä- ja sukupuolijakauma"),
                 plotlyOutput("ikaryhma"), width = 6)
          )
        ),
       tabPanel("Taustatietoja",
                fluidPage(

                  sidebarLayout(
                    # sivupaneelin valinnat
                    sidebarPanel(
                      selectInput("vaesto", "Valitse kohdejoukko",
                                  choices= c("kaikki ukrainalaiset", "kotikunnan saaneet")),
                      selectInput("jaottelu", "Lisää jaottelu ",
                                  choices= c("-", "ikäryhmä", "sukupuoli")),
                      checkboxInput(inputId = "osuus",
                                    label = "prosentteina",
                                    value = FALSE),
                      p("Voit nähdä tarkan lukumäärän tai osuuden viemällä kursorin haluamasi palkin päälle."),
                      p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon."),
                      p(strong("Huom!"),"Mikäli jonkin kuukauden tiedot eivät ole näkyvissä, tiedot on jouduttu peittämään liian pienen havaintomäärän takia.")
                    ),

                    # Create a spot for the barplot
                    mainPanel(
                      fluidRow(h2( textOutput('taustatieto_otsikko'))),
                      plotlyOutput("basic_plot"),
                      fluidRow(downloadButton("download_taustatiedot", "Lataa csv"))
                    )
                  )
                ) ## close fluid page
              ), ## close tab panel

       tabPanel("Työllistyminen",
                fluidPage(

                  sidebarLayout(
                    # sivupaneelin valinnat
                    sidebarPanel(
                      selectInput("employed", "Valitse kohdejoukko",
                                  choices= c("kaikki ukrainalaiset", "kotikunnan saaneet")),
                      selectInput("jaottelu_emp", "Lisää jaottelu ",
                                  choices= c("-", "ikäryhmä", "sukupuoli")),
                      checkboxInput(inputId = "osuus_emp",
                                    label = "prosentteina",
                                    value = FALSE),
                      p("Voit nähdä tarkan lukumäärän tai osuuden viemällä kursorin haluamasi palkin päälle."),
                      p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon."),
                      p(strong("Huom!"),"Mikäli jonkin kuukauden tiedot eivät ole näkyvissä, tiedot on jouduttu peittämään liian pienen havaintomäärän takia.")
                    ),

                    # Create a spot for the barplot
                    mainPanel(
                      fluidRow(h2(textOutput('emp_otsikko'))),
                      plotlyOutput("emp_plot"),
                      fluidRow(downloadButton("download_emp", "Lataa csv"))
                    )
                  )
                ) ## close fluid page
       ), ## close tab panel
       tabPanel("Toimialat ja ammatit",
                fluidPage(

                  sidebarLayout(
                    # sivupaneelin valinnat
                    sidebarPanel(
                      selectInput("alavaiammatti", "Valitse kategoria",
                                  choices= c("toimialat", "ammattinimikkeet")),
                      selectInput("top", "Valitse tarkasteltavien alojen lkm",
                                  choices= c(1:8),
                                  selected = 5),
                      p("Voit nähdä tarkan lukumäärän tai osuuden viemällä kursorin haluamasi palkin päälle."),
                      p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon."),
                      p(strong("Huom!"),"Mikäli jonkin kuukauden tiedot eivät ole näkyvissä, tiedot on jouduttu peittämään liian pienen havaintomäärän takia.")
                    ),

                    # Create a spot for the barplot
                    mainPanel(
                      fluidRow(h2( textOutput('toimialat_otsikko'))),
                      plotlyOutput("ala_ammatti_plot"),
                      fluidRow(downloadButton("download_alat_ja_ammatit", "Lataa csv"))
                      )
                    )

                  ) ## close fluid page

                ) ## close tab panel
     ) ## close tabset panel
   ) ## close tab panel
 )
)





# SERVERI ------------------------------------------------
server <- function(input, output, session) {

  ## url päivitys ---------------------------------
  observeEvent(input$navbarID, {
    pushQueryString <- paste0("#", input$navbarID)
    currentQuery <- sub("#", "", session$clientData$url_search)
    if(!is.null(currentQuery) && nchar(currentQuery) > 0){
      pushQueryString <- paste0(pushQueryString, "?", currentQuery)
    }
    updateQueryString(pushQueryString, mode = "push", session)
  }, priority = 1)

  observeEvent(session$clientData$url_hash, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    if(is.null(input$navbarID) || !is.null(currentHash) && currentHash != input$navbarID){
      freezeReactiveValue(input, "navbarID")
      updateNavbarPage(session, "navbarID", selected = currentHash)
    }
  }, priority = 2)



  ## API-kutsut -------------------------------

  observeEvent(input$navbarID, {
    #hakee fingridin viikkodatan vain jos on sahkonkulutus/reaaliaikainen välilehdellä'
    if(input$navbarID %in% c(sahk_etusivu_url, sah_reaaliaikainen_url)){ #sahk_etusivu_url,

      vuorokausi_sitten <<- eilen()

      #vuorokausi_sitten <<- kokonaiskulutus_kokonaistuotanto_data_fd() %>%
      #  filter(time == lubridate::floor_date(
      #    Sys.time()-lubridate::days(1),
      #    unit = "hours"))

      #kokonaiskulutus_kokonaistuotanto_data_fd_tuuli <<- lataa_aikasarja_fingrid("reaali tuulivoima") %>%
      #  arrange(desc(time)) %>%
      #  slice(which(row_number() %% 20 == 1)) %>%
      #  mutate(time = lubridate::ymd_hms(time)) %>%
      #  mutate(time = lubridate::floor_date(time, unit = "hours"))

    }
  })

  observeEvent(input$navbarID, {
    #hakee fingridin reaaliaikaisen datan vain jos on sahkonkulutus/reaaliaikainen-välilehdillä'
    if(input$navbarID %in% c(sahk_etusivu_url, sah_reaaliaikainen_url)){ #

      viimeisin_fingrid <- viimeisin()
      print(viimeisin_fingrid)

      uusin_kulutus <<- viimeisin_fingrid$value[viimeisin_fingrid$name == "reaali kokonaiskulutus"]
      uusin_tuotanto <<- viimeisin_fingrid$value[viimeisin_fingrid$name == "reaali kokonaistuotanto"]
      uusin_tuuli <<- viimeisin_fingrid$value[viimeisin_fingrid$name == "reaali tuulivoima"]
      uusin_vienti <<- viimeisin_fingrid$value[viimeisin_fingrid$name == "reaali vienti"]

      #uusin_tuotanto <<- lataa_viimeisin_fingrid("reaali kokonaistuotanto")
      #uusin_tuuli <<- lataa_viimeisin_fingrid("reaali tuulivoima")
      #uusin_vienti <<- lataa_viimeisin_fingrid("reaali vienti")
    }
  })

  # Ikonit etusivulla -------------------------------------------------------

  observeEvent(
    input$btn_ymp,{
      updateTabsetPanel(session, "navbarID", selected = sahk_etusivu_url)
    }
  )

  observeEvent(
    input$btn_tyo,{
      updateTabsetPanel(session, "navbarID", selected = tyomarkkinat_ukrainat_url)
    }
  )

  # Reaktiiviset datasetit ----------------------

  ## ladataa boxplotdatat muistille vasta kun käyttäjä menee sivulle:

  observeEvent(input$navbarID, {
    #hakee fingridin viikkodatan vain jos on sahkonkulutus/reaaliaikainen välilehdellä'
    if(input$navbarID  %in% c(sahk_etusivu_url, sah_reaaliaikainen_url)){

      print("Loading data")

      #lataa_data_cluster <- parallel::makeCluster(parallel::detectCores() - 1)
      #parallel::clusterExport(lataa_data_cluster, kuukaudet)

      boxplotit_sopimukset   <<- lataa_data("asuntokunnittain_sopimustenlkm_boxplotit",kuukaudet)
      boxplotit_maaraik     <<- lataa_data("asuntokunnittain_maaraaik_boxplotit", kuukaudet)
      boxplotit_lammitys    <<- lataa_data("asuntokunnittain_lammitysmuoto_boxplotit", kuukaudet)
      boxplotit_taajama <<- lataa_data("asuntokunnittain_taajama_boxplotit", kuukaudet)
      boxplotit_kerrostalo <<- lataa_data("asuntokunnittain_kerrostalo_boxplotit", kuukaudet)
      boxplotit_askoko <<- lataa_data("asuntokunnittain_askoko_boxplotit", kuukaudet)

      #stopCluster(lataa_data_cluster)

      print("Loaded")

      boxplotlista <<- list(
        "-" = boxplotit_asuntokunnat,
        "sopimuksien lukumäärä" = boxplotit_sopimukset,
        "määräaikaiset sopimukset"= boxplotit_maaraik,
        "lämmitys riippuvainen sähköstä" = boxplotit_lammitys,
        "asuu taajama-alueella" = boxplotit_taajama,
        "asuu kerrostalossa" = boxplotit_kerrostalo,
        "asuntokunnan koko"= boxplotit_askoko
      )
    }
  })

  observeEvent(input$resetSahko, {

    shinyjs::reset("sahkoDate")

  })

  boxplot_data <- reactive({
    return(boxplotlista[[input$soptyyp]][[input$kk]])
  })


  kunta_data <- reactive({
    sapply(kunta_kvantiilit, hae_kunta, haettava_kunta = input$kotkunt) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("kvantiili") %>%
      as_tibble() %>%
      unnest(cols = as.character(kuukaudet))
  })

  output$boxplot_otsikko <- renderText({
    paste0("Sähkönkulutus tulokymmenyksittäin ",
           kuukaudet_suom(input$kk),"ssa ",
           format(as.Date(input$kk), "%Y")
    )
  })

  valintojen_valinnat <- reactive({

    if (input$tarktaso == 'Maakunnittain') {

      kunnat %>%
        filter(maakunta != "Ahvenanmaa") %>%
        distinct(maakunta) %>%
        pull()

    } else if (input$tarktaso == 'Kunnittain') {

      kunnat %>%
        filter(maakunta != "Ahvenanmaa") %>%
        distinct(kunnan_nimi) %>%
        pull()

    } else {

      return(c("Suomi"))

    }

  })

  observe({
    if(input$tarktaso != 'Koko maa') {
      updateSelectInput(session,
                        'valitut',
                        choices = valintojen_valinnat())

    } else {
      updateSelectInput(session,
                        'valitut',
                        selected = 'Suomi',
                        choices = valintojen_valinnat())
    }
  })

  aikasarja_data <- reactive({

    if (input$tarktaso == 'Maakunnittain') {

      aikasarja_data_raw %>%
        left_join(kunnat) %>%
        group_by(maakunta, kuukausi) %>%
        summarise(sahkonkul = sum(sahkonkul),
                  sum_ak = sum(sum_ak)) %>%
        rename(alue = maakunta)

    } else if (input$tarktaso == 'Kunnittain') {

      aikasarja_data_raw %>%
        rename(alue = kunnan_nimi)

    } else {

      aikasarja_data_raw %>%
        group_by(kuukausi) %>%
        summarise(sahkonkul = sum(sahkonkul),
                  sum_ak = sum(sum_ak)) %>%
        mutate(alue = 'Suomi')

    }

  })

  #print(input$sahkoDate)
  globalEndTime <- reactive({
    return(input$sahkoDate[1])
  })

  energiantuotanto_data_frame <- reactive({

    energialahteet <- fetch_energialahteet()

      energiantuotanto_data_frame_decomp <- data.table::fread("./data/energiantuotanto_dekomponoitu.csv")

      print(energiantuotanto_data_frame_decomp)

      colnames(energiantuotanto_data_frame_decomp) <- c("time","kokonaiskulutus","pientuotanto","tehoreservi","tuulivoima",
                                                        "vesivoima","ydinvoima","yhteistuotanto_kaukolämpö","yhteistuotanto_teollisuus",
                                                        "vienti", "kokonaistuotanto")


      if (checkUpdateCondition(as.POSIXct(readLines("data/updateCondition_decomp.txt")[2]))) {
      #Päivitä lokaalisti säilytettävää FG:n data juoksevasti
        dataToBeUpdated <- data.table::fread("./data/energiantuotanto_dekomponoitu.csv")

        energiantuotanto_update <- do.call(data.frame, lapply(energialahteet,
                                                              assign_energiantuotanto, startTime = max(as.POSIXct(dataToBeUpdated$time)),
                                                              endTime = Sys.time()))

        print(paste("Updating data at time", as.character(Sys.time()), sep = " "))



        energiantuotanto_update <- energiantuotanto_update %>%
          select(c(2, 1, 3:ncol(energiantuotanto_update))) %>% #siirrä aikasarake df:n vasempaan reunaan
          select(-grep("time.", colnames(energiantuotanto_update))) %>% #Poista redundantit aikasarakkeet
          set_colnames(c("time", gsub(" ", "_", gsub("reaali ", "", energialahteet))))

        energiantuotanto_update$time %<>% as.character()

        #Prevent duplicate entries
        overlap <- intersect(dataToBeUpdated$time, energiantuotanto_update$time)
        print(overlap)
        overlap_ind <- which(energiantuotanto_update$time %in% overlap)
        print(overlap_ind)
        if (length(overlap_ind) != 0) {
         energiantuotanto_update <- energiantuotanto_update[-c(overlap_ind),]
        }

        energiantuotanto_update$time[nchar(as.character(energiantuotanto_update$time)) <= 10] <- paste(as.character(energiantuotanto_update$time[nchar(as.character(energiantuotanto_update$time)) <= 10]), "00:00:00", sep = " ")

        # Arrange dataframes to match each other column-wise
        dataToBeUpdated <- dataToBeUpdated %>%
          select(time, kokonaiskulutus, pientuotanto, tehoreservi, tuulivoima, vesivoima, ydinvoima, yhteistuotanto_kaukolämpö, yhteistuotanto_teollisuus, vienti, kokonaistuotanto)

        energiantuotanto_update <- energiantuotanto_update %>%
          select(time, kokonaiskulutus, pientuotanto, tehoreservi, tuulivoima, vesivoima, ydinvoima, yhteistuotanto_kaukolämpö, yhteistuotanto_teollisuus, vienti, kokonaistuotanto)

        colnames(energiantuotanto_update) <- colnames(dataToBeUpdated)

        # make sure that all columns are of the correct class
        energiantuotanto_update <- energiantuotanto_update %>%
          mutate(across(time, as.character)) %>%
          mutate(across(!time, as.numeric))

        dataToBeUpdated <- dataToBeUpdated %>%
          mutate(across(time, as.character)) %>%
          mutate(across(!time, as.numeric))

        #print(dataToBeUpdated)
        #print(energiantuotanto_update)

        #print(dataToBeUpdated$yhteistuotanto_kaukolämpö)
        #print(energiantuotanto_update$yhteistuotanto_kaukolämpö)

        #dataToBeUpdated <- rbind(dataToBeUpdated, energiantuotanto_update) %>%
        #  arrange(time)

        # Rbind the dataframes in as foolproof of a way as possible
        dataToBeUpdated <- data.table::rbindlist(list(dataToBeUpdated, energiantuotanto_update)) %>%
          arrange(time)

        # Remove duplicate entries retroactively

            temp <- dataToBeUpdated[dataToBeUpdated$time > (Sys.time() - lubridate::weeks(4)),]

            dataToBeUpdated <- dataToBeUpdated[dataToBeUpdated$time <= (Sys.time() - lubridate::weeks(4)),]

            temp <- temp[!duplicated(temp$time),]

            dataToBeUpdated <- data.table::rbindlist(list(dataToBeUpdated, temp)) %>%
              arrange(time)

        write.csv(dataToBeUpdated, "./data/energiantuotanto_dekomponoitu.csv",
                  row.names = FALSE)

        pgirmess::write.delim(as.character.POSIXt(Sys.time()), file = "data/updateCondition_decomp.txt")

        energiantuotanto_data_frame_decomp <- data.table::fread("./data/energiantuotanto_dekomponoitu.csv")

      }



      energiantuotanto_data_frame_decomp <- energiantuotanto_data_frame_decomp[as.POSIXct(energiantuotanto_data_frame_decomp$time) > as.POSIXct(input$sahkoDate[1]) &
                                                                                 as.POSIXct(energiantuotanto_data_frame_decomp$time) < Sys.time(),]

      energiantuotanto_data_frame_decomp$time <- as.POSIXct(energiantuotanto_data_frame_decomp$time, format = "%Y-%m-%d %H:%M:%S")

      print(energiantuotanto_data_frame_decomp)

    return(energiantuotanto_data_frame_decomp)

  })

  #data <- read.csv("//home.org.aalto.fi/valivia1/data/Documents/GitHub/shiny_app/data/energiantuotanto_dekomponoitu.csv")
  #data <- data[ -c(nrow(data)),]
  #write.csv(data, "//home.org.aalto.fi/valivia1/data/Documents/GitHub/shiny_app/data/energiantuotanto_dekomponoitu.csv", row.names = FALSE)


  kokonaiskulutus_kokonaistuotanto_data_fd <- reactive({

    energialahteet <- c("reaali kokonaiskulutus", "reaali kokonaistuotanto")

      energiantuotanto_data_frame_kulutus_tuotanto <- data.table::fread("./data/energiantuotanto_kulutus_tuotanto.csv")

      #energiantuotanto_data_frame_kulutus_tuotanto <- energiantuotanto_data_frame_kulutus_tuotanto[energiantuotanto_data_frame_kulutus_tuotanto$time > input$sahkoDate[1] &
      #                                                                                               energiantuotanto_data_frame_kulutus_tuotanto$time < Sys.time(),]


      #print(colnames(energiantuotanto_data_frame_kulutus_tuotanto))
      colnames(energiantuotanto_data_frame_kulutus_tuotanto) <- c("time", "tuotanto", "kulutus")

    if (checkUpdateCondition(as.POSIXct(readLines("data/updateCondition_kulutus_tuotanto.txt")[2]))) {
      #Päivitä lokaalisti säilytettävää FG:n data juioksevasti
      dataToBeUpdated <- data.table::fread("./data/energiantuotanto_kulutus_tuotanto.csv")

      energiantuotanto_update <- do.call(data.frame, lapply(energialahteet,
                                                            assign_energiantuotanto, startTime = max(as.POSIXct(dataToBeUpdated$time)),
                                                            endTime = Sys.time()))

      print(paste("Updating data at time", as.character(Sys.time()), sep = " "))

      energiantuotanto_update <- energiantuotanto_update %>%
        select(c(2, 1, 3:ncol(energiantuotanto_update))) %>% #siirrä aikasarake df:n vasempaan reunaan
        select(-grep("time.", colnames(energiantuotanto_update))) %>% #Poista redundantit aikasarakkeet
        set_colnames(c("time", gsub(" ", "_", gsub("reaali ", "", energialahteet)))) #%>%

      energiantuotanto_update$time %<>% as.character()

      #Prevent duplicate entries
      overlap <- intersect(dataToBeUpdated$time, energiantuotanto_update$time)
      #print(overlap)
      overlap_ind <- which(energiantuotanto_update$time %in% overlap)
      #print(overlap_ind)
      if (length(overlap_ind) != 0) {
        energiantuotanto_update <- energiantuotanto_update[-c(overlap_ind),]
      }
      #print(energiantuotanto_update)

      print("Update: ")
      print(energiantuotanto_update)

      dataToBeUpdated <- dataToBeUpdated %>%
        select(time, kokonaistuotanto, kokonaiskulutus)

      energiantuotanto_update <- energiantuotanto_update %>%
        select(time, kokonaistuotanto, kokonaiskulutus)

      colnames(energiantuotanto_update) <- colnames(dataToBeUpdated)

      energiantuotanto_update <- energiantuotanto_update %>%
        mutate(across(time, as.character)) %>%
        mutate(across(!time, as.numeric))

      dataToBeUpdated <- dataToBeUpdated %>%
        mutate(across(time, as.character)) %>%
        mutate(across(!time, as.numeric))

      print("Update: ")
      print(energiantuotanto_update)

      print("old:")
      print(dataToBeUpdated)

     # dataToBeUpdated <- rbind(dataToBeUpdated, energiantuotanto_update) %>%
     #  arrange(time)

      dataToBeUpdated <- data.table::rbindlist(list(dataToBeUpdated, energiantuotanto_update)) %>%
        arrange(time)

      #Remove duplicate entries retroactively. This should not be needed when the program is running normally.
      #dataToBeUpdated <- dataToBeUpdated[!duplicated(dataToBeUpdated$time),]

      write.csv(dataToBeUpdated, "./data/energiantuotanto_kulutus_tuotanto.csv",
                row.names = FALSE)

      pgirmess::write.delim(as.character.POSIXt(Sys.time()), file = "data/updateCondition_kulutus_tuotanto.txt")

      energiantuotanto_data_frame_kulutus_tuotanto <- read_csv("./data/energiantuotanto_kulutus_tuotanto.csv")

    }

      energiantuotanto_data_frame_kulutus_tuotanto <- energiantuotanto_data_frame_kulutus_tuotanto[energiantuotanto_data_frame_kulutus_tuotanto$time > as.POSIXct(input$sahkoDate[1]) &
                                                                                                     energiantuotanto_data_frame_kulutus_tuotanto$time < Sys.time(),]

      energiantuotanto_data_frame_kulutus_tuotanto$time <- as.POSIXct(energiantuotanto_data_frame_kulutus_tuotanto$time, format = "%Y-%m-%d %H:%M:%S")

    return(energiantuotanto_data_frame_kulutus_tuotanto)

  })

  viimeisin <- reactive({

    fd_viimeisin <- energiantuotanto_data_frame()[nrow(energiantuotanto_data_frame()),] %>%
      mutate(across(!time, as.numeric)) %>%
      pivot_longer(!time, names_to = "name", values_to = "value") %>%
      mutate(name = gsub("_", " ", name), keep = "unused") %>%
      mutate(name = paste0("reaali ", name), keep = "unused") %>%
      select(value, name)

    print(fd_viimeisin)

    return(fd_viimeisin)

  })

  eilen <- reactive({

    fd_eilen <- energiantuotanto_data_frame()[(nrow(energiantuotanto_data_frame()) - 24),] %>%
      mutate(across(!time, as.numeric)) %>%
      pivot_longer(!time, names_to = "name", values_to = "value") %>%
      mutate(name = gsub("_", " ", name), keep = "unused") %>%
      mutate(name = paste0("reaali ", name), keep = "unused") %>%
      select(value, name)

  })

  # valueboxit -----------------------------

  ## fingrid  -----------------------------
  output$kokonaiskulutus <- shinydashboard::renderValueBox({

    print(uusin_kulutus)

    shinydashboard::valueBox(
      paste0(tuhaterotin(round(uusin_kulutus)), " MW"),
      "Sähkön reaaliaikainen kokonaiskulutus")

  })

  output$kokonaistuotanto <- shinydashboard::renderValueBox({

    print(uusin_tuotanto)

    shinydashboard::valueBox(
      paste0(tuhaterotin(round(uusin_tuotanto)), " MW"),
      "Sähkön reaaliaikainen kokonaistuotanto")

  })

  output$tuulisuhde <- shinydashboard::renderValueBox({
    osuus <- uusin_tuuli/uusin_tuotanto

    shinydashboard::valueBox(
      prosenttierotin(round(osuus,3)),
      "Tuulivoiman osuus tämänhetkisestä sähköntuotannosta"
      )
  })

  output$muutoskulutus <- shinydashboard::renderValueBox({

   #print(vuorokausi_sitten)

    kulutus_eilen <- vuorokausi_sitten$value[vuorokausi_sitten$name == "reaali kokonaiskulutus"]

    value <- (uusin_kulutus-kulutus_eilen)/kulutus_eilen

    etumerkki <- ifelse(value > 0, "+", "")

    shinydashboard::valueBox(
      paste0(etumerkki,tuhaterotin(prosenttierotin(round(value,3)))),
      "Muutos sähkön kokonaiskulutuksessa viimeisen 24 tunnin aikana")

  })

  output$muutostuotanto <- shinydashboard::renderValueBox({

    #print(vuorokausi_sitten)

    tuotanto_eilen <- vuorokausi_sitten$value[vuorokausi_sitten$name == "reaali kokonaistuotanto"]

    value <- (uusin_tuotanto-tuotanto_eilen)/tuotanto_eilen

    etumerkki <- ifelse(value > 0, "+", "")

    shinydashboard::valueBox(
      paste0(etumerkki,tuhaterotin(prosenttierotin(round(value,3)))),
      "Muutos sähkön kokonaistuotannossa viimeisen 24 tunnin aikana")

  })

  output$nettovienti <- shinydashboard::renderValueBox({

    print(uusin_vienti)

    teksti <- ifelse(uusin_vienti > 0,
                     "Suomesta viedään sähköä",
                     "Suomeen tuodaan sähköä")

    shinydashboard::valueBox(
      paste0(tuhaterotin(round(abs(uusin_vienti)))," MW"),
      teksti)

  })

  ## desiili sivu ---------------------------------------
  output$taustaotsikko <- renderText(

    if(input$soptyyp != '-'){
      return("Taustaa tulokymmenyksistä")
    } else{
      NULL
    }

  )


  output$askumaarat <- shinydashboard::renderValueBox({

    sum <- sum(boxplotit_asuntokunnat[[input$kk]]$n)
    shinydashboard::valueBox(tuhaterotin(sum), "asuntokuntien lukumäärä")
  })



  output$dessuhde <- shinydashboard::renderValueBox({
    values <- boxplotit_asuntokunnat[[input$kk]] %>%
      filter(desiili %in% c(1,10)) %>%
      group_by(desiili) %>%
      summarise(y_mean = mean(y_mean)) %>%
      ungroup() %>%
      select(y_mean) %>%
      pull()

    shinydashboard::valueBox(tuhaterotin(round(values[2]/values[1],2)), "Korkeatuloisin desiili kulutti tässä kuussa kertaa enemmän sähköä kuin pienituloisin desiili.")
    })

  # Plotit ----------------------------------------

  ## piirakkaplot -------------------------------------------

  output$piirakkaplot <- renderPlot({

    #fd_data <- lataa_kaikki()

    #print(fd_data)

    fd_data <- viimeisin()

    plot_data <- fd_data%>%
      separate(name,c("turha", "name")) %>%
      select(-turha) %>%
      filter(!name %in% c("kokonaistuotanto",
                          "kokonaiskulutus",
                          "vienti")) %>%
      group_by(name) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(value = value / sum(value))

    viiva_data <- fd_data %>%
      separate(name,c("turha", "name")) %>%
      select(-turha) %>%
      filter(name %in% c("kokonaistuotanto",
                         "kokonaiskulutus")) %>%
      pivot_wider(names_from = name, values_from = value)

    plot_data %>%
      ggplot(aes(x="", y=value, fill = name)) +
      geom_bar(stat="identity", width = 1,size = 1, colour = "white") +
      geom_vline(
        aes(
          linetype = "kokonaiskulutus",
          xintercept = 0.5+sqrt(viiva_data$kokonaiskulutus/viiva_data$kokonaistuotanto)
        ),
        size = 1) +
      coord_polar("y", start=0) +
      scale_fill_manual(
        name=NULL,
        values = c("#721d41",
                   "#8482bd",
                   "#234721",
                   "#393594",
                   "#AED136",
                   "#f16c13"),
        labels = paste0(plot_data$name, " ", prosenttierotin(round(plot_data$value,3)))
      )+
      scale_linetype_manual(
        name = NULL,
        values = c("dashed")
      )+
      theme_void()+
      theme(
        plot.title = element_text(size = 22),
        legend.text = element_text(size= 20),
        plot.caption =  element_text(size = 14, hjust = 0)) +
      labs(title = "Sähkön tämänhetkinen tuotanto sekä kulutus",
           caption = stringr::str_wrap("Tiedot ovat Fingridin avoin data  -verkkopalvelusta ja perustuvat käytönvalvontajärjestelmän reaaliaikaisiin mittauksiin. Lisätietoja sähköntuotannosta sekä kulutuksesta voi löytää \"Reaaliaikainen sähkönkäyttötilanne\"-osiosta. Ympyröiden pinta-alojen suhde kuvaa kulutuksen sekä tuotannon suhdetta.", 80))

    })

  ## aikasarjadata --------------------------------------
  output$aikasarjaplot <- renderPlot({

    data <- aikasarja_data()

    if(input$suure == 'per capita'){

      data <- data %>%
        mutate(sahkonkul = sahkonkul / sum_ak)

      y_akseli <-"Sähkönkulutus kWh / hlö"

    } else{
      data <- data %>%
        mutate(sahkonkul = sahkonkul / 1000)

      y_akseli <- "Sähkönkulutus MWh"
    }

    data %>%
      ungroup() %>%
      filter(kuukausi >= input$aikasarja[1],
             kuukausi <= input$aikasarja[2]) %>%
      ggplot(
        aes(x = kuukausi,
            y = sahkonkul,
            colour = alue),
        alpha = 0.2
      ) +
      geom_line(size = 1)+
      gghighlight(alue %in% input$valitut, label_key = alue) +
      scale_color_brewer(palette = "Dark2") +
      scale_y_continuous(name = y_akseli,
                         label = tuhaterotin)+
      scale_x_date(name = NULL,
                   label = formatoi_kuukaudet_plot) +
      coord_cartesian(ylim = c(0,max(data$sahkonkul))) +
      theme_light() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
  })

  output$viikkoplot <- renderPlotly({

    #print(kokonaiskulutus_kokonaistuotanto_data_fd())
    #data <- kokonaiskulutus_kokonaistuotanto_data_fd()

    data <- kokonaiskulutus_kokonaistuotanto_data_fd()
    colnames(data) <- c("time", "kokonaistuotanto", "kokonaiskulutus")

    #print(n = 200,data)

    if ((difftime(Sys.time(), input$sahkoDate[1]) > weeks(4)) & ! ("Näytä kuvaaja aina tunneittain" %in% input$reaaliaikaKuvaajaAsetus)) {

       data <- data %>%
        mutate(aika = lubridate::floor_date(time, unit = "days")) %>%
        group_by(aika) %>%
        dplyr::summarise(across(c("kulutus", "tuotanto"), ~mean(.x, na.rm = TRUE)))

    } else {
      colnames(data) <- gsub("time", "aika", colnames(data))
    }

    print(data)

    data %>%
      pivot_longer(-aika, values_to = "arvo", names_to = "muuttuja") %>%
      ggplot(aes(x = aika,
                 y = arvo,
                 colour = muuttuja)) +
      geom_line(aes(y = arvo, col = muuttuja), size = 1) +
      scale_y_continuous(label = tuhaterotin)+
      scale_x_datetime(#breaks = "1 day",
                       date_labels = "%d.%m.")+
      scale_color_manual(
        name = NULL,
        labels = c("Kokonaiskulutus",
                   "Tuotanto"),
        values = c("#393594","#721d41") )+
      theme_light() +
      labs(x = NULL, y = 'MW')+
      theme(legend.position = 'bottom') +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10),
            legend.text = element_text(size= 10))

    ggplotly(tooltip = c("aika", "colour", "y"))  %>%
      layout(legend = list(orientation = "h"))

  })

  output$viikkoplot_dekomponoitu <- renderPlotly({

    #print(energiantuotanto_data_frame())

    energiantuotanto_data_frame <- energiantuotanto_data_frame() %>%
      select(-c(vienti, kokonaistuotanto))
    #print(energiantuotanto_data_frame)

    if ((difftime(Sys.time(), input$sahkoDate[1]) > weeks(4)) & !("Näytä kuvaaja aina tunneittain" %in% input$reaaliaikaKuvaajaAsetus)) {

      #energiantuotanto_data_frame[,1] %<>% as.POSIXct()

      #colnames(energiantuotanto_data_frame) <- c("time","kokonaiskulutus","pientuotanto","tehoreservi","tuulivoima",
      #                                                  "vesivoima","ydinvoima","yhteistuotanto_kaukolämpö","yhteistuotanto_teollisuus")

      #print(energiantuotanto_data_frame)

      energiantuotanto_data_frame <- energiantuotanto_data_frame %>%
        mutate(aika = lubridate::floor_date(time, unit = "days")) %>%
        group_by(aika) %>%
        dplyr::summarise(across(c("pientuotanto", "tehoreservi", "tuulivoima",
                                  "vesivoima", "ydinvoima", "yhteistuotanto_kaukolämpö", "yhteistuotanto_teollisuus",
                                  "kokonaiskulutus"), ~mean(.x, na.rm = TRUE)))

    } else {
      colnames(energiantuotanto_data_frame) <- gsub("time", "aika", colnames(energiantuotanto_data_frame))
    }

    #print(energiantuotanto_data_frame)

    energiantuotanto_data_frame %>%
      pivot_longer(cols = c(pientuotanto, tuulivoima, ydinvoima, tehoreservi, vesivoima, yhteistuotanto_kaukolämpö, yhteistuotanto_teollisuus),
                   values_to = "arvo", names_to = "muuttuja") %>%
      mutate(muuttuja = gsub("_", ", ", muuttuja)) %>%
      pivot_longer(cols = c(kokonaiskulutus),
                   names_to = "kokkul", values_to = "kokonaiskulutus") %>%

      ggplot(aes(aika)) +
      geom_col(aes(y = arvo, fill = muuttuja)) +
      scale_fill_manual(name = NULL,
                         #labels = c("pientuotanto", "tehoreservi", "tuulivoima",
                         #            "vesivoima", "ydinvoima", "yhteistuotanto, kaukolämpö", "yhteistuotanto, teollisuus"),
                         values = c("#721d41", "#CC8EA0", "#FBE802", "#F16C13", "#FFF1E0", "#AED136", "#8482BD", "#393594")) +

      geom_line(aes(y = kokonaiskulutus, col = kokkul), size = 1, show.legend = FALSE) +
      scale_color_manual(name = NULL,
                         labels = c("kokonaiskulutus"),
                         values = c("#393594",
                                    guide = "none")) +
      guides(colour = "none") +

      scale_x_continuous(label = tuhaterotin) +
      scale_x_datetime(#breaks = "1 week",
                       date_labels = "%d.%m.") +
           theme_light() +
           labs(x = NULL, y = 'MW') +
           theme(legend.position = 'bottom') +
           theme(axis.text = element_text(size = 10),
                 axis.title = element_text(size = 10),
                 legend.text = element_text(size= 10))

    ggplotly(tooltip = c("aika", "fill", "y")) %>%
      layout(legend = list(orientation = "h"))

  })

  ## Tausta kuvaajat --------------------------------------------

  output$tausta <- renderPlot({

    if (input$soptyyp == 'määräaikaiset sopimukset') {

      boxplotit_maaraik[[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = factor(is_fixed_term_agreement)))+
        geom_col(position = 'fill')+
        scale_fill_manual(
          name = "Määräaikainen sähkösopimus",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())+

        scale_y_continuous(label = prosenttierotin)

    } else if(input$soptyyp == 'lämmitys riippuvainen sähköstä') {

      boxplotit_lammitys[[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = is_heating_dependent_on_electricity))+
        geom_col(position = 'fill')+
        scale_fill_manual(
          name = "Lämmitys riippuvainen sähköstä",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)

    } else if (input$soptyyp == 'sopimuksien lukumäärä') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = yli_1_sopimus))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "Asuntokunnalla solmittu yli yksi sopimus",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)

    } else if (input$soptyyp == 'asuu taajama-alueella') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = factor(asuu_taajamassa)))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "Asuntokunta asuu taajama-alueella",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)
    } else if (input$soptyyp == 'asuu kerrostalossa') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = asuu_kerrostalossa ))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "Asuntokunta asuu kerrostalossa",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)
    } else if (input$soptyyp == 'asuntokunnan koko') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = yli_1_akkoko))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "asuntokunnan koko yli yksi henkilö",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)
    }



    })


## boxplotit ----------------------------------------
  output$boxplot <- renderPlot({



    if (input$soptyyp == 'lämmitys riippuvainen sähköstä') {
### lämmitysboxplot ----------------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(
              x = rep(c(-0.2,0.2),10)
            ),
            width = 0.2)

      }
        plot <- plot +
          geom_boxplot(
            aes(
              min = y_25,
              lower = y_25,
              middle = y_median,
              upper = y_75,
              max = y_75,
              fill = is_heating_dependent_on_electricity
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Lämmitys ei riippuvainen sähköstä',
                    'Lämmitys riippuvainen sähköstä'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )

    } else if (input$soptyyp == 'määräaikaiset sopimukset') {

### määräaikaisboxplot --------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(
              x = rep(c(-0.2,0.2),10)
            ),
            width = 0.2)
      }


        plot <- plot +
          geom_boxplot(
            aes(
              min = y_25,
              lower = y_25,
              middle = y_median,
              upper = y_75,
              max = y_75,
              fill = factor(is_fixed_term_agreement)
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(
          name = "Sähkönkulutus (kWh)",
          labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Ei määräaikaista','Määräaikainen'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    } else if (input$soptyyp == "sopimuksien lukumäärä") {

### sopimusten lukumäärä ------------------------

      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(yli_1_sopimus)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Yksi sopimus','Kaksi tai useampia'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12)
        )


    }else if (input$soptyyp == "asuu kerrostalossa") {

      ### asuu kerrostalossa ------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(asuu_kerrostalossa)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(
          name = "Sähkönkulutus (kWh)",
          labels = tuhaterotin
        ) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Ei asu kerrostalossa','asuu kerrostalossa'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    }else if (input$soptyyp == "asuu taajama-alueella") {

      ### Taajamassa asuminen ------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(asuu_taajamassa)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Ei asu taajamassa','asuu taajamassa'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    }else if (input$soptyyp == "asuntokunnan koko") {

      ### asuntokunnan koko ------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(yli_1_akkoko)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Yksi asukas','Kaksi tai useampia'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    } else {
### Normiboxplot ----------------------
    plot <- boxplot_data() %>%
      ggplot(
        aes(
          x = factor(desiili)
        )
      )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            width = 0.2)
      }

        plot <- plot +
          geom_boxplot(
            aes(
              min = y_25,
              lower = y_25,
              middle = y_median,
              upper = y_75,
              max = y_75,
              fill = 'median'
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          )
        )
      }

      if(input$locked_scale){
          max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
          plot <- plot + coord_cartesian(ylim = c(0,max_y))
        }

      plot +
      scale_y_continuous(name = "Sähkönkulutus (kWh)",
                         labels = tuhaterotin) +
      scale_x_discrete(name = "Tulokymmenys")+
      scale_fill_manual(
        name = '25 % - mediaani- 75 %',
        label = NULL,
        values = c('#F16C13',"#234721")
      )+
      scale_colour_manual(
        name = NULL,
        label = '5 % - 95 %',
        values = 'black'
      )+
      scale_alpha_manual(
        name = NULL,
        values = 1
      )+
      theme_linedraw()+
      theme(
        legend.position = 'bottom',
        panel.border =element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size= 12)
      )
    }
    })

  # ukraina ----------------------------------
  ## etusivu ---------------------------------
  output$ikaryhma <- renderPlotly({

    ## create plot
    p <- ikajakauma %>%
      rename("ikäryhmä" = "age_group") %>%
      rename("lukumäärä" = "n") %>%
      ggplot() +
      geom_col(aes(x = ikäryhmä, y =lukumäärä, fill = sukupuoli), alpha = alpha_u, position = "dodge") +
      scale_fill_manual(values = c(light_blue, orange)) +
      scale_x_discrete(name = "ikäryhmä") +
      scale_y_continuous(name = "henkilöä", labels = tuhaterotin) +
      theme_light() +
      theme(
        legend.title = element_blank(),
        text = element_text(size = font_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = font_size))

    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.5, xanchor = 'center'))

  })
  ## taustatietoja ---------------------------
  ukraina_basics_data <- reactive({

    ## filter
    if(input$vaesto == "kotikunnan saaneet") {
      data <- kotikunta
    } else {
      data <- ei_kotikuntaa
    }

    return(data)

  })

  output$basic_plot <- renderPlotly({

    data <-  ukraina_basics_data()

    if(input$jaottelu == "-") {

      p <- Ukraina_kuvaaja(data, "none", FALSE, "lukumäärä", NULL, "henkilöä", alpha_u, font_size)

    } else if(input$jaottelu == "ikäryhmä") {

      if(input$vaesto == "kotikunnan saaneet") {NULL}
      else {

      if (input$osuus) {

        p <- Ukraina_kuvaaja(data, "ikäryhmä", TRUE, "osuus", "ikäryhmä", "prosenttia", alpha_u, font_size)

      } else{

        p <- Ukraina_kuvaaja(data, "ikäryhmä", FALSE, "lukumäärä", "ikäryhmä", "henkilöä", alpha_u, font_size)

      }
      }


    } else if (input$jaottelu == "sukupuoli"){

      if (input$osuus) {

        p <- Ukraina_kuvaaja(data, "sukupuoli", TRUE, "osuus", "sukupuoli", "prosenttia", alpha_u, font_size)

      } else{

        p <- Ukraina_kuvaaja(data, "sukupuoli", FALSE, "lukumäärä", "sukupuoli", "henkilöä", alpha_u, font_size)
      }


    }

  })
  ## tyollistyminen ---------------------------
  ukraina_emp_data <- reactive({

    ## filter
    if(input$employed == "kotikunnan saaneet") {
      data <- employed_kotikunta %>%
        filter(tilasto_time > dmy("01/04/2022")) %>%
        mutate(n = if_else(tilasto_time < dmy("01/03/2023"), 0, n))

    } else {
      data <- employed %>% filter(tilasto_time > dmy("01/04/2022"))
    }

    return(data)

  })

  output$emp_plot <- renderPlotly({

    data <-  ukraina_emp_data()

    if(input$jaottelu_emp == "-") {

      ## distinct
      summary <- data %>%
        distinct(tilasto_time, n_total)

      ## plot
      p <- Ukraina_kuvaaja(summary, "none", FALSE, "lukumäärä", NULL, "henkilöä", alpha_u, font_size)

    } else if(input$jaottelu_emp == "ikäryhmä") {


        if (input$osuus_emp) {

          ## plot
          p <- Ukraina_kuvaaja(data, "ikäryhmä", TRUE, "osuus", "ikäryhmä", "henkilöä", alpha_u, font_size)


        } else{

          ## plot
          p <- Ukraina_kuvaaja(data, "ikäryhmä", FALSE, "lukumäärä", "ikäryhmä", "henkilöä", alpha_u, font_size)

        }


    } else if (input$jaottelu_emp == "sukupuoli"){


      if (input$osuus_emp) {

        ## plot
        p <- Ukraina_kuvaaja(data, "sukupuoli", TRUE, "osuus", "sukupuoli", "henkilöä", alpha_u, font_size)

      } else{

        ## plot
        p <- Ukraina_kuvaaja(data, "sukupuoli", FALSE, "lukumäärä", "sukupuoli", "henkilöä", alpha_u, font_size)

      }


    }

  })

  ## alat ja ammatit -------------------------
  ukraina_alat_ja_ammatit <- reactive({

    ## filter
    if(input$alavaiammatti == "toimialat") {

      ## yleisimmät toimialat
      top <<- toimialat %>%
        group_by(toimiala) %>%
        dplyr::summarise(n = mean(n)) %>%
        arrange(desc(n)) %>% slice(1:input$top) %>%
        pull(toimiala)
      data <- toimialat

    } else {

      ## yleisimmät ammatit
      top <<- ammatit %>%
        group_by(prof_l3) %>%
        dplyr::summarise(n = mean(n))  %>%
        arrange(desc(n)) %>% slice(1:input$top) %>%
        pull(prof_l3)
      data <- ammatit
    }

    return(data)

  })
  output$ala_ammatti_plot <- renderPlotly({

    ## get data
    data <-  ukraina_alat_ja_ammatit()



    if (input$alavaiammatti == "toimialat") {

      ## plot
      p <- Ukraina_kuvaaja(data, "toimiala", FALSE, "lukumäärä", "ala", "henkilöä", alpha_u, font_size)

    } else if (input$alavaiammatti == "ammattinimikkeet") {

      ## plot
      p <- Ukraina_kuvaaja(data, "ammatti", FALSE, "lukumäärä", "ala", "henkilöä", alpha_u, font_size)

    }

  })



  ## otsikot --------------------------------
  output$toimialat_otsikko <- renderText({

    lkm <- numerolle_teksti(input$top)

    if(lkm == "Yksi"){
      if(input$alavaiammatti == "toimialat") {
        paste0( "Yleisin toimiala")
      } else {
        paste0("Yleisin ammattinimike")
      }
    } else {
      if(input$alavaiammatti == "toimialat"){
        mika_ala <- "toimialaa"
      } else {
        mika_ala <- "ammattinimikettä"
      }
      paste0(lkm, " yleisintä ", mika_ala)
    }


  })

  output$emp_otsikko <- renderText({

    # if (input$osuus_emp & input$jaottelu_emp == "-") {
    #   abs <- "lukumäärä"
    # } else if (input$osuus_emp){
    #   abs <- "osuus"
    # } else {
    #   abs <- "lukumäärä"
    # }

    if(input$employed == "kotikunnan saaneet"){
      kotikunta <- "kotikunnan saaneet"
    } else {
      kotikunta <- ""
    }

    paste0("15–64-vuotiaat ", kotikunta, " ukrainalaiset palkansaajat")
  })

  output$taustatieto_otsikko <- renderText({

    if (input$osuus & input$jaottelu == "-") {
      abs <- "lukumäärä"
    } else if (input$osuus){
      abs <- "osuus"
    } else {
      abs <- "lukumäärä"
    }

    if(input$vaesto == "kotikunnan saaneet"){
      kotikunta <- "kotikunnan saaneiden"
    } else {
      kotikunta <- ""
    }

    paste0("1.3.2022 jälkeen saapuneiden ", kotikunta, " ukrainalaisten ", abs)
  })

  ## downloaderit --------------------------------
  output$download_taustatiedot <-downloadHandler(

    filename = function(){
      if(input$vaesto == "kotiunnan saaneet"){
        return(paste0("kotikunnan_saaneet_ukrainalaiset.csv"))
      } else {
        return(paste0("kaikki_ukrainalaiset.csv"))
      }

    },
    content = function(file){

      ## get the data
      data <- ukraina_basics_data()

      if(input$jaottelu == "-") {

        ## distinct
        summary <- data %>%
          distinct(tilasto_time, n_total) %>%
          rename(c("aika" = "tilasto_time", "n" = "n_total"))

      } else if(input$jaottelu == "ikäryhmä") {

        ## summarise
        summary <- data %>%
          group_by(tilasto_time, n_total, age_group) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2)) %>%
          rename(c("aika" = "tilasto_time", "ikäryhmä" = "age_group"))

      } else if (input$jaottelu == "sukupuoli"){

        ## summarise
        summary <- data %>%
          group_by(tilasto_time, n_total, sukupuoli) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2)) %>%
          rename(c("aika" = "tilasto_time"))

      }

      write.csv(summary, file, row.names = F, fileEncoding = "ISO-8859-1")
    }

  )

  output$download_emp <-downloadHandler(

    filename = function(){
      if(input$employed == "kotikunnan saaneet"){
        return(paste0("kotikunnan_saaneet_ukrainalaiset_palkansaajat.csv"))
      } else {
        return(paste0("kaikki_ukrainalaiset_palkansaajat.csv"))
      }

    },
    content = function(file){

      ## get the data
      data <- ukraina_emp_data()

      if(input$jaottelu_emp == "-") {

        ## distinct
        summary <- data %>%
          distinct(tilasto_time, n_total) %>%
          rename(c("aika" = "tilasto_time", "n" = "n_total"))

      } else if(input$jaottelu_emp == "ikäryhmä") {

        if(input$employed == "kotikunnan saaneet") {
          data <- data %>%
            filter(tilasto_time >  dmy("01/02/2023"))
        }

        ## summarise
        summary <- data %>%
          group_by(tilasto_time, n_total, age_group) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2)) %>%
          rename(c("aika" = "tilasto_time", "ikäryhmä" = "age_group"))

      } else if (input$jaottelu_emp == "sukupuoli"){

        if(input$employed == "kotikunnan saaneet") {
          data <- data %>%
            filter(tilasto_time >  dmy("01/02/2023"))
        }

        ## summarise
        summary <- data %>%
          group_by(tilasto_time, n_total, sukupuoli) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2)) %>%
          rename(c("aika" = "tilasto_time"))

      }

      write.csv(summary, file, row.names = F, fileEncoding = "ISO-8859-1")
    }

  )


  output$download_alat_ja_ammatit <-downloadHandler(

    filename = function(){
      if(input$alavaiammatti == "toimialat"){
        return(paste0("toimialat.csv"))
      } else {
        return(paste0("ammattinimikkeet.csv"))
      }

    },
    content = function(file){

      ## get the data
      data <- ukraina_alat_ja_ammatit()

      ## rename variables
      data <- data %>% select(-ala) %>%
        rename(any_of(c("aika" = "tilasto_time", "ammattikoodi" = "prof_l3")))

      write.csv(data, file, row.names = F, fileEncoding = "ISO-8859-1")
    }

  )


  # downloaderit ----------------------------------


  output$download_aikasarja <-downloadHandler(
    filename = function(){
      if(input$tarktaso == "Koko maa"){
        return(paste0("datahuone_fdh_kokonaiskulutus.csv"))
      } else if(input$tarktaso == "Maakunnittain"){
        return(paste0("datahuone_fdh_kokonaiskulutus_maakunnittain.csv"))
      } else {
        return(paste0("datahuone_fdh_kokonaiskulutus_kunnittain.csv"))
      }

    },
    content = function(file){

      data <- aikasarja_data() %>%
        rename(sahkon_kulutus_yht_kwh = sahkonkul,
               asukkaiden_lkm = sum_ak)

      write.csv(data, file, row.names = F, fileEncoding = "UTF-8")
    }
  )

  output$download <-downloadHandler(
    filename = function(){
      paste0("datahuone_fdh_",input$kk,".csv")
    },
    content = function(file){
      data <- boxplot_data() %>%
        rename(y_05 = y_min,
               y_95 = y_max,
               asuntokuntie_lkm = n,
               tulokymmenys = desiili)

      if(!input$mean){
        data <- data %>%
          select(-y_mean)
      }

      if(!input$error){
        data <- data %>%
          select(-c(y_min, y_max))
      }
      data <- data %>%
        rename_all(~str_replace_all(., "y_", "sahkonkaytto_"))

      write.csv(data, file, row.names = F)
    }
  )

  output$download_dekomponoitu <- downloadHandler(
    filename = function(){
      paste0("datahuone_fdh_dekomponoitu_", as.character(input$sahkoDate[1]), "_", as.character(input$sahkoDate[2]), ".csv")
    },
    content = function(file){
      data <- energiantuotanto_data_frame() #%>%
        #select(-c(vienti, kokonaistuotanto)) # TODO: MINE DATA UP TO 2019!
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(
  ui = ui,
  server = server
  )
