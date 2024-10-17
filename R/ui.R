# Description:  Generates the user interface (ui) of the shiny app
# Usage:        Run using the app.R script the project root
# Output:       User interface object
# Author:
# Date:         2024-08-27
#
# Dependencies ----

library(dplyr)
library(lubridate)
library(shiny, warn.conflicts = F)
library(plotly)

# Local attributes ----

# URL osoitteet

lisaa_logo <- F # lisää datahuoneen logo yläoikealle

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

#haetaan kuukaudet mistä sähkönkäyttöädataa
kuukaudet <- feather::read_feather("data/kuukaudet_saatavilla.feather") %>%
  arrange(kuukaudet) %>%
  pull()

# Run ----

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
                width = 12)#, Jos reaaliaikainen piirakkakuvaaja palautetaan Shinyyn, tämän osion leveys tulee muuttaa kahdestatoista kuuteen
              #column(
              #plotOutput("piirakkaplot"),
              #  width = 6)
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
                            "Asukaskohtainen kulutus")
              ),
              checkboxInput(
                inputId = 'aikasarjaViivat',
                label = 'Lisää kuvaajaan jakaumaviivat (5% ja 95%)',
                value = TRUE
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
              ),
              fluidRow(h2("Kotitalouksien sähkönkulutus tuloluokittain, jakaumakuvio")),
              fluidRow(
                column(plotOutput("aikasarjaplot_viikset"), width = 11),
              ),
              fluidRow(
                downloadButton("download_viiksiplot", "Lataa csv")
              )
            )
          )
        ),
        ## desiilipaneeli --------------------------------
        tabPanel(
          title = "Sosioekonomiset muuttujat",
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
              p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon. \nHuom! Dataa ei ole saatavilla vuoden 2023 toukokuulta")
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
        #tabPanel(

        #shinyjs::useShinyjs(),

        #title = "Reaaliaikainen sähkönkäyttötilanne",
        #value = sah_reaaliaikainen_url,
        #sidebarLayout(
        # sidebarPanel(
        #   dateRangeInput(
        #     "sahkoDate", "Valitse aikaväli:",
        #     start = Sys.time()-lubridate::weeks(1),
        #     end = Sys.time(),
        #     min = lubridate::as_datetime("27-11-2019", format = "%d-%m-%Y"),
        #     max = Sys.time(),
        #     separator = "-"
        #   ),

        #   checkboxGroupInput("reaaliaikaKuvaajaAsetus", "",
        #                      c("Lukitse kuvaaja tuntitasolle"), selected = NA),

        #   p("Voit muokata esitysmuotoa yllä olevilla asetuksilla. Kuvaajan oletusasetus on muuttaa tarkasteluaikaväli päiviin, kun valittu aikaväli on pidempi kuin kuukausi.
        #     Tätä asetusta voi muuttaa, mutta kuvaaja saattaa tällöin latautua hitaasti. Ladattavaan dataan vaikuttaa ainoastaan valittu aikaväli."),

        #   actionButton("resetSahko", "Palauta oletusasetukset")
        # ),

        # mainPanel(
        #   fluidRow(
        #     h1("Reaaliaikainen sähkönkäyttötilanne")
        #   ),
        #   fluidRow(
        #     valueBoxOutput("kokonaiskulutus", width = 4),
        #     valueBoxOutput("kokonaistuotanto", width = 4),
        #     valueBoxOutput("tuulisuhde", width = 4)
        #   ),
        #   fluidRow(
        #     valueBoxOutput("muutoskulutus", width = 4),
        #     valueBoxOutput("muutostuotanto", width = 4),
        #     valueBoxOutput("nettovienti", width = 4)
        # ),
        #   fluidRow(h2("Sähkön kulutus sekä tuotanto Suomessa")),
        #   fluidRow(
        #     column(plotlyOutput("viikkoplot"), width = 10)
        #   ),
        #   fluidRow(
        #     column(plotlyOutput("viikkoplot_dekomponoitu"), width = 10)
        #   ),
        #   fluidRow(
        #     column(
        #       p("Lähde: Fingridin avoin data -verkkopalvelu"),width = 4
        #     )
        #   ),

        # fluidRow(
        #    downloadButton("download_dekomponoitu", "Lataa csv")
        #   )
        #  )


        # )

        #),

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

# Clean-up ----
rm(list = c())
gc()
