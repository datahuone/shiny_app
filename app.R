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

source("funktiot.R", encoding = 'UTF-8')

#Sys.setlocale("LC_ALL", "Finnish_Finland.1252")

### ladataan data ----------------
kuukaudet <- feather::read_feather("data/kuukaudet_saatavilla.feather")$kuukaudet

#ladataan boxplot datat
boxplotit_sopimukset <- lataa_data("asuntokunnittain_sopimustenlkm_boxplotit",kuukaudet)
boxplotit_asuntokunnat <- lataa_data("asuntokunnittain_boxplotit",kuukaudet)
boxplotit_maaraik <- lataa_data("asuntokunnittain_maaraaik_boxplotit",kuukaudet)
boxplotit_lammitys <- lataa_data("asuntokunnittain_lammitysmuoto_boxplotit", kuukaudet)
boxplotit_taajama <- lataa_data("asuntokunnittain_taajama_boxplotit", kuukaudet)
boxplotit_kerrostalo <- lataa_data("asuntokunnittain_kerrostalo_boxplotit", kuukaudet)
boxplotit_askoko <- lataa_data("asuntokunnittain_askoko_boxplotit", kuukaudet)

boxplotlista <- list(
  "-" = boxplotit_asuntokunnat,
  "sopimuksien lukumäärä" = boxplotit_sopimukset,
  "määräaikaiset sopimukset"= boxplotit_maaraik,
  "lämmitys riippuvainen sähköstä" = boxplotit_lammitys,
  "asuu taajama-alueella" = boxplotit_taajama,
  "asuu kerrostalossa" = boxplotit_kerrostalo,
  "asuntokunnan koko"= boxplotit_askoko
)


kunta_kvantiilit <- lataa_data("kunta_kvantiilit",
                               kuukaudet)

kunnat <- feather::read_feather("data/kunnat.feather")

aikasarja_data_raw <- feather::read_feather("data/aikasarjat/kulutus_kk.feather")

Kunnan_nimet <- kunnat %>%
  distinct(kunnan_nimi)

uusin_kulutus <- lataa_viimeisin_fingrid("reaali kokonaiskulutus")
uusin_tuotanto <- lataa_viimeisin_fingrid("reaali kokonaistuotanto")
uusin_tuuli <- lataa_viimeisin_fingrid("reaali tuulivoima")
uusin_vienti <- lataa_viimeisin_fingrid("reaali vienti")

viikko_data_fd <- lataa_viikko_fingridistä() %>%
  arrange(desc(time)) %>%
  slice(which(row_number() %% 20 == 1)) %>% #otetaan vain yksi havainto joka tunnilta
  mutate(time = lubridate::ymd_hms(time)) %>%
  mutate(time = lubridate::floor_date(time, unit = "hours"))


vuorokausi_sitten <- viikko_data_fd %>%
  filter(time == lubridate::floor_date(
    Sys.time()-lubridate::days(1),
    unit = "hours"))


# UI -------------------------------------
ui <- navbarPage(

  tags$head(
    tags$link(rel = "icon",
              type = "image/png",
              sizes = "32x32",
              href = "DH_pikkulogo.png"),
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
        "
        )
      )
    ),

    # Application title
      title =  div(img(src="Datahuone_graafi_littee.png", height = 50)),
      windowTitle = "Datahuone",




  tabPanel(

    # Etusivu -----------------------------------------------
    title = "Etusivu",
    fluidPage(
      fluidRow(
        column(
          h1('VATT Datahuone'),

          p("Tervetuloa VATT Datahuoneen Shiny-appiin! Alla esittelyt lyhyesti jokaisen osion sisältämistä tiedoista. Tämä auttaa sinua saamaan paremman käsityksen sovelluksen tarjoamista mahdollisuuksista ja hyödyntämään sitä tehokkaasti."),
          h2("Kotitalouksien sähkönkulutus - Fingrid Datahubin tilastotietojen tarkastelu"),
          width = 10)),
      fluidRow(
        column(
          p("Täältä voit tarkastella Suomalaisten sähkönkäyttöä Fingrid Datahubin tilastotietojen avulla, jotka on yhdistetty Tilastokeskuksen rekisteriaineistoihin."),

          p("Osiossa Reaaliaikainen sähkönkäyttötilanne on reaaliaikaisia tietoja sähkön käytöstä sekä tuotannosta. Nämä tiedot ovat  Fingridin avoin data-verkkopalvelusta. Näihin reaaliaikaisiin lukuihin kuuluvat kaikki sähkönkäyttäjät."),

          p("Suomen kotitalouksien sähkönkäyttöä on mahdollista tarkastella eri tarkastelutasoilla. Osiossa Kotitalouksien kokonaiskulutuksen trendit tietoja esitellään maantieteellisten alueiden kokonaissähkönkulutusta sekä per henkilö sähkönkulutusta."),

          p("Osiossa Sosioekonomisten muuttujien vaikutus esitellään sosioekonomisten muuttujien vaikutusta sähkönkulutukseen. Näitä muuttujia ovat esimerkiksi asuntokuntien tulotaso, asumismuoto sekä koko."),

          p("Kuntakohtainen tarkastelu nimisessä osiossa voit tarkistaa, miten oman asuntokuntasi sähkönkulutus vertautuu muihin asuinkuntasi asuntokuntiin. Tämä antaa hyvän käsityksen siitä, millä tasolla oma sähkönkulutus suhteutuu paikalliseen keskiarvoon ja millaisia erotuksia on eri kuntien välillä."),

          p("Tavoitteemme on tarjota käyttäjille mahdollisimman monipuolinen ja kattava näkymä Suomen kotitalouksien sähkönkulutukseen. Tiedot perustuvat Fingrid Datahubin tilastotietoihin, joita olemme yhdistäneet Tilastokeskuksen rekisteriaineistoihin. Lisäksi näiden tietojen tueksi olemme kerrännet yhteen Toivomme, että sivustomme auttaa käyttäjiä ymmärtämään paremmin sähkönkulutuksen jakautumista ja siihen vaikuttavia tekijöitä eri tarkastelutasoilla."),

          p("Huomioithan, että sivustomme kehitys on edelleen käynnissä ja jos havaitset ongelmia tai huomaat virheitä, otathan yhteyttä sähköpostiosoitteeseen theo.blauberg@vatt.fi tai voit vaihtoehtoisesti luoda bugiraportin",
            tags$a( href ='https://github.com/bbtheo/Shiny_app_datahuone', 'GitHub-sivuillemme.')),

          width = 10))
      )
    ),

  # sähköjutut ---------------------------
  navbarMenu(
    title = "Kotitalouksien sähkönkulutus",

    ### aikasarjapaneeli ----------------------
    tabPanel(
      title = "Kotitalouksien kokonaiskulutuksen trendit",
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
          fluidRow(h1("Kotitalouksien sähkönkäyttö")),
          fluidRow(
            column(plotOutput("aikasarjaplot"), width = 11),
            )
          )
        )
      ),
    ## desiilipaneeli --------------------------------
    tabPanel(
      title = "Sosioekonomisten muuttujien tarkastelu",
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
              valueBoxOutput("sopmaarat", width = 4),
              valueBoxOutput("askumaarat", width = 4),
              valueBoxOutput("dessuhde", width = 4)
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
 ## kuntapaneeli ------------------------
    tabPanel(
      title = "Kuntakohtainen tarkastelu",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            'kotkunt',
            label = "Kotikuntasi",
            choices = Kunnan_nimet %>% filter(kunnan_nimi != "Luhanka") %>% pull(),
            selected = "Helsinki"),
          selectInput(
            'kunt_kk',
            label = 'Kuukausi',
            choices = sort(kuukaudet),
            selected = max(kuukaudet)
          ),
          numericInput(
            "kwh_kulutus",
            label = "Syötä asuntokuntasi kokonaissähkönkulutus verrataksesi muihin kotikuntasi asuntokuntiin:",
            value = 100,
            min = 0,
            max = 10000,
            step = 50)
        ),
        mainPanel(
          fluidRow(
            valueBoxOutput("kulutus_desiili", width = 4),
            valueBoxOutput("kunta_mediaanit", width = 4),
            valueBoxOutput("kuntien_suhteet", width = 4)
          ),
          fluidRow(
            column(width = 1),
            column(
              plotOutput("kuntakuvaaja"),
              width = 10
            ),
            column(width = 1)
          ),
        )
        )

      ),
     tabPanel(
       title = "Reaaliaikainen sähkönkäyttötilanne",
       fluidPage(
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
           column(plotOutput("viikkoplot"), width = 10)
         ),
         fluidRow(
           column(
             p("Lähde: Fingridin avoin data-verkkopalvelu"),width = 4
           )
         )
       )
     )
 ),
  navbarMenu(
    title = "Lisätietoja",
    tabPanel(
      title = "Taustaa datasta",
      h2("Oletukset datan taustalla:"),
      column(width = 1),
      column(includeMarkdown("data/dataselite.md"), width = 10),
      column(width = 1)
      ),
    tabPanel(
      title = "Linkkejä",
      "Työn alla"
      )
    )
  )





# SERVERI ------------------------------------------------
server <- function(input, output, session) {


  # Reaktiiviset datasetit ----------------------

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



  # valueboxit -----------------------------

  ## fingrid  -----------------------------
  output$kokonaiskulutus <- shinydashboard::renderValueBox({

    shinydashboard::valueBox(
      paste0(tuhaterotin(round(uusin_kulutus)), " MW"),
      "Sähkön reaaliaikainen kokonaiskulutus")

  })

  output$kokonaistuotanto <- shinydashboard::renderValueBox({

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

    kulutus_eilen <- vuorokausi_sitten %>% select(kulutus) %>% pull()

    value <- (uusin_kulutus-kulutus_eilen)/kulutus_eilen

    etumerkki <- ifelse(value > 0, "+", "")

    shinydashboard::valueBox(
      paste0(etumerkki,tuhaterotin(prosenttierotin(round(value,3)))),
      "Muutos sähkön kokonaiskulutuksessa viimeisen 24 tunnin aikana")

  })

  output$muutostuotanto <- shinydashboard::renderValueBox({

    tuotanto_eilen <- vuorokausi_sitten %>% select(tuotanto) %>% pull()
    value <- (uusin_tuotanto-tuotanto_eilen)/tuotanto_eilen

    etumerkki <- ifelse(value > 0, "+", "")

    shinydashboard::valueBox(
      paste0(etumerkki,tuhaterotin(prosenttierotin(round(value,3)))),
      "Muutos sähkön kokonaistuotannossa viimeisen 24 tunnin aikana")

  })

  output$nettovienti <- shinydashboard::renderValueBox({

    teksti <- ifelse(uusin_vienti > 0,
                     "Suomesta viedään sähköä",
                     "Suomeen tuodaan sähköä")

    shinydashboard::valueBox(
      paste0(tuhaterotin(round(abs(uusin_vienti)))," MW"),
      teksti)

  })
  ## desiili sivu ---------------------------------------
  output$sopmaarat <- shinydashboard::renderValueBox({

    sum <- sum(boxplotit_sopimukset[[input$kk]]$n)

    shinydashboard::valueBox(tuhaterotin(sum), "sopimuksien lukumäärä")
  })


  output$taustaotsikko <- renderText(

    if(input$soptyyp != '-'){
      return("Taustaa tulokymmenistä")
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

    shinydashboard::valueBox(tuhaterotin(round(values[2]/values[1],2)), "Korkeatuloisin desiili kuluttaa kertaa enemmän sähkkön kuin pienituloisin desiili.")
    })


  ## kuntasivu -------------------------------------------------------

  output$kulutus_desiili <- shinydashboard::renderValueBox({
    value <- mean(kunta_data()[input$kunt_kk] <= input$kwh_kulutus)

    shinydashboard::valueBox(prosenttierotin(value-0.05),
                             "Kotikuntasi asuntokunnista\nkuluttaa vähemmän sähköä")
  })

  output$kunta_mediaanit <- shinydashboard::renderValueBox({
    median <- kunta_kvantiilit[[input$kunt_kk]] %>%
      filter(kunnan_nimi == input$kotkunt) %>%
      select(Q_50) %>%
      pull()

    shinydashboard::valueBox(paste0(round(median), ' kWh'), paste0("Kotikuntasi asuntokuntien mediaanikulutus ",
                                            kuukaudet_suom(input$kunt_kk),"ssa "))
  })

  output$kuntien_suhteet <- shinydashboard::renderValueBox({
    median <- kunta_kvantiilit[[input$kunt_kk]] %>%
      filter(kunnan_nimi == input$kotkunt) %>%
      select(Q_50) %>%
      pull()

    kuntien_lkm <- sum(kunta_kvantiilit[[input$kunt_kk]]$Q_50 <= median)

    shinydashboard::valueBox(paste0(kuntien_lkm,'.'), "Pienin mediaanikulutus Suomessa")
  })

  # Plotit ----------------------------------------

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
      coord_cartesian(ylim = c(0,max(data$sahkonkul)))
  })

  output$viikkoplot <- renderPlot({

    viikko_data_fd %>%
      pivot_longer(-time) %>%
      ggplot(aes(x = time,
                 y = value,
                 colour = name,
                 group = name)) +
      geom_line(size = 1.5)+
      scale_x_datetime(breaks = "1 day",
                       date_labels = "%d.%m.")+
      scale_color_manual(
        name = NULL,
        labels = c("Kokonaiskulutus",
                   "Tuotanto"),
        values = c("#393594","#721d41") )+
      theme_light() +
      labs(x = NULL, y = 'MW')+
      theme(legend.position = 'bottom')

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
          panel.grid.minor.x = element_blank())+
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
          panel.grid.minor.x = element_blank())+
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
          panel.grid.minor.x = element_blank())+
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
          panel.grid.minor.x = element_blank())+
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
          panel.grid.minor.x = element_blank())+
        scale_y_continuous(label = prosenttierotin)
    }



    })

  output$kuntakuvaaja <- renderPlot({

    kunta_data() %>%
      pivot_longer(-kvantiili) %>%
      group_by(name) %>%
      mutate(value_diff = value - lag(value, default = value[1]),
             kvantiili = factor(kvantiili, sort(unique(kvantiili), decreasing = TRUE))) %>%
      ungroup() %>%
      ggplot(aes(x = as.Date(name),
                 y = value_diff,
                 fill = kvantiili),
             colour = 'black')+
      geom_area(position = 'stack')+
      geom_point(aes(
        x = as.Date(input$kunt_kk),
        y = input$kwh_kulutus,
        alpha = "Sinun kulutuksesi"),
        size = 5,
        colour = "red") +
      guides(fill="none")+
      scale_alpha_manual(name = NULL,
                         values = 1) +
      scale_x_date(name = NULL,
                   label = formatoi_kuukaudet_plot) +
      scale_y_continuous(name = "Sähkönkulutus kWh",
                         label = tuhaterotin)+
      theme(legend.position = 'bottom') +
      scale_fill_viridis_d()
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
          plot.caption = element_text(hjust = 0)
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
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
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
          plot.caption = element_text(hjust = 0)
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
          plot.caption = element_text(hjust = 0)
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
            fill = factor(asuu_kerrostalossa )
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
          plot.caption = element_text(hjust = 0)
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
          plot.caption = element_text(hjust = 0)
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
          plot.caption = element_text(hjust = 0)
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
        plot.caption = element_text(hjust = 0)
      )
    }
    })

  # downloaderit ----------------------------------

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
}

# Run the application
shinyApp(ui = ui, server = server)
