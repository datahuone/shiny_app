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

Sys.setlocale("LC_ALL", "Finnish_Finland.1252")

### ladataan data ----------------
kuukaudet <- feather::read_feather(
  "data/kuukaudet_saatavilla.feather")$kuukaudet

#ladataan sopimusdata
boxplotit_sopimukset <- lataa_data("sopimuksittain_boxplotit",
                                           kuukaudet)

#ladatataan asuntokuntadata
boxplotit_asuntokunnat <- lataa_data("asuntokunnittain_boxplotit",
                                             kuukaudet)

boxplotit_maaraik <- lataa_data("asuntokunnittain_maaraaik_boxplotit",
                                        kuukaudet)

boxplotit_lammitys <- lataa_data("asuntokunnittain_lammitysmuoto_boxplotit",
                                         kuukaudet)

kunta_kvantiilit <- lataa_data("kunta_kvantiilit",
                               kuukaudet)

kunnat <- feather::read_feather("data/kunnat.feather")

aikasarja_data_raw <- feather::read_feather("data/aikasarjat/kulutus_kk.feather")

Kunnan_nimet <- kunnat %>%
  distinct(kunnan_nimi)



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

          p("Tervetuloa VATT Datahuoneen Shiny-appiin! Alla esittelyt lyhyesti jokaisen sivun sisältämistä tiedoista. Tämä auttaa sinua saamaan paremman käsityksen sovelluksen tarjoamista mahdollisuuksista ja hyödyntämään sitä tehokkaasti."),
          h2("Kotitalouksien sähkönkulutus - Fingrid Datahubin tilastotietojen tarkastelu"),
          width = 10)),
      fluidRow(
        column(
          p("Täältä voit tutkia Suomen kotitalouksien sähkönkäyttöä Fingrid Datahubin tilastotietojen avulla, jotka on yhdistetty Tilastokeskuksen rekisteriaineistoihin. Sivulla Reaaliaikainen sähkönkäyttötilanne on mahdollista nähdä reaaliaikaisia tioetoja "),

          p("Sivustollamme on mahdollista tarkastella Suomen kotitalouksien sähkönkäyttöä eri tarkastelutasoilla. Sivulla Kotitalouksien kokonaiskulutuksen trendit voi tarkastella eri maantieteellisten alueiden, kokonaissähkönkulutusta sekä per henkilökulutusta."),

          p("Sivulla Sosioekonomisten muuttujien vaikutus voi tarkastella erilaisten sosioekonomisten muuttujien vaikutusta sähkönkulutukseen. Näihin muuttujiin kuuluvat esimerkiksi asuntokuntien tulotaso, asumismuoto ja koko. Tämä tarkastelutapa auttaa ymmärtämään, mitkä tekijät vaikuttavat eniten sähkönkulutukseen ja miten ne korreloivat keskenään."),

          p("Kolmannella sivulla Kuntakohtainen tarkastelu on mahdollista tarkastella asuinkuntien mukaan. Tällä sivulla voit tarkistaa, miten oman asuntokuntasi sähkönkulutus vertautuu muihin asuinkuntasi asuntokuntiin. Tämä antaa hyvän käsityksen siitä, millä tasolla oma sähkönkulutus suhteutuu paikalliseen keskiarvoon ja millaisia erotuksia on eri kuntien välillä."),
          p("Tavoitteenamme on tarjota käyttäjille mahdollisimman monipuolinen ja kattava näkymä Suomen kotitalouksien sähkönkulutukseen. Tiedot perustuvat Fingrid Datahubin tilastotietoihin, joita olemme yhdistäneet Tilastokeskuksen rekisteriaineistoihin. Toivomme, että sivustomme auttaa käyttäjiä ymmärtämään paremmin sähkönkulutuksen jakautumista ja siihen vaikuttavia tekijöitä eri tarkastelutasoilla."),
          p("Huomioithan, että sivustomme kehitys on edelleen käynnissä ja jos havaitset ongelmia tai huomaat virheitä, otathan yhteyttä sähköpostiosoitteeseen theo.blauberg@vatt.fi tai voit vaihtoehtoisesti luoda bugiraportin",
            tags$a( href ='https://github.com/bbtheo/Shiny_app_datahuone', 'GitHub-sivuillemme.'),"Kiitos yhteistyöstä!"),

          width = 10))
      )
    ),

  # sähköjutut ---------------------------
  navbarMenu(
    title = "Kotitalouksien sähkönkulutus",

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
        )
      )
    ),
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
          )
        ),
        mainPanel(
          fluidRow(h1("Sähkönkäytön trendit Suomessa")),
          fluidRow(
            column(width = 1),
            column(plotOutput("aikasarjaplot"), width = 10),
            column(width = 1)
            )
          )
        )
      ),
    ## desiilipaneeli --------------------------------
    tabPanel(
      title = "Sosioekonomisten muuttujien vaikutus",
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
              choices = c("-",
                          "määräaikaiset sopimukset",
                          "lämmitysmuodot"),
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
              h3(textOutput("taustaotsikko"))
              ),
            fluidRow(
              column(width = 1),
              column(
                plotOutput("tausta"),
                width = 10
              ),
              column(width = 1)
              ),
            fluidRow(
              column(width = 8),
              downloadButton("download", "Lataa csv")
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

      )
 ),
  navbarMenu(
    title = "Lisätietoja",
    tabPanel(
      title = "Datalähteet",
      h2("Oletukset datan taustalla:"),
      column(width = 1),
      column('Työn alla', width = 10),
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
    if (input$soptyyp == "sopimukset") {
      # jos halutaan sopimukset palautetaan sopimusdata
      return(boxplotit_sopimukset[[input$kk]])
    }

    else if (input$soptyyp == "määräaikaiset sopimukset") {
      return(boxplotit_maaraik[[input$kk]])
    }

    else if (input$soptyyp == "lämmitysmuodot") {
      return(boxplotit_lammitys[[input$kk]])
    }

    return(boxplotit_asuntokunnat[[input$kk]])
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

    arvo <- lataa_viimeisin_fingrid("reaali kokonaiskulutus")

    shinydashboard::valueBox(
      paste0(tuhaterotin(round(arvo)), " MW"),
      "Sähkön reaaliaikainen kokonaiskulutus")

  })

  output$kokonaistuotanto <- shinydashboard::renderValueBox({

    arvo <- lataa_viimeisin_fingrid("reaali kokonaistuotanto")

    shinydashboard::valueBox(
      paste0(tuhaterotin(round(arvo)), " MW"),
      "Sähkön reaaliaikainen kokonaistuotanto")

  })

  output$tuulisuhde <- shinydashboard::renderValueBox({
    osuus <- lataa_viimeisin_fingrid("reaali tuulivoima") / lataa_viimeisin_fingrid("reaali kokonaistuotanto")

    shinydashboard::valueBox(
      prosenttierotin(round(osuus,3)),
      "Tuulivoiman osuus tämänhetkisestä sähköntuotannosta"
      )



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
      filter(desiili_new2 %in% c(1,10)) %>%
      group_by(desiili_new2) %>%
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

  output$tausta <- renderPlot({

    if (input$soptyyp == 'määräaikaiset sopimukset') {

      boxplotit_maaraik[[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili_new2),
          y = n,
          fill = factor(IsFixedTermAgreement)))+
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

    } else if(input$soptyyp == 'lämmitysmuodot') {

      boxplotit_lammitys[[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili_new2),
          y = n,
          fill = IsHeatingDependentOnElectricity))+
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

  output$boxplot <- renderPlot({


    ### lämmitysboxplot ----------------------------------
    if (input$soptyyp == 'lämmitysmuodot') {

      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili_new2)
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
              fill = IsHeatingDependentOnElectricity
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili_new2),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_lammitys, function(x) max(x$y_max)))
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

    }

    else if (input$soptyyp == 'määräaikaiset sopimukset') {

### määräaikaisboxplot --------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili_new2)
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
              fill = factor(IsFixedTermAgreement)
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili_new2),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_lammitys, function(x) max(x$y_max)))
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


    } else {
### Normiboxplot ----------------------
    plot <- boxplot_data() %>%
      ggplot(
        aes(
          x = factor(desiili_new2)
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
            x = factor(desiili_new2),
            alpha = 'Keskiarvo'
          )
        )
      }

      if(input$locked_scale){
          max_y <- max(sapply(boxplotit_lammitys, function(x) max(x$y_max)))
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

  output$download <-downloadHandler(
    filename = function(){
      paste0("datahuone_fdh_",input$kk,".csv")
    },
    content = function(file){
      data <- boxplot_data() %>%
        rename(y_05 = y_min,
               y_95 = y_max,
               asuntokuntie_lkm = n,
               tulokymmenys = desiili_new2)

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
