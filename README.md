
# N.B. Branch kehitys on kesken

N.B.! Alla oleva teksti on luonnos projektin tulevasta rakenteesta, joka elää tällä hetkellä tässä branchissä. Rakenne on muutettu alla kuvatun mukaiseksi, mutta muutokset eivät ole vielä valmiita. Tärkeintä on muokata R/ui.R ja R/server.R tiedostopolut  siten, että ne ovat suhteessa R/server.R tiedostoon! Lisäksi funktiot on eroteltu omiksi useampaan eri tiedostoon, ja näiden polut pitää korjata, ennen kuin ohjelma toimii.

*When Shiny runs the commands in server.R, it will treat all file paths as if they begin in the same directory as server.R. In other words, the directory that you save server.R in will become the working directory of your Shiny app.* https://shiny.posit.co/r/getstarted/shiny-basics/lesson5/

# Datahuone Kojelauta

Sivusto on löydetävissä osoitteesta [https://datahuone.shinyapps.io/dataholvi/](https://datahuone.shinyapps.io/dataholvi/). 

Tulevaisuudessa sivustolle olisi tarkoitus lisätä myös muita Datahuoneen rutiiniraportoitavia tietoja. Pyrimme jatkuvasti kehittämään sivustoa vastaamaan käyttäjien tarpeita ja tarjoamaan helposti saatavilla olevaa ja käyttökelpoista tietoa.

# Hakemistorakenne

## data/

Kansio Fionassa käsitellyn aineiston säilytykseen.

## cache/

Kansio mahdollisesti edelleen prosessoidun aineiston ja sisäisten aineistojen säilytykseen. 

## R

Sisältää shiny applikaation toimintaan liittyvän ohjelmakoodin. Huomaa, että raa'an aineiston
käsittely suoritetaan Fionassa. Kansio on jaettu hakemistoihin projektin perusteella ja yleiskäyttöiset funktiot sijaitsevat kansiossa `utils`.

### R/electricity/

Sisältää sähkökulutustietojen visualisointia ja muokkausta koskevat skriptit ja funktiot.

### R/labourmarket/

Sisältää työmarkkinatietojen visualisointia ja muokkausta koskevat skriptit ja funktiot.

### R/utils/

Sisältää yleisesti käytettyjen apufunktioiden määrittelyt.

### ui.R

Sisältää Shiny-applikaation user interface -objektiin liittyvän ohjelmakoodin.

### server.R

Sisältää Shiny-applikaation server-objektiin liittyvän ohjelmakoodin.

## content

Sisältää projektikohtaiset markdown-tiedostot.

### content/electricity/

Sisältää sähkönkulutustietoja kuvailevat markdown-tiedostot.

### content/labourmarket/

Sisältää työmarkkinatietoja kuvailevat markdown-tiedostot.

## icons/

Sisältää Shiny-applikaatioon lisättävät logot ja ikonit.

## tests/

Sisältää R-kansiossa sijaitsevien funktioiden mahdolliset testit.

## app.R

Shiny-applikaation käyttöliittymä, joka kutsuu tarvittavia ohjelman osia. Tämä
skripti käynnistää Shiny-applikaation.

# Nykyiset raportit

## Sähkönkulutustiedot

Visualisointien tavoitteena on tuoda nähtäville Suomen kotitalouksien sähkönkulutustietoja. Tiedot on kerätty Fingrid Datahubista, joka on Suomen sähköjärjestelmän keskitetty tietojärjestelmä. Lisäksi tietoihin on yhdistetty Tilastokeskuksen rekisteriaineistoja, jolloin käyttäjät voivat tutkia sähkönkulutuksen tilastotietoja monipuolisesti. Tämä verkkosivusto on suunniteltu helpoksi käyttää ja kaikki tiedot ovat avoimesti saatavilla.

Tavoitteenamme on tarjota tietoa, joka auttaa kansalaisia ymmärtämään paremmin Suomen sähköjärjestelmää ja edistää energiatehokkaita valintoja. Tervetuloa tutustumaan sivustoon ja oppimaan lisää Suomen kotitalouksien sähkönkäytöstä!

## Tilapäisen suojelun piirissä olevat ukrainalaiset Suomessa
