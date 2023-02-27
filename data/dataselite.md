# Taustaa datasta

### Avoin sähködata

Avoin data on peräisin Fingridin hallinnoimasta Avoin Data API:sta, joka tarjoaa reaaliaikaista sähköjärjestelmän dataa avoimessa muodossa. API sisältää tietoa esimerkiksi sähkönkulutuksesta, tuotannosta, siirrosta ja hintakehityksestä Suomessa. Tämän avoimen datan käyttö mahdollistaa reaaliaikaisen seurannan sähköjärjestelmän tilasta ja kulutuksesta.

Tämä API on löydettävissä osoitteesta [https://data.fingrid.fi/fi/](https://data.fingrid.fi/fi/).

### Säädata

Säädatan lähteenä käytämme Ilmatieteen laitoksen ylläpitämää fmi2 R-pakettia, joka mahdollistaa API-yhteydenoton Ilmatieteen laitoksen palveluihin. Tarkemmat tiedot tä'stä R-paketista voi löytää github-sivuilta [https://github.com/rOpenGov/fmi2](https://github.com/rOpenGov/fmi2).

### Asuntokuntakohtainen sähkönkäyttö 

kotitalouksien mittarikohtainen sähkönkulutus saadaan Fingrid Datahub -aineistosta. Tiedostoissa esitetään luvut joulukuun 2022 kulutuksesta. Mittaripisteen sähkösopimus yhdistetään asuntokuntiin sähkösopimuksen haltijan pseudonymisoidun henkilönumeron avulla.

#### Asuntokuntien määrittely:

Asuntokunnat on määritelty Tilastokeskuksen väestön ennakkotietojen mukaan. Asuntokunnan kotikunnaksi on määritelty vakituisen osoitteen sijaintikunta. Asuntokunnan sähkönkulutukseksi on laskettu kaikki asuntokunnan hallussa olevien sähkösopimusten kulutus, eli asuntokunnalla voi olla useita sähkösopimuksia. Suurimmalla osalla asuntokunnista on kuitenkin vain yksi, vakituiseen osoitteeseen tehty, sähkösopimus. Mukana laskelmissa ovat vain ne asuntokunnat, joilla havaitaan sähkösopimus ja vakituinen osoite. Aineistossa havaitaan noin 2,5 miljoonaa asuntokuntaa, kun taas noin 300 000 asuntokunnalla ei ole sähkösopimusta. Sähkösopimuksettomia asuntokuntia ovat esimerkiksi ne, joilla vuokranantaja tai joku muu taho maksaa sähkön.

Asuntokunnat on jaettu tulokymmenyksiin henkilöiden perusteella siten, että asuntokunnan kaikille henkilöille on allokoitu asuntokunnan yhteenlasketut käytettävissä olevat tulot, ja henkilöt on järjestetty tulojen mukaiseen järjestykseen ja jaettu kymmeneen yhtä suureen joukkoon. Näin yhdessä tulokymmenyksessä on kymmenesosa henkilöistä, ei asuntokunnista.

Asuntokunta on määritelty sähkölämmittäjäksi, mikäli Fingridin tietojen mukaan sopimus josta on havaittu eniten kulutusta on sähkölämmitteisessä talossa. Myös asuinmuoto on määritelty sen mukaan mikäli suurin kulutus tulee asunnosta joka on kerrostalo.

