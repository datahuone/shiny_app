## Taustaa datasta

### Avoin sähködata
<font size="4"> 
Avoin data on peräisin Fingridin hallinnoimasta Avoin Data API:sta, joka tarjoaa reaaliaikaista sähköjärjestelmän dataa avoimessa muodossa. API sisältää tietoa esimerkiksi sähkönkulutuksesta, tuotannosta, siirrosta ja hintakehityksestä Suomessa. Tämän avoimen datan käyttö mahdollistaa reaaliaikaisen seurannan sähköjärjestelmän tilasta ja kulutuksesta.

Tämä API on löydettävissä osoitteesta [https://data.fingrid.fi/fi/](https://data.fingrid.fi/fi/).
</font> 

### Asuntokuntakohtainen sähkönkäyttö 
<font size="4"> 
Kotitalouksien mittarikohtainen sähkönkulutus saadaan Fingrid Datahub -aineistosta. Tiedostoissa esitetään luvut joulukuun 2022 kulutuksesta. Mittaripisteen sähkösopimus yhdistetään asuntokuntiin sähkösopimuksen haltijan pseudonymisoidun henkilönumeron avulla.
</font> 


#### Asuntokuntien määrittely
<font size="4"> 
Asuntokunnat on määritelty Tilastokeskuksen väestön ennakkotietojen mukaan. Asuntokunnan kotikunta määritellään vakituisen osoitteen sijaintikunnan perusteella. Asuntokunnan sähkönkulutus lasketaan kaikkien asuntokunnan hallussa olevien sähkösopimusten kulutuksesta, sillä asuntokunnalla voi olla useita sähkösopimuksia. Suurimmalla osalla asuntokunnista on kuitenkin vain yksi vakituiseen osoitteeseen tehty sähkösopimus. Aineistossa käsitellään vain niitä asuntokuntia, joilla on havaittu sähkösopimus ja vakituinen osoite. Aineistossa havaitaan noin 2,5 miljoonaa asuntokuntaa, kun taas noin 300 000 asuntokunnalla ei ole sähkösopimusta. Esimerkkejä sähkösopimuksettomista asuntokunnista ovat ne, joilla vuokranantaja tai joku muu taho maksaa sähkön.

Asuntokunnat on jaettu tulokymmenyksiin henkilöiden perusteella siten, että kaikille asuntokunnan henkilöille on allokoitu asuntokunnan yhteenlasketut käytettävissä olevat tulot, ja henkilöt on järjestetty tulojen mukaiseen järjestykseen ja jaettu kymmeneen yhtä suureen joukkoon. Näin yhdessä tulokymmenyksessä on kymmenesosa henkilöistä, ei asuntokunnista.

Asuntokunta määritellään sähkölämmittäjäksi, jos Fingridin tietojen mukaan sopimus, josta on havaittu eniten kulutusta, on sähkölämmitteisessä talossa. Mikäli asuntokunnalla on hallussaan useampi asunto, määritellään asuinmuoto myös sen mukaan, mistä suurin kulutus tulee. Mikäli suurin kulutus tulee kerrostaloasunnosta, valitaan asuntokunta kerrostaloasujaksi. 

</font> 

