# 2. faza: Uvoz podatkov

library(dplyr)
library(rvest)
library(magrittr)

# Funkcija, ki uvozi občine iz Wikipedije
uvoz <- function(i){
  j=2
  if (i %in% c(13,19)){
    j=3
  }
  link <- paste0("https://en.wikipedia.org/wiki/20",i,"%E2%80%93",(i+1),"_FC_Barcelona_season")
  stran <- html_session(link) %>% read_html()
  tabelaIN <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j]] %>% html_table(dec=",")
  tabelaOUT <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j+1]] %>% html_table(dec=",")
  
  if (i>17){
    #zadnja vrstica je Total je ne rabim:
    tabelaIN<- tabelaIN[-nrow(tabelaIN),]
    tabelaOUT<- tabelaOUT[-nrow(tabelaOUT),]
    
    #flagicon je ob igralcu, ker je tudi ob klubu poberemo vse flagicon in vzamemo lihe, sicer isto kot else{}
    flagIN <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j]]
    flagOUT <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j+1]]
    
    flagIN <- flagIN %>% html_nodes(xpath=".//span[@class='flagicon']")
    flagOUT <- flagOUT %>% html_nodes(xpath=".//span[@class='flagicon']")
    
    flagIN <- flagIN %>% html_nodes('a')
    flagOUT <- flagOUT %>% html_nodes('a')
    dolzinaIN <- length(flagIN)
    #vse lihe do dolzine nodeset
    vzemiIN <- seq(1, dolzinaIN, 2)
    flagIN <- flagIN[vzemiIN]
    
    dolzinaOUT <- length(flagOUT)
    #vse lihe do dolzine nodeset
    vzemiOUT <- seq(1, dolzinaOUT, 2)
    flagOUT <- flagOUT[vzemiOUT]
    
    
    tabelaIN <- tabelaIN[c(4,2,5,6)]
    tabelaOUT <- tabelaOUT[c(4,2,5,6)]
    tabelaIN$Drzavljanstvo <- flagIN %>% html_attr(., "title")
    tabelaOUT$Drzavljanstvo <- flagOUT %>% html_attr(., "title")
    names <- c('Ime', 'Polozaj', 'Klub', 'Vrednost transferja', 'Drzavljanstvo')
    names(tabelaIN) <- names
    names(tabelaOUT) <- names
  }
  
  else{
    #posebnost je drzavljanstvo saj je pod title linka zastavice (flagicon)
    flagIN <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j]]
    flagOUT <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j+1]]
    
    #če <td><span class="flagicon"><a href="/wiki/ potem državljanstvo sicer lahko tudi država kluba
    flagIN <- flagIN %>% html_nodes(xpath=".//td[not(@style='text-align:left;')]")
    flagOUT <- flagOUT %>% html_nodes(xpath=".//td[not(@style='text-align:left;')]")
    #ima flagicon 
    flagIN <- flagIN %>% html_nodes(xpath=".//span[@class='flagicon']")
    flagOUT <- flagOUT %>% html_nodes(xpath=".//span[@class='flagicon']")
    
    #title se nahaja v 'a' (link)
    flagIN <- flagIN %>% html_nodes('a')
    flagOUT <- flagOUT %>% html_nodes('a')
    #atribut title dodamo kot drzavljanstvo
    tabelaIN$Drzavljanstvo <- flagIN %>% html_attr(., "title")
    tabelaOUT$Drzavljanstvo <- flagOUT %>% html_attr(., "title")
    
    #razvrstimo in preimenujemo
    tabelaIN <- tabelaIN[c(4,5,2,7,8,11,13)]
    tabelaOUT <- tabelaOUT[c(4,5,2,7,8,10,12)]
    names <- c('Ime', 'Starost', 'Polozaj', 'Klub', 'Tip transferja', 'Vrednost transferja', 'Drzavljanstvo')
    names(tabelaIN) <- names
    names(tabelaOUT) <- names
  }
  pathIN = file.path("C:", "Users", "marsovc", "Documents", "Analiza Transferjev FC Barcelona", "podatki", paste0('transferIN',i,'.csv'))
  pathOUT = file.path("C:", "Users", "marsovc", "Documents", "Analiza Transferjev FC Barcelona", "podatki", paste0('transferOUT',i,'.csv'))
  write.csv(tabelaIN,pathIN, row.names = TRUE)
  write.csv(tabelaOUT,pathOUT, row.names = TRUE)
}
#funkcijo izvedemo za leta 10-11,...,19-20
#for(i in seq(10,19,1)){uvoz(i)}
