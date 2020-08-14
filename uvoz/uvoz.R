# 2. faza: Uvoz podatkov

library(dplyr)
library(rvest)

# Funkcija, ki uvozi občine iz Wikipedije

link <- "https://en.wikipedia.org/wiki/2010%E2%80%9311_FC_Barcelona_season"
stran <- html_session(link) %>% read_html()
tabelaIN <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[2]] %>% html_table(dec=",")
tabelaOUT <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[3]] %>% html_table(dec=",")

#razvrstimo in preimenujemo
tabelaIN <- tabelaIN[c(4,5,2,7,8,11)]
tabelaOUT <- tabelaOUT[c(4,5,2,7,8,10)]
names <- c('Ime', 'Starost', 'Polozaj', 'Klub', 'Tip transferja', 'Vrednost transferja')
names(tabelaIN) <- names
names(tabelaOUT) <- names

#če <td><span class="flagicon"><a href="/wiki/ potem državljanstvo sicer lahko tudi država kluba
flag <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[2]] %>% html_nodes(xpath=".//span[@class='flagicon']")
