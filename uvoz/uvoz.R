# 2. faza: Uvoz podatkov

options(warn=-1)

library(dplyr)
library(rvest)
library(magrittr)

lct <- Sys.getlocale("LC_TIME") #da bomo lahko vrnili stvar na svoje mesto
Sys.setlocale("LC_TIME", "C") #sicer date=(NA,NA,...)

#funkcija za ciscenje cudnih vrednosti npr fee: €75,000,000[B] => €75,000,000
pocisti <- function(podatki){
  podatki<- gsub("\\[|\\]",'',podatki)
  podatki<- gsub("€",'',podatki)
  podatki<- gsub(",",'',podatki)
  vec <- vector() 
  for (i in podatki){
    #is numeric ne deluje ker substr vrne string in je npr. "5" string
    if(grepl("[[:digit:]]",(substr(i,nchar(i),nchar(i))))){
      vec <- c(vec,i)
    }
    else{
      vec <- c(vec,substr(i,1,nchar(i)-1))
    }
  }
  return(vec)
}

# Funkcija, ki uvozi rojstne datume igralcev
uvozRD <- function(i){
  j=2
  if (i==19){j=3}
  
  link <- paste0("https://en.wikipedia.org/wiki/20",i,"%E2%80%93",(i+1),"_FC_Barcelona_season")
  stran <- html_session(link) %>% read_html()
  
  #pojavi se nam problem starosti igralca ob podpisu pogodbe, 
  #zato moramo za igralca pogledati njegovo stran (a href) in dobiti rojstni datum <span class="bday">
  linkIN <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j]]
  linkOUT <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[j+1]]
  linkIN <- linkIN %>% html_nodes('a')
  linkOUT <- linkOUT %>% html_nodes('a')
  linkIN <- linkIN %>% html_attr('href')
  linkOUT <- linkOUT %>% html_attr('href')
  
  #niso vsi href igralci, treba na roke preveriti
  if (i==18){
    linkIN <- linkIN[c(2,8,13,19,25)]
    linkOUT <- linkOUT[c(3,10,17,23,30,37,44,50,58,65)]
  }
  
  if (i==19){
    linkIN <- linkIN[c(3,10,16,22,27,32,38)]
    linkOUT <- linkOUT[c(3,9,14,20,24,30,37,43,48,53,58,64,69)]
  }
  
  rodIN <- seq(1,length(linkIN),1)
  rodOUT <- seq(1,length(linkOUT),1)
  
  for (i in seq(1,length(linkIN),1)){
    #sedaj iz linkov pogledam stran in dobim RD
    linkRD <- paste0('https://en.wikipedia.org',linkIN[i])
    stranRD <- html_session(linkRD) %>% read_html()
    #infobox je okno na desni kjer so podatki o osebi
    RD <- stranRD %>% html_nodes(xpath="//table[@class='infobox vcard']") %>% .[[1]]
    #tukaj imam podatek o rojstvu
    RD <- RD %>% html_nodes(xpath="//span[@class='bday']")
    RD <- html_text(RD)
    rodIN[i] <- RD
  }
  for (i in length(linkOUT)){
    linkRD <- paste0('https://en.wikipedia.org',linkOUT[i])
    stranRD <- html_session(linkRD) %>% read_html()

    RD <- stranRD %>% html_nodes(xpath="//table[@class='infobox vcard']") %>% .[[1]]
    RD <- RD %>% html_nodes(xpath="//span[@class='bday']")
    RD <- html_text(RD)
    rodOUT[i] <- RD
  }
  return(list(rodIN,rodOUT))
}

#funkcija ki v csv uvozi transferje
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
    #koliko je igralec star? rabim link
    
    dolzinaOUT <- length(flagOUT)
    #vse lihe do dolzine nodeset
    vzemiOUT <- seq(1, dolzinaOUT, 2)
    flagOUT <- flagOUT[vzemiOUT]
    
    #sedaj moramo s pomočjo funkcije uvozRD ugotoviti koliko so bili igralci stari ob prestopu
    #to naredimo tako, da spremenimo datume v as.date nato pogledamo as.numeric(difftime...)
    #uvoz rojstnih datumov
    rod <- uvozRD(i)
    rodIN <- rod[1]
    rodOUT <- rod[2]
    
    #uredimo obliko
    tabelaIN$`Entry date` <- pocisti(tabelaIN$`Entry date`)
    tabelaOUT$`Exit date` <- pocisti(tabelaOUT$`Exit date`)
    
    #shranimo kot as.date
    rodIN <- as.Date(rodIN[[1]], '%Y-%m-%d')
    rodOUT <- as.Date(rodOUT[[1]], '%Y-%m-%d')
    rodTabelaIN <- as.Date(tabelaIN$`Entry date`, '%d %B %Y')
    rodTabelaOUT <- as.Date(tabelaOUT$`Exit date`, '%d %B %Y')
    
    #pogledamo time diff v letih
    rodIN <- as.numeric(difftime(rodTabelaIN,rodIN,units="weeks")/52.25)
    rodOUT <- as.numeric(difftime(rodTabelaOUT,rodOUT,units="weeks")/52.25)
    
    #ga zaokrožimo navzdol da dobimo starost
    tabelaIN$Starost <- floor(rodIN)
    tabelaOUT$Starost <- floor(rodOUT)

    tabelaIN <- tabelaIN[c(4,ncol(tabelaIN),2,5,6)]
    tabelaOUT <- tabelaOUT[c(4,ncol(tabelaOUT),2,5,6)]
  
    tabelaIN$Drzavljanstvo <- flagIN %>% html_attr(., "title")
    tabelaOUT$Drzavljanstvo <- flagOUT %>% html_attr(., "title")
      
    names <- c('Ime', 'Starost', 'Polozaj', 'Klub', 'Vrednost transferja', 'Drzavljanstvo')
    names(tabelaIN) <- names
    names(tabelaOUT) <- names

    #funkcijo pocisti uporabimo se na vrednostih transferjev
    tabelaIN$`Vrednost transferja` <- pocisti(tabelaIN$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- pocisti(tabelaOUT$`Vrednost transferja`)
    
    #vrednosti free zamenjamo z 0
    tabelaIN$`Vrednost transferja` <- gsub('Free transfer',0,tabelaIN$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub('Free transfer',0,tabelaOUT$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub('Loan Retur',0,tabelaIN$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub('Loan Retur',0,tabelaOUT$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub('fre',0,tabelaIN$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub('fre',0,tabelaOUT$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub('Fre',0,tabelaIN$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub('Fre',0,tabelaOUT$`Vrednost transferja`)
    
    #vrednosti undisclosed zamenjamo z -1
    tabelaIN$`Vrednost transferja` <- gsub("Undisclosed",'-1000000',tabelaIN$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub("Undisclosed",'-1000000',tabelaOUT$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub("Undisclose",'-1000000',tabelaIN$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub("Undisclose",'-1000000',tabelaOUT$`Vrednost transferja`)

    #dodaj se 'loan out' in 'loan in', ki sta posebni tabeli
    
    tabelaIN$`Vrednost transferja` <- (as.numeric(tabelaIN$`Vrednost transferja`)/1000000)
    tabelaOUT$`Vrednost transferja` <- (as.numeric(tabelaOUT$`Vrednost transferja`)/1000000)
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
    tabelaIN <- tabelaIN[c(4,5,2,7,11,13)]
    tabelaOUT <- tabelaOUT[c(4,5,2,7,10,12)]
    names <- c('Ime', 'Starost', 'Polozaj', 'Klub', 'Vrednost transferja', 'Drzavljanstvo')
    names(tabelaIN) <- names
    names(tabelaOUT) <- names
    
    #posebno ciscenje za vrednosti transferjev: crka -> '', + -> ' ', € -> '', \ -> ''
    tabelaIN$`Vrednost transferja` <- gsub("Undisclosed",'-1',tabelaIN$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub("[[:punct:]]\\d+[[:punct:]]",'',tabelaIN$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub("\\[|\\]",'',tabelaIN$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub("[[:alpha:]]",'',tabelaIN$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub('\\+',',',tabelaIN$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub("€",'',tabelaIN$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub("/",'',tabelaIN$`Vrednost transferja`)
    tabelaIN$`Vrednost transferja` <- gsub(" ",'',tabelaIN$`Vrednost transferja`)
    
    tabelaOUT$`Vrednost transferja` <- gsub("Undisclosed",'-1',tabelaOUT$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub("[[:punct:]]\\d+[[:punct:]]",'',tabelaOUT$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub("\\[|\\]",'',tabelaOUT$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub("[[:alpha:]]",'',tabelaOUT$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub('\\+',',',tabelaOUT$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub("€",'',tabelaOUT$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub("/",'',tabelaOUT$`Vrednost transferja`)
    tabelaOUT$`Vrednost transferja` <- gsub(" ",'',tabelaOUT$`Vrednost transferja`)

    
    #split po ',' in sestejem, če prazno vrnem 0
    vredIN = vector()
    for (j in tabelaIN$`Vrednost transferja`){
      j <- toString(j)
      if(j==""){
        vredIN <- c(vredIN,0)
      }
      else{
        val <- (strsplit(j,','))[[1]]
        vredIN <- c(vredIN,(cumsum(val))[length(val)])
      }
    }
    
    vredOUT = vector()
    for (j in tabelaOUT$`Vrednost transferja`){
      if(j==""){
        vredOUT <- c(vredOUT,0)
      }
      else{
        val <- (strsplit(j,','))[[1]]
        vredOUT <- c(vredOUT,(cumsum(val))[length(val)])
      }
    }
    
    tabelaIN$`Vrednost transferja` <- vredIN
    tabelaOUT$`Vrednost transferja` <- vredOUT
  }
  
  tabelaIN$`Leto` <- rep(2000+i, nrow(tabelaIN))
  tabelaOUT$`Leto` <- rep(2000+i, nrow(tabelaOUT))
  
  pathIN = paste0("./podatki/",'transferIN',i,'.csv')
  pathOUT = paste0("./podatki/",'transferOUT',i,'.csv')
  write.csv(tabelaIN,pathIN, row.names = TRUE)
  write.csv(tabelaOUT,pathOUT, row.names = TRUE)
}

#funkcijo izvedemo za leta 10-11,...,19-20
for(i in seq(10,19,1)){uvoz(i)}
Sys.setlocale("LC_TIME", lct) #vrnimo stvar na svoje mesto