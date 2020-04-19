library(httr)
library(XML)

transfer = list()
for (i in 10:19){
  url <- paste0("https://en.wikipedia.org/wiki/20",i,"%E2%80%93",i+1,"_FC_Barcelona_season")
  r <- GET(url)
  tabele <- readHTMLTable(doc=content(r, "text"))
  #ker ni vedno ista tabela po vrsti v kateri so podatki oziroma jih je lahko več moramo postaviti if stavke
  if (i == 15){ 
    transfer <- c(transfer,tabele[6],tabele[7])
  }
  else if (i == 13){
    transfer <- c(transfer,tabele[5],tabele[6])
  }
  else if (i == 18){
    transfer <- c(transfer,tabele[7],tabele[8],tabele[9],tabele[10])
  }
  else if (i == 19){
    transfer <- c(transfer,tabele[5],tabele[6],tabele[7]) 
  }
  else{
    transfer <- c(transfer,tabele[7],tabele[8])
  }
}