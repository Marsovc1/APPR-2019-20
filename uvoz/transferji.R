library(httr)
library(XML)

#strani po posebnostih: 10;11,12,13,14;15,16,17;18,19
#10,...,17 2 tabeli IN in OUT
#18,19 4 tabele Transfer in & out in Loan in & out



#10,...,14: in = doc[7], out = doc[8]
#15,16,17: in = doc[6], out = doc[7]
#18: doc[7],...,doc[10]
#19: doc[5],...,doc[7]

transfer = list()
for (i in 10:19){
  url <- paste0("https://en.wikipedia.org/wiki/20",i,"%E2%80%93",i+1,"_FC_Barcelona_season")
  r <- GET(url)
  tabele <- readHTMLTable(doc=content(r, "text"))
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