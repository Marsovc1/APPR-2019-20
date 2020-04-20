library(httr)
library(XML)

transfer = list()
for (i in 10:19){
  url <- paste0("https://en.wikipedia.org/wiki/20",i,"%E2%80%93",i+1,"_FC_Barcelona_season") #link
  r <- GET(url) #pobere vse kar lahko o strani
  tabele <- readHTMLTable(doc=content(r, "text")) #poskuša prebrati vse html tabele na strani
  
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

ime = list()
for (i in seq(1,21,2)){
  ime[i] <- paste0("20",(i+1)/2+9," Transfer In")
  ime[i+1] <- paste0("20",(i+1)/2+9, " Transfer Out")
  if (i == 21){
    ime[i] <- "2019 Transfer In"
    ime[i+1] <- "2019 Transfer Out"
    ime[i+2] <- "2019 Loan Out" 
  }
  ime[19] <- "2018 Loan In"
  ime[20] <- "2018 Loan Out"
}
names(transfer) <- ime

for (i in 1:23){
  ime = names(transfer[i])
  dat = paste0(ime,".Rda")
  path = file.path("C:", "Users", "marsovc", "Documents", "Analiza Transferjev FC Barcelona", "podatki", dat)
  save(ime, file = path)
}