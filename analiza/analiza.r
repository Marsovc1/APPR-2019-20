# 4. faza: Analiza podatkov
library("tidyverse")

prihod <- read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki','transferIN10.csv'))
for (i in 11:19){
  prihod <- rbind(prihod, read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki',paste0('transferIN',i,'.csv'))))
}
  
odhod <- read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki','transferOUT10.csv'))
for (i in 11:19){
  odhod <- rbind(odhod, read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki',paste0('transferOUT',i,'.csv'))))
}

#število prestopov
#gibanje povprečne starosti
#katere pozicije
#katere države so bolj prestopane -> zemljevid
#povprečna cena in gibanje skozi leto (vpliv prestopa Neymarja)
#razlika med ceno prihodov in odhodov (naraščanje?)
#vložki kluba

