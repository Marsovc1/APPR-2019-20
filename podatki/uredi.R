library(tidyverse)

###############################################################################################################
#sedaj uredimo tabele predvsem dodamo državljanstva, ki bodo nekoliko zoprna, saj niso v tabeli kot ime ampak href*
#pomagamo si z regularnimi izrazi

i=10
  datotekaIN = paste0("20", i, " Transfer In", ".csv")
  datotekaOUT = paste0("20", i, " Transfer Out", ".csv")
  file_locationIN = file.path("C:","Users","marsovc","Documents","Analiza Transferjev FC Barcelona","podatki",datotekaIN)
  file_locationOUT = file.path("C:","Users","marsovc","Documents","Analiza Transferjev FC Barcelona","podatki",datotekaOUT)

  df = read.csv(file_locationIN,header=TRUE)
  neki = df[1,]