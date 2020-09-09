# 4. faza: Analiza podatkov
library("tidyverse")

#=================================================================
#uvozim in zdruzim tabele
prihod <- read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki','transferIN10.csv'))
for (i in 11:19){
  prihod <- rbind(prihod, read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki',paste0('transferIN',i,'.csv'))))
}

odhod <- read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki','transferOUT10.csv'))
for (i in 11:19){
  odhod <- rbind(odhod, read.csv(file = file.path('~','Analiza Transferjev FC Barcelona','podatki',paste0('transferOUT',i,'.csv'))))
}

#=================================================================
#stevilo prestopov, povprecna vrednost, povprecna starost, skupna vrednost prestopov

prestopi <- data.frame("Leto" = 2010:2019)

steviloIn <- prihod %>% group_by(Leto, add = FALSE) %>%  summarise(n = n())
steviloOut <- odhod %>% group_by(Leto, add = FALSE) %>%  summarise(n = n())
prestopi$stevilo_prihodov <- steviloIn$n
prestopi$stevilo_odhodov <- steviloOut$n

avgIn <- prihod %>% group_by(Leto, add = FALSE) %>%  summarize(n = round(mean(Vrednost.transferja, na.rm=TRUE),2))
avgOut <- odhod %>% group_by(Leto, add = FALSE) %>%  summarize(n = round(mean(Vrednost.transferja, na.rm=TRUE),2))
prestopi$Povprecna_vrednost_prihoda <- avgIn$n
prestopi$Povprecna_vrednost_odhoda <- avgOut$n

avgIn <- prihod %>% group_by(Leto, add = FALSE) %>%  summarize(n = round(mean(Starost, na.rm=TRUE),1))
avgOut <- odhod %>% group_by(Leto, add = FALSE) %>%  summarize(n = round(mean(Starost, na.rm=TRUE),1))
prestopi$Povprecna_starost_prihoda <- avgIn$n
prestopi$Povprecna_starost_odhoda <- avgOut$n

avgIn <- prihod %>% group_by(Leto, add = FALSE) %>%  summarize(n = sum(Vrednost.transferja))
avgOut <- odhod %>% group_by(Leto, add = FALSE) %>%  summarize(n = sum(Vrednost.transferja))
prestopi$Skupna_vrednost_prihodov <- avgIn$n
prestopi$Skupna_vrednost_odhodov <- avgOut$n

rownames(prestopi) <- prestopi[,1]
prestopi <- prestopi[-1]

#=================================================================
#polozaji
polozaj <- data.frame("Leto" = 2010:2019)

#popravimo, kadar je stevilo vrstic manjse od 10 (npr za 2012 ni prestopov vezistov -> ni vrstice -> dodam 2012 0)
popravi <- function(polozaj_in){
  if (nrow(polozaj_in)!=10){
    popravek <- setdiff(2010:2019,polozaj_in$Leto)
    df <- tibble('Leto'=popravek,'n'=0)
    popravek<- rbind(polozaj_in,df)
    popravek <- popravek %>% arrange(popravek,'Leto')
    return(popravek)
  }
  else{return(polozaj_in)}
}

polozaj_in <- prihod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='FW'|Polozaj=='LW') %>% summarise(n = n())
polozaj_in <- popravi(polozaj_in)
polozaj$Napadalec_prihod <- polozaj_in$n

polozaj_in <- prihod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='MF'|Polozaj=='CM'|Polozaj=='DM') %>% summarise(n = n())
polozaj_in <- popravi(polozaj_in)
polozaj$Vezist_prihod <- polozaj_in$n

polozaj_in <- prihod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='DF'|Polozaj=='RB') %>% summarise(n = n())
polozaj_in <- popravi(polozaj_in)
polozaj$Branilec_prihod <- polozaj_in$n

polozaj_in <- prihod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='GK') %>% summarise(n = n())
polozaj_in <- popravi(polozaj_in)
polozaj$Vratar_prihod <- polozaj_in$n

polozaj_out <- odhod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='FW') %>% summarise(n = n())
polozaj_out <- popravi(polozaj_out)
polozaj$Napadalec_odhod <- polozaj_out$n

polozaj_out <- odhod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='MF'|Polozaj=='CM') %>% summarise(n = n())
polozaj_out <- popravi(polozaj_out)
polozaj$Vezist_odhod <- polozaj_out$n

polozaj_out <- odhod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='DF'|Polozaj=='LB'|Polozaj=='RB'|Polozaj=='LWB'|Polozaj=='CB') %>% summarise(n = n())
polozaj_out <- popravi(polozaj_out)
polozaj$Branilec_odhod <- polozaj_out$n

polozaj_out <- odhod %>% group_by(Leto, add = FALSE) %>% filter(Polozaj=='GK') %>% summarise(n = n())
polozaj_out <- popravi(polozaj_out)
polozaj$Vratar_odhod <- polozaj_out$n

rownames(polozaj) <- polozaj[,1]
polozaj <- polozaj[-1]

#=================================================================
#katere drzave so bolj prestopane -> zemljevid

drzave_prihodi <- data.frame("Leto" = 2010:2019)
drzave_odhodi <- data.frame("Leto" = 2010:2019)

drzave_prihodi_vrednost <- data.frame("Leto" = 2010:2019)
drzave_odhodi_vrednost <- data.frame("Leto" = 2010:2019)

drzave_in <- unique(prihod$Drzavljanstvo)
drzave_out <- unique(odhod$Drzavljanstvo)

for (i in drzave_in){
  df <- prihod %>%group_by(Leto,add=FALSE) %>% filter(Drzavljanstvo==i)%>%summarise(n=n())
  df <- popravi(df)
  drzave_prihodi[toString(i)] <- df$n
  
  df <- prihod %>%group_by(Leto,add=FALSE) %>% filter(Drzavljanstvo==i)%>%summarise(n = sum(Vrednost.transferja))
  df <- popravi(df)
  drzave_prihodi_vrednost[toString(i)] <- df$n
}
rownames(drzave_prihodi) <- drzave_prihodi[,1]
drzave_prihodi <- drzave_prihodi[-1]
drzave_prihodi<- drzave_prihodi[ , order(names(drzave_prihodi))]

rownames(drzave_prihodi_vrednost) <- drzave_prihodi_vrednost[,1]
drzave_prihodi_vrednost <- drzave_prihodi_vrednost[-1]
drzave_prihodi_vrednost<- drzave_prihodi_vrednost[ , order(names(drzave_prihodi_vrednost))]

for (i in drzave_out){
  df <- odhod %>%group_by(Leto,add=FALSE) %>% filter(Drzavljanstvo==i)%>%summarise(n=n())
  df <- popravi(df)
  drzave_odhodi[toString(i)] <- df$n
  
  df <- odhod %>%group_by(Leto,add=FALSE) %>% filter(Drzavljanstvo==i)%>%summarise(n = sum(Vrednost.transferja))
  df <- popravi(df)
  drzave_odhodi_vrednost[toString(i)] <- df$n
}
rownames(drzave_odhodi_vrednost) <- drzave_odhodi_vrednost[,1]
drzave_odhodi_vrednost <- drzave_odhodi_vrednost[-1]
drzave_odhodi_vrednost<- drzave_odhodi_vrednost[ , order(names(drzave_odhodi_vrednost))]
#==============================================================================================
#poglejmo skupno stevilo
drzave <- bind_rows(prihod,odhod)
drzave_imena <- unique(drzave$Drzavljanstvo)

drzave_vsi <- data.frame("Leto" = 2010:2019)

for (i in drzave_imena){
  df <- drzave %>%group_by(Leto,add=FALSE) %>% filter(Drzavljanstvo==i)%>%summarise(n=n())
  df <- popravi(df)
  drzave_vsi[toString(i)] <- df$n
}

rownames(drzave_vsi) <- drzave_vsi[,1]
drzave_vsi <- drzave_vsi[-1]
drzave_vsi<- drzave_vsi[ , order(names(drzave_vsi))]

#==============================================================================================
#transponiramo dfje za vizualizacijo

drzave_odhodi <- as.data.frame(t(drzave_odhodi))
drzave_prihodi <- as.data.frame(t(drzave_prihodi))
drzave_vsi <- as.data.frame(t(drzave_vsi))
drzave_odhodi_vrednost <- as.data.frame(t(drzave_odhodi_vrednost))
drzave_prihodi_vrednost <- as.data.frame(t(drzave_prihodi_vrednost))

drzave_vsi <- tibble::rownames_to_column(drzave_vsi, "Drzave")
colnames(drzave_vsi) <- c('Drzave',2010:2019)


drzave_prihodi <- tibble::rownames_to_column(drzave_vsi, "x")
drzave_prihodi <- drzave_prihodi[,-1]

drzave_odhodi <- drzave_odhodi[-1,]
drzave_odhodi <- tibble::rownames_to_column(drzave_odhodi, "Drzave")
colnames(drzave_odhodi) <- c('Drzave',2010:2019)

drzave_prihodi_vrednost <- tibble::rownames_to_column(drzave_prihodi_vrednost, "Drzave")
drzave_odhodi_vrednost <- tibble::rownames_to_column(drzave_odhodi_vrednost, "Drzave")

#=================================================================
#shranimo izluscene/'analizirane' podatke v CSV-je

path = "./analiza/"

write.csv(drzave_odhodi,paste0(path,'drzave_odhodi.csv'), row.names = TRUE)
write.csv(drzave_prihodi,paste0(path,'drzave_prihodi.csv'), row.names = TRUE)
write.csv(drzave_vsi,paste0(path,'drzave_vsi.csv'), row.names = TRUE)
write.csv(drzave_odhodi_vrednost,paste0(path,'drzave_odhodi_vrednost.csv'), row.names = TRUE)
write.csv(drzave_prihodi_vrednost,paste0(path,'drzave_prihodi_vrednost.csv'), row.names = TRUE)
write.csv(polozaj,paste0(path,'polozaj.csv'), row.names = TRUE)
write.csv(prestopi,paste0(path,'prestopi.csv'), row.names = TRUE)
