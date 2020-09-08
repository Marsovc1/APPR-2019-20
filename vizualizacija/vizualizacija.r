# 3. faza: Vizualizacija podatkov

library(tidyverse)

#graf, ki ga želimo narisati print(graf)

#================================================================================================
#vrednosti prestopov

prestopi <- read.csv("./analiza/prestopi.csv")

prestopi_stevilo <- prestopi %>% select(X,Število_prihodov, Število_odhodov)
prestopi_stevilo <- reshape2::melt(prestopi_stevilo, id.var = 'X')

prestopi_stevilo_plot <- ggplot(prestopi_stevilo, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
                          geom_point()+xlab("Leto")+ylab("Število prestopov")+
                          scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

prestopi_cena <- prestopi %>% select(X,Povprecna_vrednost_prihoda,Povprecna_vrednost_odhoda)
prestopi_cena <- reshape2::melt(prestopi_cena, id.var = 'X')

prestopi_cena_plot <- ggplot(prestopi_cena, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
                      geom_point()+xlab("Leto")+ylab("Povprečna cena prestopov")+
                      geom_smooth(method='lm',se=F)+
                      scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))


prestopi_starost <- prestopi %>% select(X,Povprecna_starost_prihoda,Povprecna_starost_odhoda)
prestopi_starost <- reshape2::melt(prestopi_starost, id.var = 'X')

prestopi_starost_plot <- ggplot(prestopi_starost, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
  geom_point()+xlab("Leto")+ylab("Povprečna starost prestopov")+
  scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

prestopi_skupna <- prestopi %>% select(X,Skupna_vrednost_prihodov,Skupna_vrednost_odhodov)
prestopi_skupna <- reshape2::melt(prestopi_skupna, id.var = 'X')

prestopi_skupna_plot <- ggplot(prestopi_skupna, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
  geom_point()+xlab("Leto")+ylab("Skupna vrednost prestopov")+geom_vline(xintercept = 2017)+
  scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

#================================================================================================
#polozaj prestopov

polozaj <- read.csv("./analiza/polozaj.csv")
polozaj_prihod_stevilo <- polozaj %>% select(X,Napadalec_prihod,Vezist_prihod,Branilec_prihod,Vratar_prihod)
polozaj_prihod_stevilo <- reshape2::melt(polozaj_prihod_stevilo, id.var = 'X')
polozaj_odhod_stevilo <- polozaj %>% select(X,Napadalec_odhod,Vezist_odhod,Branilec_odhod,Vratar_odhod)
polozaj_odhod_stevilo <- reshape2::melt(polozaj_odhod_stevilo, id.var = 'X')

vloge1 <- c('Napadalec-Prihod','Vezist-Prihod','Branilec-Prihod','Vratar-Prihod')
vloge2 <- c('Napadalec-Odhod','Vezist-Odhod','Obramba-Odhod','Vratar-Odhod')

barve1 <- c("#00F5FF", "#0276FD", "#009ACD", "#00688B")
barve2 <- c("#ffb2b2", "#ff0000", "#b20000", "#4c0000")

polozaj_stevilo_prihod_plot <- ggplot(polozaj_prihod_stevilo, aes(x=X,y=value,colour = variable))+geom_path(size = 1.5)+
  scale_color_manual(name = "Polozaj-Prestop", labels = vloge1,values=barve1)+
  xlab('Leto')+ylab('Število prestopov')+
  theme(legend.text = element_text(size = 14))+guides(colour = guide_legend(override.aes = list(size=5)))

polozaj_stevilo_odhod_plot <- ggplot(polozaj_odhod_stevilo, aes(x=X,y=value,colour = variable))+geom_path(size = 1.5)+
  scale_color_manual(name = "Polozaj-Prestop", labels = vloge2,values=barve2)+
  xlab('Leto')+ylab('Število prestopov')+
  theme(legend.text = element_text(size = 14))+guides(colour = guide_legend(override.aes = list(size=5)))

#================================================================================================
#drzave -> zemljevid/geoplot

drzave_prihodi <- read.csv("./analiza/drzave_prihodi.csv")

source("./lib/uvozi.zemljevid.r")
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", "ne_110m_admin_0_countries", pot.zemljevida="", encoding="UTF-8")%>% fortify()

require(ggplot2)
x <- ggplot() + geom_path(data=zemljevid, aes(x=long, y = lat, group = group))
print(x)

#drzave_prihodi <- drzave_prihodi %>% rename(SOVEREIGNT = X)
