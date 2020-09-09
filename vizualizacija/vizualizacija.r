# 3. faza: Vizualizacija podatkov

library(tidyverse)
warning=FALSE
#graf, ki ga zelimo narisati plot(graf)

#================================================================================================
#stevilo, povprecna cena, starost in skupna cena prestopov

prestopi <- read.csv("analiza/prestopi.csv")

#povprecno stevilo prestopov
prestopi_stevilo <- prestopi %>% select(X,stevilo_prihodov, stevilo_odhodov)
prestopi_stevilo <- reshape2::melt(prestopi_stevilo, id.var = 'X')

prestopi_stevilo_plot <- ggplot(prestopi_stevilo, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
                          geom_point()+xlab("Leto")+ylab("stevilo prestopov")+
                          scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

#povprecna cena prestopa
prestopi_cena <- prestopi %>% select(X,Povprecna_vrednost_prihoda,Povprecna_vrednost_odhoda)
prestopi_cena <- reshape2::melt(prestopi_cena, id.var = 'X')

prestopi_cena_plot <- ggplot(prestopi_cena, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
                      geom_point()+xlab("Leto")+ylab("Povprecna cena prestopov")+
                      geom_smooth(method='lm',se=F)+
                      scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

#povprecna starost prestopa
prestopi_starost <- prestopi %>% select(X,Povprecna_starost_prihoda,Povprecna_starost_odhoda)
prestopi_starost <- reshape2::melt(prestopi_starost, id.var = 'X')

prestopi_starost_plot <- ggplot(prestopi_starost, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
  geom_point()+xlab("Leto")+ylab("Povprecna starost prestopov")+ scale_x_continuous(breaks=seq(2010, 2020, 2))
  scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

#skupna cena prestopov
prestopi_skupna <- prestopi %>% select(X,Skupna_vrednost_prihodov,Skupna_vrednost_odhodov)
prestopi_skupna <- reshape2::melt(prestopi_skupna, id.var = 'X')

prestopi_skupna_plot <- ggplot(prestopi_skupna, aes(x=X,y=value,colour = variable))+geom_path(size = 1)+
  geom_point()+xlab("Leto")+ylab("Skupna vrednost prestopov")+geom_vline(xintercept = 2017)+
  scale_x_continuous(breaks=seq(2010, 2020, 2))+
  scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

#plot(prestopi_skupna_plot)
#========================================================================================================
#napredna analiza napoved
leta <- data.frame(X=seq(2020,2022,1))
prestopi_skupna_prihod <- prestopi %>% select(X,Skupna_vrednost_prihodov)
prestopi_skupna_odhod <- prestopi %>% select(X,Skupna_vrednost_odhodov)

napoved_prihod <- lm(data=prestopi_skupna_prihod,prestopi_skupna_prihod$Skupna_vrednost_prihodov ~ X)
napoved_vrednost_prihod <- mutate(leta, Skupna_vrednost_prihodov=predict(napoved_prihod, leta))

napoved_odhod <- lm(data=prestopi_skupna_odhod,prestopi_skupna_odhod$Skupna_vrednost_odhodov ~ X)
napoved_vrednost_odhod <- mutate(leta, Skupna_vrednost_odhodov=predict(napoved_odhod, leta))

prestopi_skupna_prihod_plot <- ggplot(prestopi_skupna_prihod, aes(x=X,y=Skupna_vrednost_prihodov,colour = 'Blue'))+geom_path(size = 1)+
  geom_smooth(method = lm, color="#ffff00",se=FALSE,size=1.2)+geom_point()+
  geom_point(data=napoved_vrednost_prihod, aes(x=X,y=Skupna_vrednost_prihodov,colour='Green'))+
  scale_x_continuous('Leto', breaks=seq(2010, 2022, 1), limits=c(2010, 2022))+
  xlab("Leto")+ylab("Skupna vrednost prestopov")+geom_vline(xintercept = 2017)+
  scale_color_manual(name = "Vrednost prestopov", labels = c('Prihodi','Napoved'),values=c('Blue','Green'))

prestopi_skupna_odhod_plot <- ggplot(prestopi_skupna_odhod, aes(x=X,y=Skupna_vrednost_odhodov,colour = 'Red'))+geom_path(size = 1)+
  geom_smooth(method = lm, color="Blue",se=FALSE,size=1.2)+geom_point()+
  geom_point(data=napoved_vrednost_odhod, aes(x=X,y=Skupna_vrednost_odhodov,colour='Green'))+
  scale_x_continuous('Leto', breaks=seq(2010, 2022, 1), limits=c(2010, 2022))+
  xlab("Leto")+ylab("Skupna vrednost prestopov")+geom_vline(xintercept = 2017)+
  scale_color_manual(name = "Vrednost prestopov", labels = c('Napoved','Odhodi'),values=c('Green','Red'))

#print(c(napoved_prihod$coefficients,napoved_odhod$coefficients))
#plot(prestopi_skupna_prihod_plot)
#plot(prestopi_skupna_odhod_plot)
#================================================================================================
#polozaj prestopov

polozaj <- read.csv("analiza/polozaj.csv")
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
  xlab('Leto')+ylab('stevilo prestopov')+
  theme(legend.text = element_text(size = 14))+guides(colour = guide_legend(override.aes = list(size=5)))

polozaj_stevilo_odhod_plot <- ggplot(polozaj_odhod_stevilo, aes(x=X,y=value,colour = variable))+geom_path(size = 1.5)+
  scale_color_manual(name = "Polozaj-Prestop", labels = vloge2,values=barve2)+
  xlab('Leto')+ylab('stevilo prestopov')+
  theme(legend.text = element_text(size = 14))+guides(colour = guide_legend(override.aes = list(size=5)))

#================================================================================================
#drzave -> zemljevid/geoplot

#uvoz in ureditev stevila prestopov
drzave_prihodi <- read.csv("analiza/drzave_prihodi.csv")
drzave_prihodi <- drzave_prihodi[-1]
colnames(drzave_prihodi) <- c('Drzava',2010:2019)
drzave_prihodi <- data.frame(drzave_prihodi$Drzava,rowSums(drzave_prihodi[2:11]))
colnames(drzave_prihodi) <- c('drzava','stevilo')

drzave_odhodi <- read.csv("analiza/drzave_odhodi.csv")
drzave_odhodi <- drzave_odhodi[-1]
colnames(drzave_odhodi) <- c('Drzava',2010:2019)
drzave_odhodi <- data.frame(drzave_odhodi$Drzava,rowSums(drzave_odhodi[2:11]))
colnames(drzave_odhodi) <- c('drzava','stevilo')

#zemljevid
source("./lib/uvozi.zemljevid.r")
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")%>% fortify()

#izris zemljevida stevila prestopov
drzave_prihodi <- drzave_prihodi %>% right_join(zemljevid, by=c('drzava'='NAME'))
map_prihodi <- ggplot() + geom_polygon(data=drzave_prihodi, aes(x=long, y=lat, group=group, fill=stevilo))+
  xlab('Zemljepisna sirina')+ylab('Zemljepisna dolzina')+
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(),
        legend.key.size = unit(1.5, "cm"),legend.key.width = unit(0.5,"cm"))+
  scale_fill_gradient(high="#008800", low="#CCFFCC")+labs(fill = "Skupno stevilo prihodov")

drzave_odhodi <- drzave_odhodi %>% right_join(zemljevid, by=c("drzava"='NAME'))
map_odhodi <- ggplot() + geom_polygon(data=drzave_odhodi, aes(x=long, y=lat, group=group, fill=stevilo))+
  xlab('Zemljepisna sirina')+ylab('Zemljepisna dolzina')+
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(),
        legend.key.size = unit(1.5, "cm"),legend.key.width = unit(0.5,"cm"))+
  scale_fill_gradient(high="#008800", low="#CCFFCC")+labs(fill = "Skupno stevilo odhodov")

#analogno za skupne vrednosti
drzave_prihodi_vrednost <- read.csv("analiza/drzave_prihodi_vrednost.csv")
drzave_prihodi_vrednost <- drzave_prihodi_vrednost[-1]
colnames(drzave_prihodi_vrednost) <- c('Drzava',2010:2019)
drzave_prihodi_vrednost <- data.frame(drzave_prihodi_vrednost$Drzava,rowSums(drzave_prihodi_vrednost[2:11]))
colnames(drzave_prihodi_vrednost) <- c('drzava','stevilo')

drzave_prihodi_vrednost <- drzave_prihodi_vrednost %>% right_join(zemljevid, by=c('drzava'='NAME'))
map_prihodi_vrednost <- ggplot() + geom_polygon(data=drzave_prihodi_vrednost, aes(x=long, y=lat, group=group, fill=stevilo))+
  xlab('Zemljepisna sirina')+ylab('Zemljepisna dolzina')+
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(),
        legend.key.size = unit(1.5, "cm"),legend.key.width = unit(0.5,"cm"))+
  scale_fill_gradient(high="#008800", low="#CCFFCC")+labs(fill = "Skupna vrednost prihodov")

drzave_odhodi_vrednost <- read.csv("analiza/drzave_odhodi_vrednost.csv")
drzave_odhodi_vrednost <- drzave_odhodi_vrednost[-1]
colnames(drzave_odhodi_vrednost) <- c('Drzava',2010:2019)
drzave_odhodi_vrednost <- data.frame(drzave_odhodi_vrednost$Drzava,rowSums(drzave_odhodi_vrednost[2:11]))
colnames(drzave_odhodi_vrednost) <- c('drzava','stevilo')

drzave_odhodi_vrednost <- drzave_odhodi_vrednost %>% right_join(zemljevid, by=c("drzava"='NAME'))
map_odhodi_vrednost <- ggplot() + geom_polygon(data=drzave_odhodi_vrednost, aes(x=long, y=lat, group=group, fill=stevilo))+
  xlab('Zemljepisna sirina')+ylab('Zemljepisna dolzina')+
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(),
        legend.key.size = unit(1.5, "cm"),legend.key.width = unit(0.5,"cm"))+
  scale_fill_gradient(high="#008800", low="#CCFFCC")+labs(fill = "Skupna vrednost odhodov")


#================================================================================================
#izris vseh prestopov
drzave_vsi <- read.csv("analiza/drzave_vsi.csv")
drzave_vsi <- drzave_vsi[-1]
colnames(drzave_vsi) <- c('Drzava',2010:2019)
drzave_vsi <- data.frame(drzave_vsi$Drzava,rowSums(drzave_vsi[2:11]))
colnames(drzave_vsi) <- c('drzava','stevilo')

drzave_vsi <- drzave_vsi %>% right_join(zemljevid, by=c('drzava'='NAME'))
map_vsi <- ggplot() + geom_polygon(data=drzave_vsi, aes(x=long, y=lat, group=group, fill=stevilo))+
  xlab('Zemljepisna sirina')+ylab('Zemljepisna dolzina')+
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(),
        legend.key.size = unit(1.5, "cm"),legend.key.width = unit(0.5,"cm"))+
  scale_fill_gradient(high="#008800", low="#CCFFCC")+labs(fill = "Skupna vrednost prestopov")

#================================================================================================
#shiny

