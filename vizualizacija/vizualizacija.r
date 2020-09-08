# 3. faza: Vizualizacija podatkov

library(tidyverse)

prestopi <- read.csv("./analiza/prestopi.csv")

prestopi_stevilo <- prestopi %>% select(X,Število_prihodov, Število_odhodov)
prestopi_stevilo <- reshape2::melt(prestopi_stevilo, id.var = 'X')

prestopi_stevilo_plot <- ggplot(prestopi_stevilo, aes(x=X,y=value,colour = variable))+geom_path()+
                          geom_point()+xlab("Leto")+ylab("Število prestopov")+
                          scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))


prestopi_cena <- prestopi %>% select(X,Povprecna_vrednost_prihoda,Povprecna_vrednost_odhoda)
prestopi_cena <- reshape2::melt(prestopi_cena, id.var = 'X')

prestopi_cena_plot <- ggplot(prestopi_cena, aes(x=X,y=value,colour = variable))+geom_path()+
                      geom_point()+xlab("Leto")+ylab("Povprečna cena prestopov")+
                      scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))


prestopi_starost <- prestopi %>% select(X,Povprecna_starost_prihoda,Povprecna_starost_odhoda)
prestopi_starost <- reshape2::melt(prestopi_starost, id.var = 'X')

prestopi_starost_plot <- ggplot(prestopi_starost, aes(x=X,y=value,colour = variable))+geom_path()+
  geom_point()+xlab("Leto")+ylab("Povprečna starost prestopov")+
  scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))

prestopi_skupna <- prestopi %>% select(X,Skupna_vrednost_prihodov,Skupna_vrednost_odhodov)
prestopi_skupna <- reshape2::melt(prestopi_skupna, id.var = 'X')

prestopi_skupna_plot <- ggplot(prestopi_skupna, aes(x=X,y=value,colour = variable))+geom_path()+
  geom_point()+xlab("Leto")+ylab("Skupna vrednost prestopov")+geom_vline(xintercept = 2017)+
  scale_color_manual(name = "Prestop", labels = c('Prihodi','Odhodi'),values=c('Blue','Red'))


print(prestopi_skupna_plot)