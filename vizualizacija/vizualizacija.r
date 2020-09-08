# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid -> koliko prestopov, kakšna je skupna cena skozi leta, za države prihod/odhod

source('./lib/uvozi.zemljevid.R') #pot do funkcije uvozi.zemljevid
zemljevid <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", "ne_10m_admin_0_countries",
                             pot.zemljevida="", encoding="Windows-1250")



