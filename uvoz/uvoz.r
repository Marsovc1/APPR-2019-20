# UREJANJE GLAVNE TABELE O MIGRACIJI
migracija <- read_csv("podatki/migracija.csv", n_max = 160776, na = c("..")) %>%
  select(-"Migration by Gender Code", -"Country Origin Code", -"Country Dest Code")

colnames(migracija) <- c("origin_country", "gender", "dest_country", 
                         "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2010")

skupno <- migracija %>% filter(gender=="Total") %>% select(-"gender") %>% 
  gather(decade, number, "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2010") %>%
  arrange(origin_country)

poSpolih <- migracija %>% filter(gender=="Female" | gender=="Male" ) %>%
  gather(decade, number, "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2010") %>%
  arrange(origin_country)
poSpolih <- poSpolih[c(1,3,2,4,5)]


# BDP podatki iz wikipedije
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)"
stran <- read_html(url)

bdpji <- stran %>%
  html_nodes(xpath = "//table[@class='sortable wikitable']//td") %>%
  html_text() %>% str_replace_all(',', '')

bdp <- as.data.frame(matrix(bdpji, ncol = 11, byrow = TRUE))
bdp <- data.frame(lapply(bdp, as.character), stringsAsFactors=FALSE)
bdp <- bdp[1:768, ]
bdp[bdp==""] <- NA
bdp[, 2:11] <- sapply(bdp[, 2:11], as.numeric)

osemdeseta <- bdp[1:192,]
colnames(osemdeseta) <- c("country", 1980:1989)
devetdeseta <- bdp[193:384,]
colnames(devetdeseta) <- c("country", 1990:1999)
dvatisoca <- bdp[385:576,]
colnames(dvatisoca) <- c("country", 2000:2009)
dvadeseta <- bdp[577:768,]
colnames(dvadeseta) <- c("country", 2010:2019)

osemdeseta <- osemdeseta %>% gather(leto, BDP, "1980":"1989")
devetdeseta <- devetdeseta %>% gather(leto, BDP, "1990":"1999")
dvatisoca <- dvatisoca %>% gather(leto, BDP, "2000":"2009")
dvadeseta <- dvadeseta %>% gather(leto, BDP, "2010":"2019")

bdp <- rbind(osemdeseta, devetdeseta, dvatisoca, dvadeseta)
bdp$BDP <- bdp$BDP * 1000


# POPULACIJA
pop <- read.csv2("podatki/populacija.csv", skip = 16) %>% 
  filter(Type=="Country/Area") %>%
  select(-"Index", -"Variant", -"Notes", -"Country.code", -"Parent.code", -"Type") 
  
colnames(pop) <- c("country", 1950:2020)
pop[, 2:72] <- as.data.frame(apply(pop[, 2:72],2,function(x)gsub('\\s+', '',x)))
pop[, 1:72] <- sapply(pop[, 1:72], as.character)
pop[, 2:72] <- sapply(pop[, 2:72], as.numeric)
pop <- arrange(pop, drzava)
pop <- pop[, c(1, 12:62)]
pop <- pop %>% gather(leto, populacija, "1960":"2010")
pop$populacija <- pop$populacija * 1000


# RELIGIJE
relig <- read.csv("podatki/religije.csv") %>%
  select(-"pop2019")

rm(devetdeseta, dvadeseta, dvatisoca, migracija, osemdeseta, stran, bdpji, url)