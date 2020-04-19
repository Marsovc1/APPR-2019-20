for (i in 10:19){
  link = paste0("https://en.wikipedia.org/wiki/20",i,"%E2%80%93",i+1,"_FC_Barcelona_season")
  destfile = file.path("C:","Users","marsovc","Documents","Analiza Transferjev FC Barcelona","uvoz","HTML",paste0("FCB ",i,".html"))
  download.file(link, destfile)
}