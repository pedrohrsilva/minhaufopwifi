ses <- nsessoes
ses$timeop <- anydate(ses$timeop)

SessoesDia <- data.frame(date=unique(ses$timeop), total=0)   

for(day in unique(ses$timeop)){
  SessoesDia[SessoesDia$date == day, "total"] <- sum(ses[ses$timeop == day,"New"])
  
}

SessoesDia$dia <- format(SessoesDia$date, "%a")
SessoesDia <- SessoesDia[2:22,]
