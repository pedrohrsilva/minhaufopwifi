ses <- dataTraffic2
ses$times <- anydate(ses$times)

SessoesDia <- data.frame(date=unique(ses$times), topUp=0, dateUp=dataTraffic2[1:length(unique(ses$times)),"times"], topDown=0, dateDown =dataTraffic2[1:length(unique(ses$times)),"times"])   

for(day in unique(ses$times)){
  SessoesDia[SessoesDia$date == day, "topUp"] <- max(ses[ses$times == day,"upload"])
  i <- which(ses$upload == max(ses[ses$times == day,"upload"]))
  SessoesDia[SessoesDia$date == day, "dateUp"] <- dataTraffic2[i,"times"]
  SessoesDia[SessoesDia$date == day, "atvUP"] <- qualAtividade(dataTraffic2[i,"times"])
  SessoesDia[SessoesDia$date == day, "topDown"] <- max(ses[ses$times == day,"download"])
  i <- which(ses$download == max(ses[ses$times == day,"download"]))
  SessoesDia[SessoesDia$date == day, "dateDown"] <- dataTraffic2[i,"times"]
  SessoesDia[SessoesDia$date == day, "atvDown"] <- qualAtividade(dataTraffic2[i,"times"])
}
SessoesDia$topUp <- SessoesDia$topUp * 10 ^ -6
SessoesDia$topUp <- paste(round(SessoesDia$topUp, 2), "MB")
SessoesDia$topDown <- SessoesDia$topDown * 10 ^ -6
SessoesDia$topDown <- paste(round(SessoesDia$topDown,2), "MB")
SessoesDia$dia <- format(SessoesDia$date, "%A")
SessoesDia
