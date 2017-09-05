fday <- as.Date(min(nsessoes$timeop))
lday <- as.Date(max(nsessoes$timeop))
week <- seq.Date(day, lday,by="day")

ses <- nsessoes
ses$timeop <- anydate(ses$timeop)

top10SessoesDia <- data.frame(date=unique(ses$timeop), top=0, timestamp=nsessoes[1:length(unique(ses$timeop)),"timeop"])   

for(day in unique(ses$timeop)){
  top <- which(ses$Current == max(ses[ses$timeop == day,"Current"]) & ses$timeop == day)
  top10SessoesDia[top10SessoesDia$date == day, "top"] <- ses[top[1],"Current"]
  top10SessoesDia[top10SessoesDia$date == day, "timestamp"] <- nsessoes[top[1], "timeop"]
  
}

top10SessoesDia$dia <- format(top10SessoesDia$date, "%a")

top10SessoesDia$atividade <- ""

top10SessoesDia <- top10SessoesDia[1:28,]

for(i in 1:nrow(top10SessoesDia))
{
  top10SessoesDia$atividade[i] <- qualAtividade(top10SessoesDia$timestamp[i])
}


printaSessaoSemanaTOPL <- function(sessoes){
  library(lubridate)
  semana <- sessoes
  semana$cor <- NULL
  date1 <- min(semana$timestamp)
  date2 <- max(semana$timestamp)
  legendaX <- paste(format.POSIXct(date1,"%F-%A"))
  maxDay <- format.POSIXct(date2,"%d/%m/%y")
  titleX <- "Pico Diário de Sessões"
  subtitleX <- paste(format.POSIXct(date1,"%d/%m/%y"),"até",maxDay)
  
  
  
  classEventos <- categorizaObjeto(date1)
  
  for (i in 1:nrow(semana)){
    t1 <- semana$timestamp[i]
    semana$cor[i] <- qualAtividade(t1)
  }
  library(RColorBrewer)
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(classEventos$cor)
  
  # labelDias <- function(dat){
  #         legenda <- format.POSIXct(dat,"%H:%M")
  #         legenda[which(hour(dat)==15)] <- format.POSIXct(dat[which(hour(dat)==15)],"%A %H:%M")
  #         legenda
  # }
  
  f <- ggplot(semana,aes(x=date, y=top)) 
  f <- f + geom_col(aes(fill=cor)) 
  #f <- f + geom_vline(xintercept = as.numeric(eventos))
  #f <- f + scale_x_datetime(date_labels = "%a - %H:%M")
  f <- f + scale_x_date(breaks=as.Date(semana$date), labels = format(semana$timestamp, "%a - %H:%M"))
  #f <- f + scale_y_continuous(labels = function(x) paste(round(x)),breaks=seq(min(ds1$sum), limiteYSum+50, by=50), limits=c(0, limiteYSum))
  f <- f + labs(title=titleX, x="Dia - Hora", y="Nº de Sessões", size="Novas Sessões", colour="Atividade", subtitle=subtitleX) 
  #f <- f +  scale_colour_continuous(low = "gray", high="red", breaks=seq(0,limitCorNew,length.out = 3), limits=c(0,limitCorNew), guide="legend") 
  f <- f + scale_fill_manual(name="Atividade", values=myColors)
  f <- f + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_text(angle = -90, hjust = 0))
  f
  # ggsave(paste(legendaX,".png",sep=""), plot = f)
}

printaSessaoSemanaTOPL(top10SessoesDia)
