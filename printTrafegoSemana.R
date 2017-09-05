nsessoes$timeop <- as.POSIXct(nsessoes$timeop)


printaTrafegoSemanaL <- function(trafego,date1,date2){
  library(lubridate)
  trafego$download <- trafego$download * 10^-6 * 8 / 300;
  trafego$upload <- trafego$upload * 10^-6 * 8 / 300;
  semana <- trafego[trafego$times >= date1 & trafego$times <= date2,]
  semana$cor <- NULL
  legendaX <- paste(format.POSIXct(date1,"%F-%A"))
  maxDay <- format.POSIXct(date2,"%d/%m/%y")
  titleX <- "UtilizaÃ§Ã£o da Rede Minha UFOP Wifi"
  subtitleX <- paste(format.POSIXct(date1,"%d/%m/%y"),"até",maxDay)
  
  
  
  classEventos <- categorizaObjeto(date1)
  
  for (i in 1:nrow(semana)){
    t1 <- semana$times[i]
    semana$cor[i] <- as.character(qualAtividade(t1))
  }
  library(RColorBrewer)
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(classEventos$cor)
  
  # labelDias <- function(dat){
  #         legenda <- format.POSIXct(dat,"%H:%M")
  #         legenda[which(hour(dat)==15)] <- format.POSIXct(dat[which(hour(dat)==15)],"%A %H:%M")
  #         legenda
  # }
  
  f <- ggplot(semana,aes(x=times, y=download)) 
  f <- f + geom_col(aes(fill=cor)) 
  #f <- f + geom_vline(xintercept = as.numeric(eventos))
  f <- f + scale_x_datetime(date_breaks = "1 days",date_labels = "%d", limits = c(date1,date2) )
  f <- f + scale_y_continuous(limits=c(0, 100))
  #f <- f + scale_y_continuous(labels = function(x) paste(round(x)),breaks=seq(min(ds1$sum), limiteYSum+50, by=50), limits=c(0, limiteYSum))
  f <- f + labs(title=titleX, x="Dia - Hora", y="MB", size="Novas Sessões", colour="Atividade", subtitle=subtitleX) 
  #f <- f +  scale_colour_continuous(low = "gray", high="red", breaks=seq(0,limitCorNew,length.out = 3), limits=c(0,limitCorNew), guide="legend") 
  f <- f + scale_fill_manual(name="Atividade", values=myColors)
  f <- f + theme(legend.position="none", plot.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_text(angle = -90, hjust = 0))
  f
  # ggsave(paste(legendaX,".png",sep=""), plot = f)
}

printaTrafegoSemana <- function(sessoes,date1,date2){
  library(lubridate)
  sessoes$download <- sessoes$download * 10^-6 * 8 / 300;
  sessoes$upload <- sessoes$upload * 10^-6 * 8 / 300;
  semana <- sessoes[sessoes$times >= date1 & sessoes$times <= date2,]
  semana$cor <- NULL
  
  legendaX <- paste(format.POSIXct(date1,"%F-%A"))
  maxDay <- format.POSIXct(date2,"%d/%m/%y")
  titleX <- "Tráfego de Upload da Rede Minha UFOP Wifi"
  subtitleX <- paste(format.POSIXct(date1,"%d/%m/%y"),"até",maxDay)
  
  
  
  classEventos <- categorizaObjeto(date1)
  
  for (i in 1:nrow(semana)){
    t1 <- semana$times[i]
    semana$cor[i] <- as.character(qualAtividade(t1))
  }
  library(RColorBrewer)
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(classEventos$cor)
  
  # labelDias <- function(dat){
  #         legenda <- format.POSIXct(dat,"%H:%M")
  #         legenda[which(hour(dat)==15)] <- format.POSIXct(dat[which(hour(dat)==15)],"%A %H:%M")
  #         legenda
  # }
  
  f <- ggplot(semana,aes(x=times, y=download)) 
  f <- f + geom_col(aes(fill=cor)) 
  #f <- f + geom_vline(xintercept = as.numeric(eventos))
  f <- f + scale_x_datetime(date_breaks = "1 days", date_labels = "%d", limits = c(date1,date2) )
  f <- f + scale_y_continuous(limits=c(0, 100))
  f <- f + labs(title=titleX, x="", y="MB", size="Novas Sessões", colour="Atividade", subtitle=subtitleX) 
  #f <- f +  scale_colour_continuous(low = "gray", high="red", breaks=seq(0,limitCorNew,length.out = 3), limits=c(0,limitCorNew), guide="legend") 
  f <- f + scale_fill_manual(name="Atividade", values=myColors)
  f <- f + theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_blank())
  f
  # ggsave(paste(legendaX,".png",sep=""), plot = f)
}

#ini <- date1
#fim <- date2
dataTraffic2 <- dataTraffic[dataTraffic$times >= ini & dataTraffic$times <= fim,]
#dataTraffic2 <- dataTraffic2[dataTraffic2$upload != max(dataTraffic2$upload),]
date1 <- as.POSIXct(strptime("2017-07-18 16:10","%Y-%m-%d %H:%M"))
date2 <- as.POSIXct(strptime("2017-07-25 16:10","%Y-%m-%d"))
#date2 <- as.POSIXct(date1 + 7*24*60*60);
s1 <- printaTrafegoSemana(dataTraffic2,date1,date2)
date1 <- date2;
date2 <- as.POSIXct(date1 + 7*24*60*60);
s2 <- printaTrafegoSemanaL(dataTraffic2,date1,date2)
sem12 <- grid.arrange(s1,s2,nrow=2)
ggsave("up-semana12.png",sem12)
date1 <- date2;
date2 <- as.POSIXct(date1 + 7*24*60*60);
s3 <- printaTrafegoSemana(dataTraffic2,date1,date2)
date1 <- date2;
date2 <- as.POSIXct(date1 + 7*24*60*60);
s4 <- printaTrafegoSemanaL(dataTraffic2,date1,date2)
sem34 <- grid.arrange(s3,s4,nrow=2)
ggsave("down-semana34.png",sem34)

