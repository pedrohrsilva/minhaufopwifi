nsessoes$timeop <- as.POSIXct(nsessoes$timeop)





printaSessaoSemanaL <- function(sessoes,date1,date2){
  library(lubridate)
  semana <- nsessoes[nsessoes$timeop >= date1 & nsessoes$timeop <= date2,]
  semana$cor <- NULL
  
  legendaX <- paste(format.POSIXct(date1,"%F-%A"))
  maxDay <- format.POSIXct(date2,"%d/%m/%y")
  titleX <- "UtilizaÃ§Ã£o da Rede Minha UFOP Wifi"
  subtitleX <- paste(format.POSIXct(date1,"%d/%m/%y"),"até",maxDay)
  
  
  
  classEventos <- categorizaObjeto(date1)
  
  for (i in 1:nrow(semana)){
    t1 <- semana$timeop[i]
    if(t1 > max(classEventos$fim)){
      classEventos$inicio <- classEventos$inicio + 24*60*60
      classEventos$fim <- classEventos$fim + 24*60*60
    }
    if(wday(t1)==1 || wday(t1)==7){
      color <- "N/A"
    } else{
      color <- classEventos$cor[classEventos$inicio <= t1 & t1 < classEventos$fim]
    }
    #print(color)
    semana$cor[i] <- as.character(color)
  }
  library(RColorBrewer)
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(classEventos$cor)
  
  # labelDias <- function(dat){
  #         legenda <- format.POSIXct(dat,"%H:%M")
  #         legenda[which(hour(dat)==15)] <- format.POSIXct(dat[which(hour(dat)==15)],"%A %H:%M")
  #         legenda
  # }
  
  f <- ggplot(semana,aes(x=timeop, y=Current)) 
  f <- f + geom_col(aes(fill=cor)) 
  #f <- f + geom_vline(xintercept = as.numeric(eventos))
  f <- f + scale_x_datetime(date_breaks = "8 hours",date_labels = "%a - %H:%M", limits = c(date1,date2) )
  f <- f + scale_y_continuous(limits=c(0, max(sessoes$Current)))
  #f <- f + scale_y_continuous(labels = function(x) paste(round(x)),breaks=seq(min(ds1$sum), limiteYSum+50, by=50), limits=c(0, limiteYSum))
  f <- f + labs(title=titleX, x="Dia - Hora", y="Nº de Sessões", size="Novas Sessões", colour="Atividade", subtitle=subtitleX) 
  #f <- f +  scale_colour_continuous(low = "gray", high="red", breaks=seq(0,limitCorNew,length.out = 3), limits=c(0,limitCorNew), guide="legend") 
  f <- f + scale_fill_manual(name="Atividade", values=myColors)
  f <- f + theme(legend.position="none", plot.title = element_blank(), plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_text(angle = -90, hjust = 0))
  f
  # ggsave(paste(legendaX,".png",sep=""), plot = f)
}

printaSessaoSemana <- function(sessoes,date1,date2){
  library(lubridate)
  semana <- nsessoes[nsessoes$timeop >= date1 & nsessoes$timeop <= date2,]
  semana$cor <- NULL
  
  legendaX <- paste(format.POSIXct(date1,"%F-%A"))
  maxDay <- format.POSIXct(date2,"%d/%m/%y")
  titleX <- "Número de Sessões da Rede Minha UFOP Wifi"
  subtitleX <- paste(format.POSIXct(date1,"%d/%m/%y"),"até",maxDay)
  
  
  
  classEventos <- categorizaObjeto(date1)
  
  for (i in 1:nrow(semana)){
    t1 <- semana$timeop[i]
    if(t1 > max(classEventos$fim)){
      classEventos$inicio <- classEventos$inicio + 24*60*60
      classEventos$fim <- classEventos$fim + 24*60*60
    }
    if(wday(t1)==1 || wday(t1)==7){
      color <- "N/A"
    } else{
      color <- classEventos$cor[classEventos$inicio <= t1 & t1 < classEventos$fim]
    }
    #print(color)
    semana$cor[i] <- as.character(color)
  }
  library(RColorBrewer)
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(classEventos$cor)
  
  # labelDias <- function(dat){
  #         legenda <- format.POSIXct(dat,"%H:%M")
  #         legenda[which(hour(dat)==15)] <- format.POSIXct(dat[which(hour(dat)==15)],"%A %H:%M")
  #         legenda
  # }
  
  f <- ggplot(semana,aes(x=timeop, y=Current)) 
  f <- f + geom_col(aes(fill=cor)) 
  #f <- f + geom_vline(xintercept = as.numeric(eventos))
  f <- f + scale_x_datetime(date_breaks = "8 hours",date_labels = "%a - %H:%M" )
  f <- f + scale_y_continuous(limits=c(0, max(sessoes$Current)))
  f <- f + labs(title=titleX, x="", y="Nº de Sessões", size="Novas Sessões", colour="Atividade", subtitle=subtitleX) 
  #f <- f +  scale_colour_continuous(low = "gray", high="red", breaks=seq(0,limitCorNew,length.out = 3), limits=c(0,limitCorNew), guide="legend") 
  f <- f + scale_fill_manual(name="Atividade", values=myColors)
  f <- f + theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x=element_blank())
  f
  # ggsave(paste(legendaX,".png",sep=""), plot = f)
}

date1 <- as.POSIXct(strptime("2017-06-27","%Y-%m-%d"))
date2 <- as.POSIXct(date1 + 7*24*60*60);
s1 <- printaSessaoSemana(nsessoes,date1,date2)
date1 <- date2;
date2 <- as.POSIXct(date1 + 7*24*60*60);
s2 <- printaSessaoSemanaL(nsessoes,date1,date2)
sem12 <- grid.arrange(s1,s2,nrow=2)
ggsave("semana12.png",sem12)
date1 <- date2;
date2 <- as.POSIXct(date1 + 7*24*60*60);
s3 <- printaSessaoSemana(nsessoes,date1,date2)
date1 <- date2;
date2 <- as.POSIXct(date1 + 7*24*60*60);
s4 <- printaSessaoSemanaL(nsessoes,date1,date2)
sem34 <- grid.arrange(s3,s4,nrow=2)
ggsave("semana34.png",sem34)

