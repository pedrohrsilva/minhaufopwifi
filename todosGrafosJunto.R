categorizaObjeto <- function (date1){
        horas <- 60*60
        minutos <- 60
        day1 <- as.POSIXlt(date1)
        if(day1$wday == 0 || day1$wday == 6){
                classEventos <- data.frame(inicio=0,fim=24*horas,cor="N/A")     
        }else{
                ruAlmoco <- 11*horas+30*minutos #(11:30)
                primeiraAula <- 13*horas+30*minutos
                segundaAula <- 14*horas+20*minutos
                intervaloTarde <- 15*horas+10*minutos
                terceiraAula <- 15*horas + 25*minutos
                quartaAula <- 16*horas + 15*minutos
                ruJanta <- 16*horas + 50*minutos
                fimQuartaAula <- 17*horas + 05*minutos
                quintaAula <- 18*horas + 50*minutos
                sextaAula <- 19*horas+40*minutos
                intervaloNoite <- 20*horas+30*minutos
                setimaAula <- 20*horas+45*minutos
                oitavaAula <- 21*horas+35*minutos
                fimAula <- 22*horas + 25*minutos
                classEventos <- data.frame(inicio=0,fim=ruAlmoco,cor="N/A")
                classEventos <- rbind(classEventos,data.frame(inicio=ruAlmoco,fim=primeiraAula,cor="RU"))
                classEventos <- rbind(classEventos,data.frame(inicio=primeiraAula,fim=segundaAula,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=segundaAula,fim=intervaloTarde,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=intervaloTarde,fim=terceiraAula,cor="Intervalo"))
                classEventos <- rbind(classEventos,data.frame(inicio=terceiraAula,fim=quartaAula,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=quartaAula,fim=ruJanta,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=ruJanta,fim=fimQuartaAula,cor="Aula+RU"))
                classEventos <- rbind(classEventos,data.frame(inicio=fimQuartaAula,fim=quintaAula,cor="RU"))
                classEventos <- rbind(classEventos,data.frame(inicio=quintaAula,fim=sextaAula,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=sextaAula,fim=intervaloNoite,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=intervaloNoite,fim=setimaAula,cor="Intervalo"))
                classEventos <- rbind(classEventos,data.frame(inicio=setimaAula,fim=oitavaAula,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=oitavaAula,fim=fimAula,cor="Aula"))
                classEventos <- rbind(classEventos,data.frame(inicio=fimAula,fim=24*horas,cor="N/A"))
        }
        classEventos$inicio <- classEventos$inicio+date1
        classEventos$fim <- classEventos$fim+date1
        classEventos
}

printaDiaJunto <- function(nsessoes,dataTraffic,date1){
        library(lubridate)
        date1 <- as.POSIXct(strptime(date1, "%F"))
        date2 <-  as.POSIXct(date1 + 24*60*60)
        
        limiteYSum <- max(dataTraffic$sum) * 10^-6
        limitCorNew <- max(nsessoes$New)
        
        nsessoes$timeop <- as.POSIXct(nsessoes$timeop)
        dataTraffic$times <- as.POSIXct(dataTraffic$times)
        dataTraffic$cor <- NULL
        nsessoes$cor <- NULL
        dataTraffic <- dataTraffic[dataTraffic$times >= date1,]
        nsessoes <- nsessoes[nsessoes$timeop >= date1,]
        
        ds1 <- merge(dataTraffic,nsessoes, by.x="times", by.y="timeop", all=TRUE)
        ds1$times <- dataTraffic$times
        legendaX <- paste(format.POSIXct(date1,"%F-%A"))
        maxDay <- format.POSIXct(max(dataTraffic$times),"%d/%m/%y")
        titleX <- "Utilização da Rede Minha UFOP Wifi"
        subtitleX <- paste(format.POSIXct(date1,"%d/%m/%y"),"até",maxDay)
        
        
        ds1$color <- ""
        ds1$sum <- ds1$sum * 10^-6
        
        classEventos <- categorizaObjeto(date1)
        
        for (i in 1:nrow(ds1)){
                t1 <- ds1$times[i]
                if(t1 > max(classEventos$fim)){
                        classEventos$inicio <- classEventos$inicio + 24*60*60
                        classEventos$fim <- classEventos$fim + 24*60*60
                }
                if(wday(t1)==1 || wday(t1)==7){
                        color <- "N/A"
                } else{
                        color <- classEventos$cor[classEventos$inicio <= t1 & t1 < classEventos$fim]
                }
                print(color)
                ds1$color[i] <- as.character(color)
        }
        library(RColorBrewer)
        myColors <- brewer.pal(5,"Set1")
        names(myColors) <- levels(classEventos$cor)
        
        # labelDias <- function(dat){
        #         legenda <- format.POSIXct(dat,"%H:%M")
        #         legenda[which(hour(dat)==15)] <- format.POSIXct(dat[which(hour(dat)==15)],"%A %H:%M")
        #         legenda
        # }
        
        f <- ggplot(ds1,aes(x=times, y=sum)) 
        f <- f + geom_col(aes(fill=ds1$color)) 
        #f <- f + geom_vline(xintercept = as.numeric(eventos))
        f <- f + scale_x_datetime(date_breaks = "24 hours",date_labels = "%A - %H:%M" )
        f <- f + geom_line(aes(x=times, y=Current, colour=New),size=1)
        f <- f + scale_y_continuous(labels = function(x) paste(round(x)),breaks=seq(min(ds1$sum), limiteYSum+50, by=50), limits=c(0, limiteYSum))
        f <- f + labs(title=titleX, x="Hora", y="Download(MB) e Sessões", size="Novas Sessões", fill="Atividade", colour="Novas Sessões", subtitle=subtitleX) 
        f <- f +  scale_colour_continuous(low = "gray", high="red", breaks=seq(0,limitCorNew,length.out = 3), limits=c(0,limitCorNew), guide="legend") 
        f <- f + scale_fill_manual(name="Atividade", values=myColors)
        f <- f + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
        f
        # ggsave(paste(legendaX,".png",sep=""), plot = f)
}

date1 <- strptime("2017-06-28 00:00","%Y-%m-%d %H:%M")
date2 <- date1 + 7*24*60*60
dataTraffic2 <- dataTraffic[dataTraffic$times >= date1 & dataTraffic$times <= date2,]
dataTraffic2$sum <- dataTraffic2$download
nsessoes2 <- nsessoes[nsessoes$timeop > date1 & nsessoes$timeop <= date2,]
dias <- format.Date(unique(as.Date(dataTraffic2$times)), "%F")
p <- printaDiaJunto(nsessoes2,dataTraffic2, dias[1])
ggsave("2017-06-28-7dias.png", plot=p)

