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
                classEventos <- rbind(classEventos,data.frame(inicio=oitavaAula,fim=24*horas,cor="N/A"))
        }
        classEventos$inicio <- classEventos$inicio+date1
        classEventos$fim <- classEventos$fim+date1
        classEventos
}

printaDiaJunto <- function(nsessoes,dataTraffic,date1){
        library(lubridate)
        library(ggplot2)
        date1 <- as.POSIXct(strptime(date1, "%F"))
        date2 <-  as.POSIXct(date1 + 24*60*60)
        
        limiteYSum <- max(dataTraffic$sum) * 10^-6
        limitCorNew <- max(nsessoes$New)
        
        nsessoes$timeop <- as.POSIXct(nsessoes$timeop)
        dataTraffic$times <- as.POSIXct(dataTraffic$times)
        dataTraffic$cor <- NULL
        nsessoes$cor <- NULL
        dataTraffic <- dataTraffic[dataTraffic$times >= date1 & dataTraffic$times <= date2,]
        nsessoes <- nsessoes[nsessoes$timeop >= date1 & nsessoes$timeop <= date2,]
        
        ds1 <- merge(dataTraffic,nsessoes, by.x="times", by.y="timeop", all=TRUE)
        ds1$times <- dataTraffic$times
        legendaX <- paste(format.POSIXct(date1,"%F-%A"))
        titleX <- "Upload e Sessões"
        subTitlex <- "Quinta-Feira 20/07/2017"
        ds1$color <- ""
        ds1$sum <- ds1$sum * 10^-6
        ds1$download <- ds1$download *10^-6
        ds1$upload <- ds1$upload *10^-6
        classEventos <- categorizaObjeto(date1)
        
        for (i in 1:nrow(ds1)){
                t1 <- ds1$times[i]
                color <- qualAtividade(t1)
                ds1$color[i] <- as.character(color)
        }
        library(RColorBrewer)
        myColors <- brewer.pal(5,"Set1")
        names(myColors) <- levels(classEventos$cor)
        
        f <- ggplot(ds1,aes(x=times, y=upload)) 
        f <- f + geom_col(aes(fill=ds1$color))
        #f <- f + geom_vline(xintercept = as.numeric(eventos))
        f <- f + scale_x_datetime(date_breaks = "100 mins", date_labels = "%H:%M")
        f <- f + geom_point(aes(x=times, y=Current))
        f <- f + scale_y_continuous(labels = function(x) paste(round(x)),breaks=seq(0, 250, by=50), limits=c(0, 250))
        f <- f + labs(title=titleX, x="Hora", y="MB e Sessões", size="Novas Sessões", fill="Atividade", subtitle=subTitlex) 
        #f <- f +  scale_colour_continuous(low = "black", high="red", breaks=seq(0,limitCorNew,length.out = 3), limits=c(0,limitCorNew), guide="legend") 
        f <- f + scale_fill_manual(name="Atividade", values=myColors)
        f <- f + theme(legend.position="bottom", plot.subtitle = element_text(hjust = 0.5),plot.title = element_text(hjust = 0.5))
        f
}

date1 <- strptime("2017-07-20 00:00","%Y-%m-%d %H:%M")
dataTraffic2 <- dataTraffic[dataTraffic$times > date1,]
dataTraffic2$sum <- dataTraffic2$upload
nsessoes2 <- nsessoes[nsessoes$timeop > date1,]
dias <- format.Date(unique(as.Date(dataTraffic2$times)), "%F")
for (i in 1:length(dias)){
        dia <- dias[i]
        printaDiaJunto(nsessoes,dataTraffic2, dia)
}

p 