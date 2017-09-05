printaDia <- function(nsessoes,dataTraffic,date1){
        date1 <- strptime(date1, "%F")
        date2 <- date1 + 24*60*60
        
        dataTraffic <- dataTraffic[dataTraffic$times >= date1 & dataTraffic$times <= date2,]
        nsessoes <- nsessoes[nsessoes$timeop >= date1 & nsessoes$timeop <= date2,]
        
        legendaX <- paste(format.POSIXlt(date1,"%F-%A"))
        
        
        primeiraAula <- date1 + 13*60*60 + 30*60  #1 antes
        segundaAula <- primeiraAula + 50*60       #2 aula
        intervaloTarde <- segundaAula + 50*60     #3 aula
        terceiraAula <- intervaloTarde + 15*60    #4 intervalo
        quartaAula <- terceiraAula + 50*60        #5 aula
        trocaTurno <- quartaAula + 50*60          #6 aula
        quintaAula <- trocaTurno + 45*60 + 60*60  #7 -- troca
        sextaAula <- quintaAula + 50*60           #8 aula
        intervaloNoite <- sextaAula + 15*60       #9 aula
        setimaAula <- intervaloNoite +50*60       #10 intervalo
        oitavaAula <- setimaAula + 50*60          #11 -- aula
        fimAula <- oitavaAula + 50*60             #12 --aula
                                                  #depois
        
        eventos <- c(primeiraAula,segundaAula,intervaloTarde,terceiraAula,quartaAula,trocaTurno,quintaAula,sextaAula,intervaloNoite,setimaAula,oitavaAula,fimAula)
        inicio_eventos <- c(date1,primeiraAula,segundaAula,intervaloTarde,terceiraAula,quartaAula,trocaTurno,quintaAula,sextaAula,intervaloNoite,setimaAula,oitavaAula,fimAula)
        fim_eventos <- c(primeiraAula,segundaAula, intervaloTarde, terceiraAula, quartaAula, trocaTurno, quintaAula, sextaAula, intervaloNoite, setimaAula, oitavaAula, fimAula,date2)
        label_e <- c("N/A","Aula", "Aula","Intervalo", "Aula", "Aula", "Intervalo", "Aula", "Aula", "Intervalo", "Aula", "Aula","N/A")
        cor_eventos <- label_e
        
        classEventos <- data.frame(inicio_eventos, fim_eventos, cor_eventos, label_e)
        
        dataTraffic$cor <- ""
        dataTraffic$label <- ""
        nsessoes$cor <- ""
        nsessoes$label <- ""

        for (i in 1:nrow(dataTraffic)){
                t1 <- dataTraffic$times[i]
                cor <- classEventos$cor_eventos[classEventos$inicio_eventos <= t1 & t1 < classEventos$fim_eventos]
                label_e <- classEventos$label_e[classEventos$inicio_eventos <= t1 & t1 < classEventos$fim_eventos]
                dataTraffic$cor[i] <- cor
                dataTraffic$label <- label_e
                #print(t1) 
                #print(cor)
        }
        
        for (i in 1:nrow(nsessoes)){
                t1 <- nsessoes$timeop[i]
                cor <- classEventos$cor_eventos[classEventos$inicio_eventos <= t1 & t1 < classEventos$fim_eventos]
                label_e <- classEventos$label_e[classEventos$inicio_eventos <= t1 & t1 < classEventos$fim_eventos]
                nsessoes$label <- label_e
                nsessoes$cor[i] <- cor
        }
                
        
        
        
        g <- ggplot(nsessoes,aes(x=nsessoes$timeop, y=nsessoes$Current, size=nsessoes$New)) 
        g <- g + geom_point(aes(colour=nsessoes$cor)) 
        g <-g + scale_x_datetime(date_breaks = "100 mins", date_labels = "%H:%M") 
        #g <- g + geom_vline(xintercept = as.numeric(eventos))
        g <- g + labs(title=legendaX, x="Hora", y="Sessões Ativas", size="Novas Sessões", "Atividade") 
        g <- g + scale_colour_discrete(labels=c("Aula","Intervalo","N/A"))
        ggsave(paste(legendaX,"-nsessoes.png",sep=""), plot=g)
        
        f <- ggplot(dataTraffic,aes(x=dataTraffic$times, y=dataTraffic$sum)) 
        f <- f + geom_col(aes(colour=dataTraffic$cor)) 
        #f <- f + geom_vline(xintercept = as.numeric(eventos))
        f <- f + scale_x_datetime(date_breaks = "100 mins", date_labels = "%H:%M")
        f <- f + scale_y_continuous(labels = function(x) paste(x*10^-6,"MB"),breaks=seq(min(dataTraffic$sum), max(dataTraffic$sum)+50*10^6, by=50*10^6))
        f <- f + labs(title=legendaX, x="Hora", y="Download Total em 5 minutos", size="Novas Sessões", colour="Atividade") 
        f <- f + scale_colour_discrete(labels=c("Aula","Intervalo","N/A"))
        
        ggsave(paste(legendaX,"-download.png",sep=""), plot = f)
}



dias <- format.Date(unique(as.Date(dataTraffic$times)), "%F")
date1 <- dias[2]
printaDia(nsessoes,dataTraffic, date1)

for (i in 1:length(dias)){
        dia <- dias[i]
        printaDia(nsessoes,dataTraffic, dia)
}


#dias <- format.Date()

#select data.*, cor.cor from 
