qualAtividade <- function (date1){
  library("lubridate")
  horas <- 60*60
  minutos <- 60
  if(wday(date1) == 1 || wday(date1) == 7){
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
  classEventos$inicio <- classEventos$inicio+ as.POSIXct(strptime(format(date1,format="%y-%m-%d"),"%y-%m-%d"))
  classEventos$fim <- classEventos$fim + as.POSIXct(strptime(format(date1,format="%y-%m-%d"),"%y-%m-%d"))
  as.character(classEventos$cor[classEventos$inicio <= date1 & date1 < classEventos$fim])
}

top10$atividade <- ""

for(i in 1:nrow(top10))
{
  top10$atividade[i] <- qualAtividade(top10$timeop[i])
}

