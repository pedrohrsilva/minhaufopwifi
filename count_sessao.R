nsessoes <- read.csv(file = "new_sessions.csv")
colnames(nsessoes) <- c("New", "Current", "timeop")

date1 <- strptime("2017-06-28 00:00","%Y-%m-%d %H:%M")
date2 <- strptime("2017-06-29 00:00","%Y-%m-%d %H:%M")

nsessoes$timeop <- paste(nsessoes$timeop, "00", sep ="")
nsessoes$timeop <- strptime(nsessoes$timeop, "%Y-%m-%d %H:%M:%OS%z")
#nsessoes <- nsessoes[nsessoes$timeop >= date1 & nsessoes$timeop <= date2,]

# legendaX <- paste(format.POSIXlt(date1,"%A-%d-%m-%y"))
# g <- ggplot(nsessoes,aes(x=nsessoes$timeop, y=nsessoes$Current, size=nsessoes$New)) + geom_point() + scale_x_datetime(date_breaks = "100 mins", date_labels = "%H:%M")
# g <- g + labs(title=legendaX, x="Hora", y="Sessões Ativas", size="Novas Sessões") 
# ggsave(paste(legendaX,"-nsessoes.png",sep=""))
# 
# f <- ggplot(dataTraffic,aes(x=dataTraffic$times, y=dataTraffic$sum)) + geom_line() 
# f <- f + scale_x_datetime(date_breaks = "100 mins", date_labels = "%H:%M")
# f <- f + scale_y_continuous(labels = function(x) paste(x*10^-6,"MB"),breaks=seq(min(dataTraffic$sum), max(dataTraffic$sum)+50*10^6, by=50*10^6))
# f <- f + labs(title=legendaX, x="Hora", y="Download Total em 5 minutos", size="Novas Sessões") 
# ggsave(paste(legendaX,"-download.png",sep=""), plot = f)
