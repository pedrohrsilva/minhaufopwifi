top10Horas <- table(format(round(sesTop10$acctstarttime, units="hours"), format="%H:%M"))
total10Horas <- table(format(round(sessions$acctstarttime, units="hours"), format="%H:%M"))

top10Horas <- as.data.frame(top10Horas)
colnames(top10Horas) <- c("x", "y")
total10Horas <- as.data.frame(total10Horas)
colnames(total10Horas) <- c("x", "y")
top10Horas$x <- as.POSIXct(strptime(as.character(top10Horas$x), "%H:%M"))
total10Horas$x <- as.POSIXct(strptime(as.character(total10Horas$x), "%H:%M"))

lim <- as.POSIXct(c(strptime("00:00", "%H:%M")-3600,strptime("23:00", "%H:%M")+3600))

g <- ggplot(top10Horas, aes(x=x, y=y)) + geom_col(aes(x,y)) #+ geom_col(aes(top10Horas$x,top10Horas$y,fill="Top10"))
g <- g + scale_x_datetime(date_breaks = "1 hours",date_labels = "%H:%M")
g <- g+ theme(axis.text.x=element_text(angle = -90, hjust = 0))
g

