setwd('~/TCC/')

session <- read.csv(file="./bdradius/session-20170627.csv", sep=";", stringsAsFactors = FALSE)
session$acctstarttime <- paste(session$acctstarttime,"00", sep="")
session$acctstarttime <- strptime(session$acctstarttime,"%Y-%m-%d %H:%M:%OS%z")
session$acctstoptime <- paste(session$acctstoptime,"00", sep="")
session$acctstoptime <- strptime(session$acctstoptime,"%Y-%m-%d %H:%M:%OS%z")
session$timeop <- paste(session$timeop,"00", sep="")
session$timeop <- strptime(session$timeop,"%Y-%m-%d %H:%M:%OS%z")
rownames(session) <- session$radacctid

traffic <- read.csv(file="./bdradius/traffic-20170627.csv", stringsAsFactors = FALSE)
traffic$timeop <- paste(traffic$timeop,"00", sep="")
traffic$timeop = strptime(traffic$timeop,"%Y-%m-%d %H:%M:%OS%z")

date1 = strptime("2017-06-27 13:00", "%Y-%m-%d %H:%M")
date2 = strptime("2017-06-27 17:00", "%Y-%m-%d %H:%M")
sessions = session[! is.na(session$acctstoptime) ,]
sessions = sessions[! is.na(sessions$acctinputoctets) ,]
sessions = sessions[! is.na(sessions$acctoutputoctets) ,]
sessions = sessions[sessions$acctstarttime > date1 & sessions$acctstoptime < date2 ,]

xrange = range(min(sessions$acctstarttime), max(sessions$acctstoptime))
inputrange = range(sessions$acctinputoctets)
outputrange = range(sessions$acctoutputoctets* 0.0009766)



plot(xrange, outputrange, type="n", xlab="Date",
     ylab="Input Octets" ) 

i <- 1
for (radacctid in sessions$radacctid){
        session_traffic = traffic[traffic$radacctid == radacctid,]
        x = session_traffic$timeop
        y = session_traffic$acctoutputoctets * 0.0009766
        lines(x, y, type="l", col=i ) 
        i <- i + 1
}

