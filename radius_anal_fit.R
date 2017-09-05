setwd('~/TCC/bdradius')

date1 <- strptime("2017-06-28 00:00","%Y-%m-%d %H:%M")
date2 <- strptime("2017-06-29 00:00","%Y-%m-%d %H:%M")

print("Reading sessions..,")
sessions <- read.csv("sessions.csv", sep=";", stringsAsFactors = FALSE)
colnames(sessions) <- c("radacctid","username","nasportid","acctstarttime","acctstoptime","acctsessiontime","acctinputoctets","acctoutputoctets","callingstationid","terminatedcause","framedip","timeop")
sessions$acctstarttime <- paste(sessions$acctstarttime, "00", sep ="")
sessions$acctstarttime <- strptime(sessions$acctstarttime, "%Y-%m-%d %H:%M:%S%z")
sessions$acctstoptime <- paste(sessions$acctstoptime, "00", sep ="")
sessions$acctstoptime <- strptime(sessions$acctstoptime, "%Y-%m-%d %H:%M:%S%z")
sessions$timeop <- paste(sessions$timeop, "00", sep ="")
sessions$timeop <- strptime(sessions$timeop, "%Y-%m-%d %H:%M:%OS%z")


sessions <- sessions[!is.na(sessions$acctstoptime),]
sessions <- sessions[!is.na(sessions$acctinputoctets),]
sessions <- sessions[!is.na(sessions$acctoutputoctets),]
sessions <- sessions[sessions$acctstarttime > date1 & sessions$acctstoptime < date2,]

print("Reading traffic,..")

traffic <- read.csv("traffic.csv", sep=",")
colnames(traffic) <- c("radacctid","acctsessiontime", "inputoctets", "outputoctets", "timeop")
traffic$timeop <- paste(traffic$timeop, "00", sep ="")
traffic$timeop <- strptime(traffic$timeop, "%Y-%m-%d %H:%M:%OS%z")
traffic$diff <- 0
traffic$ID <- as.character(seq.int(nrow(traffic)))
traffic$outputoctets <- traffic$outputoctets
rownames(traffic) <- traffic$ID

traffic <- traffic[!is.na(traffic$outputoctets),]
traffic <- traffic[traffic$timeop>=date1 & traffic$timeop<=date2,]


#dev.off()
print("Calculating diff ")

i <- 1
for (radacctid in sessions$radacctid){
        session_traffic <- traffic[traffic$radacctid==radacctid,]
        session_traffic$diff <- c(session_traffic$outputoctets[1], diff(session_traffic$outputoctets))
        for (j in 1:nrow(session_traffic)){
                ses <- session_traffic[j,]
                traffic[ses$ID,6] <- ses$diff
        }

        i <- i+1
}


histTraffic = traffic[traffic$timeop >= date1 & traffic$timeop <= date2, ]
i <- 1
times <- unique(histTraffic$timeop)
dataTraffic <- data.frame(times, sum=c(1:length(times)))

for (i in 1:length(times)){
        timeop <- times[i]
        timeop_traffic <- histTraffic[histTraffic$timeop == timeop,]
        dataTraffic$sum[i] <-  sum(timeop_traffic$diff)
        i < i + 1
}

print("Started to plot...")

pdf("Graph1.pdf",width=10,height=7)
par(mfrow=c(1,2))

xrange = range(date1, date2)
yrange = c(0,max(traffic$diff))
plot(xrange, yrange, ylim=c(0,max(traffic$diff)), type='n', xlab = "Time", ylab="Outputoctets")

i <- 1
for (radacctid in sessions$radacctid){
        session_traffic <- traffic[traffic$radacctid==radacctid,]
        x <- session_traffic$timeop
        y <- session_traffic$diff
        lines(x,y, type="o", col=i)
        i <- i+1
}
# xrange <- range(min(sessions$timeop),max(sessions$timeop))
yrange <- range(0,max(dataTraffic$sum))
#plot(xrange, yrange, type='n', xlab = "Time", ylab="Summed Outputoctets")
#for (i in 1:length(times)){
#        traf <- dataTraffic[i,]
#        lines(traf$times, traf$sum, col=i, type='h', lwd='3')
#}
plot(dataTraffic, type='l')
dev.off()

