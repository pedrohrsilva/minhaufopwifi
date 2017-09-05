setwd('~/TCC/bdradius')

date1 <- strptime("2017-07-20 13:00","%Y-%m-%d %H:%M")
date2 <- strptime("2017-07-21 22:00","%Y-%m-%d %H:%M")



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
#sessions <- sessions[sessions$acctstarttime >= date1 & sessions$acctstoptime <= date2,]

print("Reading traffic,..")

traffic <- read.csv("traffic.csv", sep=",")
colnames(traffic) <- c("radacctid","acctsessiontime", "inputoctets", "outputoctets", "timeop")
traffic$timeop <- paste(traffic$timeop, "00", sep ="")
traffic$timeop <- strptime(traffic$timeop, "%Y-%m-%d %H:%M:%OS%z")
traffic$diffDown <- 0
traffic$diffUp <- 0
traffic$ID <- as.character(seq.int(nrow(traffic)))
rownames(traffic) <- traffic$ID

traffic <- traffic[!is.na(traffic$outputoctets),]
traffic <- traffic[!is.na(traffic$inputoctets),]
#traffic <- traffic[traffic$timeop >= date1 & traffic$timeop <= date2,]


#dev.off()
print("Calculating diff ")

i <- 1
tam <- nrow(sessions)
for (radacctid in sessions$radacctid){
        print(paste(i,"/",tam))
        session_traffic <- traffic[traffic$radacctid==radacctid,]
        session_traffic$diffDown <- c(0, diff(session_traffic$outputoctets))
        session_traffic$diffUp <- c(0, diff(session_traffic$inputoctets))
        for (j in 1:nrow(session_traffic)){
                ses <- session_traffic[j,]
                traffic[ses$ID,6] <- max(0,ses$diffDown)
                traffic[ses$ID,7] <- max(0,ses$diffUp)
        }
        
        i <- i+1
}


histTraffic = traffic
i <- 1

times <- unique(histTraffic$timeop)
times <- times[date1 < times & times < date2]
dataTraffic <- data.frame(times, download=c(1:length(times), upload=c(1:length(times))))
tam <- length(times)
for (i in 1:length(times)){
        timeop <- times[i]
        timeop_traffic <- histTraffic[histTraffic$timeop == timeop,]
        dataTraffic$download[i] <-  sum(timeop_traffic$diffDown)
        dataTraffic$upload[i] <-  sum(timeop_traffic$diffUp)
        print(paste(i,"/",tam))
}
