dim(turnout)
summary(turnout)
range(turnout$year)
turnout$real_turnout_rate <- turnout[4]/turnout[3]*100
turnout$VEP_rate <- (turnout[4]+turnout[8])/turnout[2]*100
turnout$difference <- turnout[5]-turnout[11]
mean(turnout[,13])
turnout$difference
mean(turnout$difference)
turnout$difVEP <- turnout [5]-turnout[12]
mean(turnout$difference)
older <- turnout[c(1:7),]
View(older)
newer<- turnout[c(8:14),]
View(newer)
