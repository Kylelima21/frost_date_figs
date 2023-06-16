##
#McFarland Hill Daily Data
#downloaded 4/3/2017

library(plyr)
#
setwd("/Users/Nick/Documents/Projects/ACAD_managed-relocation/ClimateData")

#HCN Acadia station data from cdiac.ornl
t1<-read.csv("ME170100_2551daily.csv", header=TRUE)
head(t1)
str(t1)

t1.2<-t1[t1$Year > 1982, c(1:5,10)]
t1.3<-na.omit(t1.2)

#calculate last frost, here defined as last spring day with min temp <= -1 C
SprFrost<-ddply(t1.3, "Year", function(df)
  return(c(lastFrost=max(df$JD[df$TminF <= 32.0 & df$JD < 180]))))

JD<-t1.3[ ,c(1:3,5)]

SpFrDate<-merge(SprFrost, JD, by.x=c("Year", "lastFrost"), by.y=c("Year", "JD"))

plot(SpFrDate$Year, SpFrDate$lastFrost,
     ylab="Julian Date", xlab="Year",
     main="McFarland Hill Last Spring Frost")
abline(120, 0, col="blue")
lines(SpFrDate$Year, SpFrDate$lastFrost)
text(2013.5, 118, "April", col="blue")
abline(151, 0, col="dark grey")
text(2013.8, 149, "May", col="dark grey")
text(2013.8, 122, "May", col="dark grey")
text(2013.5, 154, "June", col="orange")
text(1986.5, 109, "McFarland Hill Weather Station Data", cex=0.5)
text(1985, 107.5, "Created by N. Fisichelli", cex=0.5)

abline(lm(lastFrost ~ Year, data=SpFrDate))

min(SpFrDate$lastFrost)

#linear model
m1<-lm(lastFrost ~ Year, data=SpFrDate)
summary(m1)
############################################################
############################################################
############################################################




############################################################
############################################################
############################################################
#New code 4/9/18 combining two data sets from NCDC
############################################################
############################################################
############################################################


############################################################

t1<-read.csv("ME_McFar_combi20180409.csv", header=TRUE)
head(t1)
str(t1)

t1.2<-t1[t1$Year > 1982, c(1:5,8)]
t1.3<-na.omit(t1.2)

#calculate last frost, here defined as last spring day with min temp <= -1 C
SprFrost<-ddply(t1.3, "Year", function(df)
  return(c(lastFrost=max(df$JD[df$TminF <= 32.0 & df$JD < 180]))))

JD<-t1.3[ ,c(1:3,5)]

SpFrDate<-merge(SprFrost, JD, by.x=c("Year", "lastFrost"), by.y=c("Year", "JD"))

plot(SpFrDate$Year, SpFrDate$lastFrost,
     ylab="Julian Date", xlab="Year",
     main="Acadia NP Last Spring Frost (1982-2017)")
abline(120, 0, col="blue")
lines(SpFrDate$Year, SpFrDate$lastFrost)
text(1983.1, 118, "April", col="blue")
abline(151, 0, col="dark grey")
text(1983.1, 149, "May", col="dark grey")
text(1983.1, 122, "May", col="dark grey")
text(1983.1, 154, "June", col="orange")
#text(1986.5, 109, "McFarland Hill Weather Station Data", cex=0.5)
text(1985.5, 107.5, "Created by N. Fisichelli", cex=0.5)

abline(lm(lastFrost ~ Year, data=SpFrDate), col="red")

min(SpFrDate$lastFrost)

#linear model
m1<-lm(lastFrost ~ Year, data=SpFrDate)
summary(m1)

rm(JD, m1, SpFrDate, SprFrost, t1, t1.2, t1.3)




###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
### First Fall Frost ###
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
#calculate last frost, here defined as last spring day with min temp <= -1 C
FalFrost<-ddply(t1.3, "Year", function(df)
  return(c(firstFrost=min(df$JD[df$TminF <= 32.0 & df$JD > 180]))))

JD<-t1.3[ ,c(1:3,5)]

FalFrDate<-merge(FalFrost, JD, by.x=c("Year", "firstFrost"), by.y=c("Year", "JD"))


plot(FalFrDate$Year, FalFrDate$firstFrost,
     ylab="Julian Date", xlab="Year",
     main="Acadia NP First Autumn Frost (1983-2017)")
lines(FalFrDate$Year, FalFrDate$firstFrost)
abline(274, 0, col="dark grey")
text(1984, 307, "November", col="blue")
abline(305, 0, col="blue")
text(1983.5, 290, "October", col="dark grey")
#text(1983.1, 278, "October", col="dark grey")
text(1984, 270, "September", col="orange")
#text(1986.5, 109, "McFarland Hill Weather Station Data", cex=0.5)
text(1985.5, 107.5, "Created by N. Fisichelli", cex=0.5)

abline(lm(firstFrost ~ Year, data=FalFrDate), col="red")

min(SpFrDate$lastFrost)

#linear model
m2<-lm(firstFrost ~ Year, data=FalFrDate)
summary(m2)







