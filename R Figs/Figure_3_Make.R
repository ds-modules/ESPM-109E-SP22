## R code to produce Figure 3, and the accompanying analyses presented in the text, in: 
## Burgess SC, Johnston EC, Wyatt ASJ, Leichter JJ, Edmunds PJ (in press) Response diversity in corals: hidden differences in bleaching mortality among cryptic Pocillopora species. Ecology.
## Code written by Scott Burgess. Feb 2021. Send comments or corrections to sburgess@bio.fsu.edu
## R version 3.6.3 (2020-02-29)


# Load required library
library(plyr)


# Import data
# setwd("")
dat <- read.csv("Data on Bleaching 2019.csv")

# Prepare data
dat$Site <- as.factor(dat$Site)
dat$Quadrat <- as.factor(dat$Quadrat)
dat$Site.Quadrat <- interaction(dat$Site,dat$Quadrat)
dat$BleachAny <- ifelse(dat$Bleaching=="0",0,1)
dat$Bleach1 <- ifelse(dat$Bleaching=="1",1,
							ifelse(dat$Bleaching=="0",0,NA))
dat$Bleach2 <- ifelse(dat$Bleaching=="2",1,
							ifelse(dat$Bleaching=="0",0,NA))
dat$Bleach3 <- ifelse(dat$Bleaching=="3",1,
							ifelse(dat$Bleaching=="0",0,NA))


# Subset data into each month
datmarch <- dat[dat$Month=="March",]
datmay <- dat[dat$Month=="May",]


# Sample sizes
# Size and bleaching were measured for 641 out of 1023 (63%) colonies assigned to a bleaching category in May 2019.
length(datmay[!is.na(datmay$Longest.cm) & !is.na(datmay$BleachAny),which(names(datmay)=="Longest.cm")]) # 641
length(datmay$Bleaching[!is.na(datmay$Bleaching)])	# 1023
(641/1032) * 100 # 62.11%

# Sample sizes at each site (from Figure 3 caption)
# n=328 at Site 1
length(datmay[datmay$Site=="1" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$BleachAny),which(names(datmay)=="Longest.cm")])
# n=313 at Site 2
length(datmay[datmay$Site=="2" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$BleachAny),which(names(datmay)=="Longest.cm")])


# Prepare data for Figure 3
foo <- datmay[datmay$Site=="1",which(names(datmay)=="Longest.cm")]
LengthvecS1 <- seq(min(foo,na.rm=T),max(foo,na.rm=T),length=100)
foo <- datmay[datmay$Site=="2",which(names(datmay)=="Longest.cm")]
LengthvecS2 <- seq(min(foo,na.rm=T),max(foo,na.rm=T),length=100)
l <- c(LengthvecS1, LengthvecS2)

newdat <- data.frame(Longest.cm=l,Site=as.factor(sort(rep(1:2,100))))

## BleachAny in May
m1 <- glm(BleachAny ~ Longest.cm * Site, data=datmay,family="binomial")
p <- predict(m1,newdat,se.fit=T)
pred.BleachAny.Length.May <- data.frame(newdat,
	mean=p$fit,
	upr = p$fit + 2*p$se.fit,
	lwr = p$fit - 2*p$se.fit)

## Bleach1 in May
m1 <- glm(Bleach1 ~ Longest.cm * Site, data=datmay,family="binomial")
p <- predict(m1,newdat,se.fit=T)
pred.Bleach1.Length.May <- data.frame(newdat,
	mean=p$fit,
	upr = p$fit + 2*p$se.fit,
	lwr = p$fit - 2*p$se.fit)

## Bleach2 in May
m1 <- glm(Bleach2 ~ Longest.cm * Site, data=datmay,family="binomial")
p <- predict(m1,newdat,se.fit=T)
pred.Bleach2.Length.May <- data.frame(newdat,
	mean=p$fit,
	upr = p$fit + 2*p$se.fit,
	lwr = p$fit - 2*p$se.fit)

	
## Bleach3 in May
m1 <- glm(Bleach3 ~ Longest.cm * Site, data=datmay,family="binomial")
p <- predict(m1,newdat,se.fit=T)
pred.Bleach3.Length.May <- data.frame(newdat,
	mean=p$fit,
	upr = p$fit + 2*p$se.fit,
	lwr = p$fit - 2*p$se.fit)
	

# Make Figure 3  
labs <- c("Partial","Moderate","Severe","Any")
x.ticks <- seq(0,45,5) 
panel.labs <- c("a) Site 1 (10m)","b) Site 2 (10m)")
points.cex <- 1.5
ax.cex <- 1.2
x.ax <- seq(0,1,0.2)
cols <- c("grey80","grey60","grey30","black")
tcksize <- 0.05
xmin=0
xmax <- 45
brks <- seq(0,xmax,5)

dev.new(width=8,height=4)
nf <- layout(matrix(1:4,2,2,byrow=T),c(3,3),c(2,0.6,2,0.6))
# layout.show(n=4)
par(oma=c(2,2,1,0),mar=c(0,0,0,0))

## Site 1
par(mar=c(1,3,1,1))
plot(c(min(x.ticks),max(x.ticks)),c(0,1),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=x.ticks,cex.axis=ax.cex)
axis(side=2,at=x.ax,las=1,cex.axis=ax.cex)
mtext(panel.labs[1],side=3,line=-1,cex=ax.cex,adj=0.05)

# Site 1 Bleach1
maxsize <- max(datmay[datmay$Site=="1" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$Bleach1),which(names(datmay)=="Longest.cm")])
foo <- pred.Bleach1.Length.May[pred.Bleach1.Length.May$Site=="1" &
				pred.Bleach1.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[1]))

# Site 1 Bleach2
maxsize <- max(datmay[datmay$Site=="1" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$Bleach2),which(names(datmay)=="Longest.cm")])
foo <- pred.Bleach2.Length.May[pred.Bleach2.Length.May$Site=="1" &
				pred.Bleach2.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[2]))

# Site 1 Bleach3
maxsize <- max(datmay[datmay$Site=="1" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$Bleach3),which(names(datmay)=="Longest.cm")])
foo <- pred.Bleach3.Length.May[pred.Bleach3.Length.May$Site=="1" &
				pred.Bleach3.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[3]))

# Site 1 BleachAny
maxsize <- max(datmay[datmay$Site=="1" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$BleachAny),which(names(datmay)=="Longest.cm")])
foo <- pred.BleachAny.Length.May[pred.BleachAny.Length.May$Site=="1" &
				pred.BleachAny.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[4]))

## Site 2 
plot(c(min(x.ticks),max(x.ticks)),c(0,1),type="n",ylab="",xlab="",yaxt="n",xaxt="n",bty="l")
axis(side=1,at=x.ticks,cex.axis=ax.cex)
axis(side=2,at=x.ax,las=1,cex.axis=ax.cex)
mtext(panel.labs[2],side=3,line=-1,cex=ax.cex,adj=0.05)

# Site 2 Bleach1
maxsize <- max(datmay[datmay$Site=="2" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$Bleach1),which(names(datmay)=="Longest.cm")])
foo <- pred.Bleach1.Length.May[pred.Bleach1.Length.May$Site=="2" &
				pred.Bleach1.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[1]))

# Site 2 Bleach2
maxsize <- max(datmay[datmay$Site=="2" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$Bleach2),which(names(datmay)=="Longest.cm")])
foo <- pred.Bleach2.Length.May[pred.Bleach2.Length.May$Site=="2" &
				pred.Bleach2.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[2]))

# Site 2 Bleach3
maxsize <- max(datmay[datmay$Site=="2" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$Bleach3),which(names(datmay)=="Longest.cm")])
foo <- pred.Bleach3.Length.May[pred.Bleach3.Length.May$Site=="2" &
				pred.Bleach3.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[3]))

# Site 2 BleachAny
maxsize <- max(datmay[datmay$Site=="2" &
			!is.na(datmay$Longest.cm) & !is.na(datmay$BleachAny),which(names(datmay)=="Longest.cm")])
foo <- pred.BleachAny.Length.May[pred.BleachAny.Length.May$Site=="2" &
				pred.BleachAny.Length.May$Longest.cm <max(maxsize),]
with(foo, lines(Longest.cm,plogis(mean),lwd=2,col=cols[4]))

legend('bottomright',legend=labs,lwd=2,col=cols,bty="n",title="Bleaching",cex=1.2)


foo <- datmay[datmay$Site=="1" & !is.na(datmay$Longest.cm),which(names(datmay)==c("Longest.cm"))]
histlim <- round_any(max(foo,na.rm=T),5,f=ceiling)
hist(foo,breaks=seq(0,histlim,5),freq=F,main="",xaxt="n",col="grey",cex.axis=1.2,las=1,ylim=c(0,0.1),xlim=c(xmin,xmax))
foo <- datmay[datmay$Site=="2" & !is.na(datmay$Longest.cm),which(names(datmay)==c("Longest.cm"))]
histlim <- round_any(max(foo,na.rm=T),5,f=ceiling)
hist(foo,breaks=seq(0,histlim,5),freq=F,main="",xaxt="n",col="grey",cex.axis=1.2,las=1,ylim=c(0,0.1),xlim=c(xmin,xmax))

mtext(side=1,line=0,"Colony size (longest diameter, cm)",outer=T,adj=0.5,cex=ax.cex)
mtext(side=2,line=0,"Probability of bleaching",outer=T,adj=0.8,cex=ax.cex)






# Statements in results section
# How many corals sampled in each site? (this includes corals with no size measurement)
with(datmay[!is.na(datmay$BleachAny),],table(Site)) # Site 1 = 546; Site 2 = 477

newdat <- expand.grid(Site=c("1","2"))

## BleachAny at each site in May 2019
m1 <- glm(BleachAny ~ Site, data=datmay,family="binomial")
p <- predict(m1,newdat,se.fit=T)
pred.BleachAny.Site.May <- data.frame(newdat,
	mean=p$fit,
	upr = p$fit + 2*p$se.fit,
	lwr = p$fit - 2*p$se.fit)

## Bleach3 at each site in May 2019
m1 <- glm(Bleach3 ~ Site, data=datmay,family="binomial")
p <- predict(m1,newdat,se.fit=T)
pred.Bleach3.Site.May <- data.frame(newdat,
	mean=p$fit,
	upr = p$fit + 2*p$se.fit,
	lwr = p$fit - 2*p$se.fit)

# Bleaching to some degree
# Site 1: 71% (67 - 75, 95%, CI)
plogis(pred.BleachAny.Site.May[1,2]) * 100
plogis(pred.BleachAny.Site.May[1,4]) * 100
plogis(pred.BleachAny.Site.May[1,3]) * 100
# Site 2: 72% (68 - 76, 95% CI)
plogis(pred.BleachAny.Site.May[2,2]) * 100
plogis(pred.BleachAny.Site.May[2,4]) * 100
plogis(pred.BleachAny.Site.May[2,3]) * 100

# Severely bleached
# Site 1: 55% (49 - 60, 95%, CI)
plogis(pred.Bleach3.Site.May[1,2]) * 100
plogis(pred.Bleach3.Site.May[1,4]) * 100
plogis(pred.Bleach3.Site.May[1,3]) * 100
# Site 2: 60% (55 - 65, 95% CI)
plogis(pred.Bleach3.Site.May[2,2]) * 100
plogis(pred.Bleach3.Site.May[2,4]) * 100
plogis(pred.Bleach3.Site.May[2,3]) * 100




