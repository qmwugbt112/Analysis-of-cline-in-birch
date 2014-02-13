library(boot) 	# has logit function
library(nlme)	# has mixed effects models

# Read in data
Nian.dat<-read.table(file.choose(),T)

# Have a peek at data
head(Nian.dat)

attach(Nian.dat)

# Analyse nana introgression
lnana<-logit(nana)

mod1<-lme(lnana~distance,random=~1|Pop)
mod1a<-lme(lnana~Latitude,random=~1|Pop)

summary(mod1a)
plot(Latitude,lnana,cex=.5,ylab='Logit( nana introgression )',xlab='Latitude')
points(Latitude,fitted(mod1,level=1),col='red',pch=5)
abline(mod1a$coefficients$fixed)


nIters<-10000
dist2<-Latitude*0
Pop2<-Pop

slopeDistn<-vector(length=nIters)
for (i in 1:nIters){
	#randomize popuation allocations
	levels(Pop2)<-sample(levels(Pop),nlevels(Pop))
	for (j in levels(Pop)) dist2[Pop==j]<-mean(Latitude[Pop2==j]) 
	slopeDistn[i]<-lme(lnana~dist2,random=~1|Pop)$coefficients$fixed[2]
}

summary(mod1a)
quantile(slopeDistn,p=1-c(0.05,0.01,0.001)/2)

#plot(distance,distance*-0.001435-4.647462-fitted(mod1))
#abline(0,0)



# Analyse pendula introgression
lpend<-logit(pend)

mod2<-lme(lpend~Latitude,random=~1|Pop)

summary(mod2)
plot(Latitude,lpend,cex=.5,ylab='Logit( pendula introgression )',xlab='Latitude')
points(Latitude,fitted(mod2,level=1),col='red',pch=5)
abline(mod2$coefficients$fixed)
#plot(Latitude,Latitude*-0.001435-4.647462-fitted(mod1))
#abline(0,0)

nIters<-10000
dist2<-Latitude*0
Pop2<-Pop

slopeDistn<-vector(length=nIters)
for (i in 1:nIters){
	#randomize popuation allocations
	levels(Pop2)<-sample(levels(Pop),nlevels(Pop))
	for (j in levels(Pop)) dist2[Pop==j]<-mean(Latitude[Pop2==j]) 
	slopeDistn[i]<-lme(lpend~dist2,random=~1|Pop)$coefficients$fixed[2]
}

summary(mod2)
quantile(-slopeDistn,p=1-c(0.05,0.01,0.001)/2)