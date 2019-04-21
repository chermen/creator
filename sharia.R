#sharia paper Politics and Religion, 2016

setwd("~")

library(foreign)
library(MASS)
library(ordinal)
library(texreg)
library(coefplot)
library(reshape2)
library(ggplot2)
library(Zelig)



shar2<-read.csv("shar_regcont3.csv", header=T,sep=",") ##data with reg controls



names(shar2)

##with fixed effects
c1<-clm(as.factor(sharia2) ~ age+income+regov+urban+native+
          male+edu+disrep+privrep+means+biz+repower
          +fedcorr+Total.victims+
          av.salary+Unemployment, data=shar2)          
summary(c1)


##without fixed effects
c2<-clm(as.factor(sharia2) ~ age+income+regov+urban+native+
          male+edu+disrep+privrep+means+biz+repower+
          fedcorr, data=shar2)          
summary(c2) 

##tables

texreg(list(c2,c1),dcolumn = TRUE,booktabs=T)

##coefficient plot
plotreg(list(c2,c1))

##OLS models
##with fixed effects
lm1<-lm(sharia ~ age+income+regov+urban+native+
          male+edu+disrep+privrep+means+biz+repower
          +fedcorr+Total.victims+
          av.salary+Unemployment, data=shar2)          
summary(lm1)

##without fixed effects
lm2<-lm(sharia ~ age+income+regov+urban+native+
          male+edu+disrep+privrep+means+biz+repower+
          fedcorr, data=shar2)          
summary(lm2)

texreg(list(lm2,lm1),dcolumn = TRUE,booktabs=T)

###predicted probs



####Produce the coefplot

library(coefplot)
g<-multiplot(c2, c1, 
             title="The Political Economy of Support for Sharia in the North Caucasus",
             xlab="Coefficient Estimates", ylab="", decreasing=T,
             intercept=F, sort="magnitude", names=c(
          "No Fixed Effects", "Fixed Effects"), innerCI = 1,
  outerCI = 2, lwdInner = 2, lwdOuter = 1, dodgeHeight = 0.8,
  pointSize = 5, legend.position = "bottom",
  plot.shapes =T, 
          coefficients=c("privrep", "fedcorr",
           "biz", "repower", "pmeans", 
          "disrep", "edu", "age", "income", "male", 
          "urban", "regov", "native", "norepgov", "means"),
          newNames=c(
          fedcorr="Fed Corruption", privrep=
          "Private", biz="Business", repower=
          "Regional Power",
          disrep="Dissatisfied",
          edu="Education", age="Age", income=
          "Income", male="Male", urban="Urban",
          regov="Unpopularity", native="Native",
          means="Means of Production"))

g+theme(panel.background=element_blank(),
plot.title = element_text(size = rel(1)))

ggsave("noshar.pdf",width=7,height=7)
dev.off()

##### marginal effect (MLE)

##full model without fixed effects
names(shar)
f<-zelig(as.factor(sharia2) ~ age+income+regov+urban+native+
           male+edu+disrep+privrep+means+biz+repower+
           fedcorr,model="ologit", data=shar2) 
summary(f)

##function to calculate standard errors
se <- function(x) sqrt(var(x)/length(x))

##Private
###run zelig simulations


hi.pr <- setx(f, privrep=quantile(shar2$privrep, prob=0.75))
lo.pr <- setx(f, privrep =quantile(shar2$privrep, prob=0.25))

plo<- sim(f, x = lo.pr)
phi<- sim(f, x = hi.pr)

pl<-melt(as.data.frame(plo$qi$ev))
ph<-melt(as.data.frame(phi$qi$ev))

pcom<-rbind(ph[201:400,],pl[201:400,])
p<-data.frame(level=rep(c("high","low"),each=200),
              meff=rep("private"), pcom)
p$err<-rep(tapply(p$value,p$level, FUN=se), each=200)
p$mean<-rep(tapply(p$value,p$level, FUN=mean), each=200)
limits1 <- aes(ymax = p$mean + p$err*1.96, ymin= p$mean - p$err*1.96)



####Effect of Means of Production

hi.me <- setx(f, pmeans=1)
lo.me<- setx(f, pmeans =0)

melo<- sim(f, x = lo.me)
mehi<- sim(f, x = hi.me)

mel<-melt(as.data.frame(melo$qi$ev))
meh<-melt(as.data.frame(mehi$qi$ev))

mecom<-rbind(meh[201:400,],mel[201:400,])
mea<-data.frame(level=rep(c("high","low"),each=200), 
                meff=rep("means"),mecom)
mea$err<-rep(tapply(mea$value,mea$level, FUN=se), each=200)
mea$mean<-rep(tapply(mea$value,mea$level, FUN=mean), each=200)
limits2 <- aes(ymax = mea$pmean + mea$err*1.96, ymin= mea$pmean - mea$err*1.96)



####Effect of Business
summary(shar2$means)
hi.biz <- setx(f, biz=quantile(shar2$biz, prob=0.80))
lo.biz <- setx(f, biz =quantile(shar2$biz, prob=0.20))

bizlo<- sim(f, x = lo.biz)
bizhi<- sim(f, x = hi.biz)

bizl<-melt(as.data.frame(bizlo$qi$ev))
bizh<-melt(as.data.frame(bizhi$qi$ev))

bizcom<-rbind(bizh[201:400,],bizl[201:400,])
bizn<-data.frame(level=rep(c("high","low"),each=200), 
                 meff=rep("business"),bizcom)
bizn$err<-rep(tapply(bizn$value,bizn$level, FUN=se), each=200)
bizn$mean<-rep(tapply(bizn$value,bizn$level, FUN=mean), each=200)
limits3 <- aes(ymax = bizn$mean + bizn$err*1.96, ymin= bizn$mean - bizn$err*1.96)


####Effect of Regional Power
summary(shar2$repower)
hi.pow  <- setx(f, repower=quantile(shar2$repower, prob=0.75))
lo.pow <- setx(f, repower =quantile(shar2$repower, prob=0.25))

powlo<- sim(f, x = lo.pow)
powhi<- sim(f, x = hi.pow)

powl<-melt(as.data.frame(powlo$qi$ev))
powh<-melt(as.data.frame(powhi$qi$ev))

powcom<-rbind(powh[201:400,],powl[201:400,])
pown<-data.frame(level=rep(c("high","low"),each=200), 
                 meff=rep("power"),powcom)
pown$err<-rep(tapply(pown$value,pown$level, FUN=se), each=200)
pown$mean<-rep(tapply(pown$value,pown$level, FUN=mean), each=200)
limits4 <- aes(ymax = pown$mean + pown$err*1.96, ymin= pown$mean - pown$err*1.96)



####Effect of Ethnic Quota
summary(shar2$equota)
hi.eq <- setx(f, equota=1)
lo.eq <- setx(f, equota=0)

eqlo<- sim(f, x = lo.eq)
eqhi<- sim(f, x = hi.eq)

eql<-melt(as.data.frame(eqlo$qi$ev))
eqh<-melt(as.data.frame(eqhi$qi$ev))

eqcom<-rbind(eqh[201:400,],eql[201:400,])
eqn<-data.frame(level=rep(c("high","low"),each=200),
                meff=rep("equota"),eqcom)
eqn$err<-rep(tapply(eqn$value,eqn$level, FUN=se), each=200)
eqn$mean<-rep(tapply(eqn$value,eqn$level, FUN=mean), each=200)
limits5 <- aes(ymax = eqn$mean + eqn$err, ymin= eqn$mean - eqn$err)



####Effect of Federal Corruption
summary(shar2$fedcorr)
hi.fcor <- setx(f, fedcorr=quantile(shar2$fedcorr, prob=0.70))
lo.fcor <- setx(f, fedcorr=quantile(shar2$fedcorr, prob=0.30))

fcorlo<- sim(f, x = lo.fcor)
fcorhi<- sim(f, x = hi.fcor)

fcorl<-melt(as.data.frame(fcorlo$qi$ev))
fcorh<-melt(as.data.frame(fcorhi$qi$ev))

fcorcom<-rbind(fcorh[201:400,],fcorl[201:400,])
fcorn<-data.frame(level=rep(c("high","low"),each=200),
                  meff=rep("fcorr"),fcorcom)
fcorn$err<-rep(tapply(fcorn$value,fcorn$level, FUN=se), each=200)
fcorn$mean<-rep(tapply(fcorn$value,fcorn$level, FUN=mean), each=200)
limits6 <- aes(ymax = fcorn$mean + fcorn$err*1.96, ymin= fcorn$mean - fcorn$err*1.96)



com<-rbind(fcorn,pown,bizn,mea,p)

limits<-aes(ymax = com$mean + com$err, ymin= com$mean - com$err)

cplot <- ggplot(com, aes(y=mean, x=meff, color=level))
cplot + geom_point(aes(shape = level, size=1)) + 
  geom_errorbar(limits, width=0.2)+
  guides(size=F)+
  labs(title = "Marginal Effects", y="Probability of Support for Sharia",
       x="")+
  scale_x_discrete(breaks=c("fcorr","power","business",
                            "means","private"),
                   labels=c("Fed Corr", "Power",
                            "Business", "Means", "Private"))+
  scale_color_discrete(name="Level",
                      breaks=c("high", "low"),
                      labels=c("High", "Low"))+
  scale_shape_discrete(name="Level",
                       breaks=c("high", "low"),
                       labels=c("High", "Low"))+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = rel(1)))

panel.background=element_blank(),
ggsave("probs.pdf",width=7,height=7)
dev.off()
            


#####
#####Marginal Effect. Combined Y
#####
#####

##function to calculate standard errors
se <- function(x) sqrt(var(x)/length(x)) 

##Private
###run zelig simulations
hi.pr <- setx(f, privrep=quantile(shar2$privrep, prob=0.75))
lo.pr <- setx(f, privrep =quantile(shar2$privrep, prob=0.25))

plo<- sim(f, x = lo.pr)
phi<- sim(f, x = hi.pr)


pl<-melt(as.data.frame(plo$qi$pr))
ph<-melt(as.data.frame(phi$qi$pr))

pcom<-rbind(ph,pl)
p<-data.frame(level=rep(c("high","low"),each=100),
              meff=rep("private"),pcom)
p$var<-as.numeric(p$X1)
p$err<-rep(tapply(p$var,p$level, FUN=se), each=100)
p$mean<-rep(tapply(p$var,p$level, FUN=mean),each=100)
limits1 <- aes(ymax = p$mean + p$err*1.96, ymin= p$mean - p$err*1.96)



####Effect of Means of Production

hi.me <- setx(f, pmeans=1)
lo.me<- setx(f, pmeans =0)

melo<- sim(f, x = lo.me)
mehi<- sim(f, x = hi.me)

mel<-melt(as.data.frame(melo$qi$pr))
meh<-melt(as.data.frame(mehi$qi$pr))

mecom<-rbind(meh,mel)
mea<-data.frame(level=rep(c("high","low"),each=100), 
                meff=rep("means"),mecom)
mea$var<-as.numeric(mea$X1)
mea$err<-rep(tapply(mea$var,mea$level, FUN=se), each=100)
mea$mean<-rep(tapply(mea$var,mea$level, FUN=mean), each=100)
limits2 <- aes(ymax = mea$mean + mea$err*1.96, ymin= mea$mean - mea$err*1.96)





####Effect of Business
summary(shar2$means)
hi.biz <- setx(f, biz=quantile(shar2$biz, prob=0.80))
lo.biz <- setx(f, biz =quantile(shar2$biz, prob=0.20))

bizlo<- sim(f, x = lo.biz)
bizhi<- sim(f, x = hi.biz)

bizl<-melt(as.data.frame(bizlo$qi$pr))
bizh<-melt(as.data.frame(bizhi$qi$pr))

bizcom<-rbind(bizh,bizl)
bizn<-data.frame(level=rep(c("high","low"),each=100), 
                 meff=rep("business"),bizcom)
bizn$var<-as.numeric(bizn$X1)
bizn$err<-rep(tapply(bizn$var,bizn$level, FUN=se), each=100)
bizn$mean<-rep(tapply(bizn$var,bizn$level, FUN=mean), each=100)
limits3 <- aes(ymax = bizn$mean + bizn$err*1.96, ymin= bizn$mean - bizn$err*1.96)


####Effect of Regional Power
summary(shar2$repower)
hi.pow  <- setx(f, repower=quantile(shar2$repower, prob=0.75))
lo.pow <- setx(f, repower =quantile(shar2$repower, prob=0.25))

powlo<- sim(f, x = lo.pow)
powhi<- sim(f, x = hi.pow)

powl<-melt(as.data.frame(powlo$qi$pr))
powh<-melt(as.data.frame(powhi$qi$pr))

powcom<-rbind(powh,powl)
pown<-data.frame(level=rep(c("high","low"),each=100), 
                 meff=rep("power"),powcom)
pown$var<-as.numeric(pown$X1)
pown$err<-rep(tapply(pown$var,pown$level, FUN=se), each=100)
pown$mean<-rep(tapply(pown$var,pown$level, FUN=mean), each=100)
limits4 <- aes(ymax = pown$mean + pown$err*1.96, ymin= pown$mean - pown$err*1.96)



####Effect of Ethnic Quota
summary(shar2$equota)
hi.eq <- setx(f, equota=1)
lo.eq <- setx(f, equota=0)

eqlo<- sim(f, x = lo.eq)
eqhi<- sim(f, x = hi.eq)

eql<-melt(as.data.frame(eqlo$qi$pr))
eqh<-melt(as.data.frame(eqhi$qi$pr))

eqcom<-rbind(eqh,eql)
eqn<-data.frame(level=rep(c("high","low"),each=100),
                meff=rep("equota"),eqcom)
eqn$var<-as.numeric(eqn$X1)
eqn$err<-rep(tapply(eqn$var,eqn$level, FUN=se), each=100)
eqn$mean<-rep(tapply(eqn$var,eqn$level, FUN=mean), each=100)
limits5 <- aes(ymax = eqn$mean + eqn$err, ymin= eqn$mean - eqn$err)


####Effect of Federal Corruption
summary(shar2$fedcorr)
hi.fcor <- setx(f, fedcorr=quantile(shar2$fedcorr, prob=0.70))
lo.fcor <- setx(f, fedcorr=quantile(shar2$fedcorr, prob=0.30))

fcorlo<- sim(f, x = lo.fcor)
fcorhi<- sim(f, x = hi.fcor)

fcorl<-melt(as.data.frame(fcorlo$qi$pr))
fcorh<-melt(as.data.frame(fcorhi$qi$pr))

fcorcom<-rbind(fcorh,fcorl)
fcorn<-data.frame(level=rep(c("high","low"),each=100),
                  meff=rep("fcorr"),fcorcom)
fcorn$var<-as.numeric(fcorn$X1)
fcorn$err<-rep(tapply(fcorn$var,fcorn$level, FUN=se), each=100)
fcorn$mean<-rep(tapply(fcorn$var,fcorn$level, FUN=mean), each=100)
limits6 <- aes(ymax = fcorn$mean + fcorn$err*1.96, ymin= fcorn$mean - fcorn$err*1.96)



####Effect of Federal Rights

summary(shar2$fedrite)
hi.fedr <- setx(f, fedrite=quantile(shar2$fedrite, prob=0.75))
lo.fedr <- setx(f, fedrite=quantile(shar2$fedrite, prob=0.25))

fedrlo<- sim(f, x = lo.fedr)
fedrhi<- sim(f, x = hi.fedr)

fedrl<-melt(as.data.frame(fedrlo$qi$pr))
fedrh<-melt(as.data.frame(fedrhi$qi$pr))

fedrcom<-rbind(fedrh,fedrl)
fedrn<-data.frame(level=rep(c("high","low"),each=100), 
                  meff=rep("frite"),fedrcom)
fedrn$var<-as.numeric(fedrn$X1)
fedrn$err<-rep(tapply(fedrn$var,fedrn$level, FUN=se), each=100)
fedrn$mean<-rep(tapply(fedrn$var,fedrn$level, FUN=mean), each=100)
limits7 <- aes(ymax = fedrn$mean + fedrn$err, ymin= fedrn$mean - fedrn$err)

com<-rbind(fcorn,pown,bizn,mea,p)
limits<-aes(ymax = com$mean + com$err*1.96, ymin= com$mean - com$err*1.96)

###stack
cplot <- ggplot(com, aes(colour=level, y=mean, x=meff))
cplot + geom_point(aes(shape = level,size=1)) + 
  geom_errorbar(limits, width=0.2)+
  guides(size=F)+
coord_cartesian(ylim = c(1, 2))+
  labs(title = "Effects of Low and High Values on Support for Sharia", 
       y="Support for Sharia",
       x="Variables")+
  scale_x_discrete(breaks=c("fcorr","power","business",
                            "means","private"),
                   labels=c("Fed Corr", "Power",
                            "Business", "Means", "Private"))
ggsave("mprivate.pdf")
dev.off()

###side by side

labeli <- function(variable, value){
  names_li <- list("fcorr"="Fed Corr", "power"="Power",
                   "business"="Business", "means"="Means", "private"="Private")
  return(names_li[value])
}

cplot <- ggplot(com, aes(colour=level, y=mean, x=level))
cplot + geom_point(aes(shape = level,size=1)) + 
  geom_errorbar(limits, width=0.2)+
  guides(size=F)+
  coord_cartesian(ylim = c(1, 2.5))+
  facet_grid(~ meff, labeller=labeli)+
  labs(title = "Marginal Effects", 
       y="Support for Sharia",
       x="")+
  scale_color_discrete(name="Level",
                       breaks=c("high", "low"),
                       labels=c("High", "Low"))+
  scale_shape_discrete(name="Level",
                       breaks=c("high", "low"),
                       labels=c("High", "Low"))+
  theme(axis.ticks = element_blank(),axis.text.x = element_blank(),
        plot.title = element_text(size = rel(1)))



  ggsave("values2.pdf",width=7,height=7)
dev.off()


