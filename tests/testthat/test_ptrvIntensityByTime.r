data(ptrv)
# All defaults
# Correction = none
breath="m69.06989..isoprene...Conc."

# All defaults
res=ptrvIntensityByTime(ptrv)
ions=unique(res$res[,"ion"])
ion=ions[6]
resIons=res$res[res$res[,"ion"]==ion,]
plot(resIons[,"time"],resIons[,"intensity"],type="l")

bool0=sum(resIons[,"intensity"]==ptrv[,ion],na.rm=T)==sum(!is.na(ptrv[,ion]))
test_that("intensity identical for no correction",expect_true(bool0))

bool1=sum(resIons[,"duration"]==c(resIons[-1,"time"]-resIons[-length(resIons[,"time"]),"time"],0))==dim(resIons)[1]
test_that("valid duration for no correction",expect_true(bool1))

# Adding total
res2=ptrvIntensityByTime(ptrv,total=TRUE)
#bool2=sum(dim(res2$res)==c(173952,7))==2
bool2="total"%in%unique(res2$res[,"ion"])
test_that("total ions added",expect_true(bool2))

# Time period
timePeriod=c(15,35)
res3=ptrvIntensityByTime(ptrv,timePeriod=timePeriod)
res_period=res$res[res$res[,"time"]>=timePeriod[1]&res$res[,"time"]<=timePeriod[2],]
#bool3=sum(res_period[1:170,1:6]!=res3$res[1:170,1:6],na.rm=T)==0&sum(res_period[801:900,1:6]!=res3$res[801:900,1:6],na.rm=T)==0
bool3=sum(res_period[,"time"]>timePeriod[2]&res_period[,"time"]<timePeriod[1])
test_that("timePeriod correct",expect_true(bool3==0))

timePeriod=c(15,35);timeStart=15
res4=ptrvIntensityByTime(ptrv,timePeriod=timePeriod,timeStart=timeStart)
bool4=mean(res4$res[,"time"])==mean(res3$res[,"time"])-timeStart
test_that("time start correct",expect_true(bool4))

# Breath ratio
res5=ptrvIntensityByTime(ptrv,referenceBreath=breath,breathRatio = T)
sum(res5$res[res5$res[,"ion"]==breath,"intensity"]!=rep(1,sum(res5$res[,"ion"]==breath)),na.rm=T)==0
test_that("breath ratio correct",expect_true(bool4))


# Cycle correction
#====================
res=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle")
grid.arrange(grobs=res$gg)
summary(res$res)
# Smoothing parameter
res=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5)
grid.arrange(grobs=res$gg)

# Max peak parameter (jusque quelle intensité doit on redescendre sur les données smoothées pour que ça compte comme un "creux")
res=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5,minExpi=1)
grid.arrange(grobs=res$gg)

res=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5,maxInspi=22)
grid.arrange(grobs=res$gg)




# Breath ratio # TDODO
ions=unique(res$res[,"ion"])


#dcasted2[,c("time","duration")]=as.character(dcasted2[,c("time","duration")])
#,varying=list(indexIons2),times=colnames(dataset)[indexIons2],v.names="intensity",timevar=)
# checking results

resultp=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",
                             timePeriod=NULL,timeStart=0,removeNoise=FALSE,
                             timeBlank=c(0,30),halfWindowSize=10, maxInspi =50,
                             total=FALSE,breathRatio=FALSE)
summary(resultp$res)


#res=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5,maxPeaks=1)
timint=resultp$res
timint[,"xmin"]=as.numeric(as.character(timint[,"time"]))-as.numeric(as.character(timint[,"duration"]))/2
timint[,"xmax"]=as.numeric(as.character(timint[,"time"]))+as.numeric(as.character(timint[,"duration"]))/2
timint[,"ymin"]=0
timint[,"ymax"]=timint[,"intensity"]/timint[,"duration"]
p1=ggplot(data=timint[timint[,"ion"]==unique(timint[,"ion"])[1],],aes(xmin=xmin,ymax=ymax,ymin=ymin,xmax=xmax,group=ion,fill=ion))+geom_rect()+theme(legend.position="none")
p2=ggplot(data=timint[timint[,"ion"]==unique(timint[,"ion"])[2],],aes(xmin=xmin,ymax=ymax,ymin=ymin,xmax=xmax,group=ion,fill=ion))+geom_rect()+theme(legend.position="none")
p3=ggplot(data=timint[timint[,"ion"]==unique(timint[,"ion"])[3],],aes(xmin=xmin,ymax=ymax,ymin=ymin,xmax=xmax,group=ion,fill=ion))+geom_rect()+theme(legend.position="none")
p4=resultp$gg[[2]]
grid.arrange(grobs=list(p1,p2,p3,p4),nrow=4)

res1=ptrvIntensityByTime(ptrv,correction="cycle",referenceBreath=breath,smoothMethod="SavitzkyGolay",ion=c("m89.07743..C5H13O....Conc."))
res=ptrvIntensityByTime(ptrv,correction="cycle",referenceBreath=breath,removeNoise=FALSE,ion=c("m89.07743..C5H13O....Conc."))
ggplotly(res$gg$p_smoothcycle)
 p=res$gg$p_breath
 p2=res$gg$p_cyclelimits
 p3=res$gg$p_smoothcycle
 grid.arrange(p,p2,p3)

 ggplotly(res1$gg$p_smoothcycle)
 p10=res1$gg$p_breath
 p12=res1$gg$p_cyclelimits
 p13=res1$gg$p_smoothcycle
 grid.arrange(p10,p12,p13)

# cycle limits
 res=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",minimalDuration=3)
 summary(res$res[,"duration"])

