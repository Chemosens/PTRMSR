library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
data("ptrv")
breath="m69.06989..isoprene...Conc."
# Without cycle correction
res0=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction = "none",timePeriod=c(0,120))
resint=ptrvIntensity(res0$res)
#theoreticalArea=sum(resIons[,"duration"]*resIons[,"intensity"],na.rm=T)
#test_that("valid area for no correction",expect_true(bool1))
# With cycle
res=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction = "cycle")
resNoise=ptrvIntensity(res=res$res,timePeriod=c(0,30))
resEnd=ptrvIntensity(res=res$res,timePeriod=c(30,120))
resTwo=ptrvIntensity(res=res$res,timePeriod=c(0,120))

# Checking the max
bool1=sum(1!=(resTwo[,"max"]==resEnd[,"max"])+(resTwo[,"max"]==resNoise[,"max"]))==0
test_that("max decomposition",
          expect_true(bool1)
)
# Checking the area # potentially sensitive to durations
bool2=max(abs(resEnd[,"area"]+resNoise[,"area"]-resTwo[,"area"]))<1e-7
test_that("area decomposition",
          expect_true(bool2)
)
# Checking aire
bool3=sum((resEnd[,"sum"]+resNoise[,"sum"]-resTwo[,"sum"]<1e-12))==length(resEnd[,"sum"])
test_that("aire decomposition",
          expect_true(bool3)
)
# Visualiser le jdd brut ()


p_resp_raw=ggplot(ptrv,aes(x=RelTime,y=m69.06989..isoprene...Conc.))+geom_line()+ggtitle("all")+theme_bw()+xlim(0,120)
ggplotly(p_resp_raw)


# Jeu de données brutes sans corrections
res=ptrvIntensityByTime(dataset=ptrv,
                        referenceBreath=breath,correction="none",
                        timePeriod=NULL,timeStart=0,removeNoise=FALSE,timeBlank=30,halfWindowSize=10)
p=ggplot(res$res,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()
ggplotly(p)

# visualiser un jeu données avec corrections de cycles (mais pas de bruit)
res_corrige=ptrvIntensityByTime(dataset=ptrv,
                        referenceBreath=breath,correction="cycle",
                        timePeriod=NULL,timeStart=0,removeNoise=FALSE,timeBlank=30,halfWindowSize=10)
p=ggplot(res_corrige$res,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()
ggplotly(p)

# visualiser un jeu données avec corrections de cycles et de bruit
res_corrige=ptrvIntensityByTime(dataset=ptrv,
                                referenceBreath=breath,correction="cycle",
                                timePeriod=NULL,timeStart=0,removeNoise=TRUE,timeBlank=30,halfWindowSize=10)
p=ggplot(res_corrige$res,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme_bw()
ggplotly(p)



# validation de l'option "maxPeaks" qui donne 
res_max=ptrvIntensityByTime(dataset=ptrv,
                        referenceBreath=breath,
                        correction="cycle",
                        timePeriod=c(0,120),
                        timeStart=0,removeNoise=FALSE,timeBlank=30,
                        halfWindowSize=5,
                        maxPeaks=50)


p_resp_raw=p_resp_raw+geom_vline(xintercept=res_max$res$time,color="red")
p_resp_raw
#p_resp=ggplot(result_all[result_all[,"ion"]=="total",],aes(x=time,y=intensity))+geom_line()+theme(legend.position="none")+ggtitle("all")


ion=unique(res$res[,"ion"])[4]
ion=breath
resdf=res$res[res$res[,"ion"]==ion,]
dataset2=ptrv[,c("RelTime",ion)];colnames(dataset2)=c("time","intensity")
p_ion_raw=ggplot(dataset2,aes(x=time,y=intensity))+geom_line()+ggtitle(ion)+xlim(0,120)
p_ion_raw=p_ion_raw+geom_vline(xintercept=res$time,color="red")
p_ion=ggplot(resdf[resdf[,"ion"]==ion,],aes(x=time,y=intensity))+geom_line()+theme(legend.position="none")+ggtitle(ion)
grid.arrange(p_ion_raw,p_ion)

p_resp_raw=ggplot(dataset2,aes(x=RelTime,y=breath))+geom_line()+ggtitle("isoprene")+xlim(0,120)
p_resp_raw=p_resp_raw+geom_vline(xintercept=res$res$time,color="red")
p_resp=ggplot(res$res[res$res[,"ion"]=="breath",],aes(x=time,y=intensity))+geom_line()+theme(legend.position="none")+ggtitle("isoprene")


p=ggplot(resdf,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme(legend.position="none")
p=ggplot(resdf[resdf[,"ion"]!="total",],aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+theme(legend.position="none")+ylim(0,10)
ggplotly(p)

