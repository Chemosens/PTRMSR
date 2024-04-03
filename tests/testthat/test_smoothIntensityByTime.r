
 data(ptrv)
 time=ptrv$RelTime
 intensity=ptrv$m47.04914..Ethanol...Conc.
 breath="m69.06989..isoprene...Conc."
 res_ibt=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5,removeNoise = FALSE)
 time2_ini=res_ibt$res[res_ibt$res[,"ion"]=="m47.04914..Ethanol...Conc.","time"]
 intensity2_ini=res_ibt$res[res_ibt$res["ion"]=="m47.04914..Ethanol...Conc.","intensity"]
 time2=time2_ini[order(time2_ini)]
 intensity2=intensity2_ini[order(time2_ini)]

 res_ibt=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5,removeNoise = FALSE,funAggregate ="max" )
 timemax_ini=res_ibt$res[res_ibt$res[,"ion"]=="m47.04914..Ethanol...Conc.","time"]
 intensitymax_ini=res_ibt$res[res_ibt$res["ion"]=="m47.04914..Ethanol...Conc.","intensity"]
 timemax=timemax_ini[order(timemax_ini)]
 intensitymax=intensitymax_ini[order(timemax_ini)]

 res_loess=loess(intensity~time)
 plot(res_loess$fitted)

 res_loess
 res05=smoothIntensityByTime(time,intensity,spar=0.5)
 res01=smoothIntensityByTime(time,intensity,spar=0.1)
 res09=smoothIntensityByTime(time,intensity,spar=0.9)
 res_loess05=smoothIntensityByTime(time,intensity,method="Loess",spar=0.5)
 res_loess09=smoothIntensityByTime(time,intensity,method="Loess",spar=0.9)
 res_sg5=smoothIntensityByTime(time,intensity,method="SavitzkyGolay",spar=5)
 res_sg50=smoothIntensityByTime(time,intensity,method="SavitzkyGolay",spar=50)
 res_sg100=smoothIntensityByTime(time,intensity,method="SavitzkyGolay",spar=100)
 res_ma5=smoothIntensityByTime(time,intensity,method="MovingAverage",spar=5)
 res_ma50=smoothIntensityByTime(time,intensity,method="MovingAverage",spar=50)
 res_ma100=smoothIntensityByTime(time,intensity,method="MovingAverage",spar=100)

 #res_sg_raw=
 par(mfrow=c(5,3))
 par(mar=c(2,2,1,1))
 plot(time,intensity,type="l",main="no smooth")
 plot(res01$time,res01$intensity,type="l",main="spar = 0.1")
 plot(res05$time,res05$intensity,type="l",main="spar = 0.5")
 plot(res09$time,res09$intensity,type="l",main="spar = 0.9")
 plot(res_loess05$time,res_loess05$intensity,type="l",main="Loess (spar=0.5)")
 plot(res_loess09$time,res_loess09$intensity,type="l",main="Loess (spar=0.9)")
 plot(res_sg5$time,res_sg5$intensity,type="l",main="Savitzky-Golay (spar=5)")
 plot(res_sg50$time,res_sg50$intensity,type="l",main="Savitzky-Golay (spar=50)")
 plot(res_sg100$time,res_sg100$intensity,type="l",main="Savitzky-Golay (spar=200)")
 plot(res_ma5$time,res_ma5$intensity,type="l",main="Moving Average (spar=5)")
 plot(res_ma50$time,res_ma50$intensity,type="l",main="MovingAverage (spar=50)")
 plot(res_ma100$time,res_ma100$intensity,type="l",main="MovingAverage (spar=200)")
 plot(time2,intensity2,main="Cycle Correction",type="l")
 plot(timemax,intensitymax,main="Cycle Correction",type="l")

 df_loess09=data.frame("time"=res_loess09$time,"intensity"=res_loess09$intensity)
 colnames(df_loess09)=c("time","intensity")
 df_loess09[,"ion"]="ion"
 df_loess09[,"duration"]=c(0,df_loess09[-1,"time"]-df_loess09[-nrow(df_loess09),"time"])
 resint=ptrvIntensity( dataset=df_loess09)


res_ibt=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction="cycle",halfWindowSize=5,removeNoise = FALSE,funAggregate ="max" )
res_sm=ptrvSmooth(dataset=res_ibt$res,method="Spline",spar=NULL,sameTime=TRUE,time_x=NULL,negativeValuesToZero=TRUE)
ggplot(res_sm,aes(x=time,y=intensity,col=ion))+geom_line()  +theme(legend.position="none")
