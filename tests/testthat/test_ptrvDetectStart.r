data("ptrv")
library(reshape2)
breath="m69.06989..isoprene...Conc."
library(ggplot2)
library(signal)

res_intensity=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)

sigIons=ptrvSignificantSNRIons(ptrv,referenceBreath=breath,multiplyNoiseBy=4,removeNoise=FALSE)
listSig=sigIons$listIons
summary(sigIons$snRatio)
IonToUse=names(sigIons$snRatio)[which.max(sigIons$snRatio)]

rest=ptrvDetectStart(res=res_intensity$res,starts=IonToUse, method="startPeakProportion")
p=ggplot(res_intensity$res[res_intensity$res[,"ion"]%in%listSig,],aes(x=time,y=intensity,group=ion,color=ion))+geom_line()
p=p+geom_vline(xintercept=rest$tx)
#ggplotly(p)

rest2=ptrvDetectStart(res=res_intensity$res,starts=listSig, method="higherThanNoise")
p=ggplot(res_intensity$res[res_intensity$res[,"ion"]%in%listSig,],aes(x=time,y=intensity,group=ion,color=ion))+geom_line()
p=p+geom_vline(xintercept=rest$tx)
#ggplotly(p)




ptrvDetectStart(res=res_intensity$res,starts=IonToUse, method="higherThanNoise",multiplyNoiseBy = 2)

test_that("start peak proportion",
          expect_true(round(rest$tx,digits=5)==33.03863)
)


#ptrv2=read.table(file="C://INRA//Data//Données Cantin//Cantin-DTS-PTRviewer//CSCA098_S002_2.txt",sep="\t",dec=",",header=T)
#res_intensity=ptrvIntensityByTime(ptrv2,referenceBreath="m69.06906..69.06906...Conc.",correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)
#ion_tds=ptrvSignificantSNRIons(res_intensity,referenceBreath="m69.06906..69.06906...Conc.",multiplyNoiseBy=4,removeNoise=FALSE)

#rest=ptrvDetectStart(res=res_intensity$res,starts=ion_tds$intersection, method="startPeakProportion",proportionOfMax=0.1,noisePeriod=c(0,25),startPeriod=c(25,40))

#ptrv3=read.table(file="C://INRA//Data//Données Cantin//Cantin-DTS-PTRviewer//CSCA102_S005_2.txt",sep="\t",dec=".",header=T)
#res_intensity=ptrvIntensityByTime(ptrv3,referenceBreath="m69.06906..69.06906...Conc.",correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)
#rest=ptrvDetectStart(res=res_intensity$res,starts=ion_tds$intersection, method="startPeakProportion",proportionOfMax=0.1,noisePeriod=c(0,25),startPeriod=c(25,40))



#referenceBreathingLong="m69.06906..69.06906...Conc."



