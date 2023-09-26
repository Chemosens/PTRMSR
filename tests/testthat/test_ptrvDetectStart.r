data("ptrv")
library(reshape2)
library(ggplot2)
library(signal)

breath="m69.06989..isoprene...Conc."
res_intensity=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)
res_intensity=ptrvIntensityByTime(ptrv,referenceBreath=breath,correction = "cycle")

sigIons=ptrvSignificantSNRIons(ptrv,referenceBreath=breath,multiplyNoiseBy=4,removeNoise=FALSE)
listSig=sigIons$listIons
summary(sigIons$snRatio)
IonToUse=names(sigIons$snRatio)[which.max(sigIons$snRatio)]

rest=ptrvDetectStart(res=res_intensity$res,starts=IonToUse, method="startPeakProportion")
p=ggplot(res_intensity$res[res_intensity$res[,"ion"]%in%listSig,],aes(x=time,y=intensity,group=ion,color=ion))+geom_line()
p=p+geom_vline(xintercept=rest$tx)
#ggplotly(p)

rest2=ptrvDetectStart(res=res_intensity$res,starts=listSig, method="higherThanNoise",noisePeriod=c(0,30))
p2=ggplot(res_intensity$res[res_intensity$res[,"ion"]%in%listSig,],aes(x=time,y=intensity,group=ion,color=ion))+geom_line()
p2=p2+geom_vline(xintercept=rest2$tx)
#ggplotly(p)

ptrvDetectStart(res=res_intensity$res,starts=IonToUse, method="higherThanNoise",multiplyNoiseBy = 2,noisePeriod=c(0,30))

test_that("start peak proportion",
          expect_true(round(rest$tx,digits=5)<35)
)


#ptrv2=read.table(file="C://INRA//Data//Données Cantin//Cantin-DTS-PTRviewer//CSCA098_S002_2.txt",sep="\t",dec=",",header=T)
#res_intensity=ptrvIntensityByTime(ptrv2,referenceBreath="m69.06906..69.06906...Conc.",correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)
#ion_tds=ptrvSignificantSNRIons(res_intensity,referenceBreath="m69.06906..69.06906...Conc.",multiplyNoiseBy=4,removeNoise=FALSE)

#rest=ptrvDetectStart(res=res_intensity$res,starts=ion_tds$intersection, method="startPeakProportion",proportionOfMax=0.1,noisePeriod=c(0,25),startPeriod=c(25,40))

#ptrv3=read.table(file="C://INRA//Data//Données Cantin//Cantin-DTS-PTRviewer//CSCA102_S005_2.txt",sep="\t",dec=".",header=T)
#res_intensity=ptrvIntensityByTime(ptrv3,referenceBreath="m69.06906..69.06906...Conc.",correction = "cycle",removeNoise=FALSE,breathRatio = FALSE)
#rest=ptrvDetectStart(res=res_intensity$res,starts=ion_tds$intersection, method="startPeakProportion",proportionOfMax=0.1,noisePeriod=c(0,25),startPeriod=c(25,40))



#referenceBreathingLong="m69.06906..69.06906...Conc."


#test sur données de Christopher
#=================================
# setwd("C:/Users/capeltier/Desktop/DataAnalysis/Chistopher PTR/Caroline_Peltier/A1")
# files=list.files()
# file=files[1]
# e2mb="m131.09549..Ethyl.2methylbutyrate...Conc."
# ptrFile_i=read.table(file,sep="\t",header=T)
# tempsSequences=read.xlsx(xlsxFile="C:/Users/capeltier/Desktop/DataAnalysis/Chistopher PTR/Caroline_Peltier/Temps_sequence.xlsx",startRow=6)[1:2,]
# startPeaks=tempsSequences[tempsSequences[,"Temps.apparition"]==substr(file,8,11),-1]
#
#
# nbStimulations=length(startPeaks)
# finalMatrix=rep(NA,nbStimulations);names(finalMatrix)=paste0("pic",1:nbStimulations)
# finalMatrix2=finalMatrix3=finalMatrix
# ions=colnames(ptrFile_i)[-1]
# longDf=reshape(ptrFile_i,direction="long",varying=list(ions),times=ions)
# colnames(longDf)=c("time","ion","intensity","id")
# starts=c(e2mb)
# p=ggplot(longDf[longDf[,"ion"]%in%starts,],aes(x=time,y=intensity))+geom_line(color="grey")+theme_bw()
# stimulation=data.frame(X1=t(startPeaks))
# p=p+geom_vline(data=stimulation,aes(xintercept=X1))
# # ggplotly(p)
# problematicPeaks=rep("no",nbStimulations)
# localMax=rep(NA,nbStimulations)
# minTime=startPeaks[1]
# ImaxAllPeriod=max(longDf[longDf[,"time"]>minTime,"intensity"])
# relativeIntensityThreshold=rit*ImaxAllPeriod
# endPeaks=NULL
# i=1
# startPeak=as.numeric(as.character(startPeaks[i]))
# if(is.null(endPeaks))
# {
#   if(i!=nbStimulations){endPeak=as.numeric(as.character(startPeaks[i+1]))}else{endPeak=max(ptrFile_i[,"RelTime"],na.rm=T)}
# }
# c(startPeak, endPeak)
startPeriod=c(33.6, 48)
starts="m131.09549..Ethyl.2methylbutyrate...Conc."
# test higher than noise
#==========================
# defaut: le max du bruit défini par la periode
#save(longDf,file="longDf.RData")
data(longDf)
res_higherThanNoise=ptrvDetectStart(res=longDf,starts=starts,startPeriod=c(33.6, 48),method="higherThanNoise",multiplyNoiseBy=1.5,noisePeriod=c(10,30),statOfNoise="avg",peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50)
res_higherThanNoise$diagnosis=="ok"
res_higherThanNoise$tx
res_higherThanNoise$potentialPeaks
library(plotly)
ggplotly(res_higherThanNoise$gg)

test_that("Diagnostic higher than noise - rounded value",expect_true(round(res_higherThanNoise$tx,digits=2)==35.22))
test_that("Diagnostic higher than noise ",expect_true(res_higherThanNoise$diagnosis=="ok"))

# Bruit sur l'ensemble de la degustation
res_higherThanNoise2=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherThanNoise",multiplyNoiseBy=3,peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50,noisePeriod=NULL,statOfNoise="bl")
test_that("Diagnostic higher than noise 2- rounded value",expect_true(round(res_higherThanNoise2$tx,digits=2)==35.29))
test_that("Diagnostic higher than noise 2",expect_true(res_higherThanNoise2$diagnosis=="ok"))
# Bruit sur la periode dans lequel on cherche le start
res_higherThanNoise3=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherThanNoise",multiplyNoiseBy=3,peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50,noisePeriod=startPeriod,statOfNoise="blperiod")
test_that("Diagnostic higher than noise 3- rounded value",expect_true(round(res_higherThanNoise3$tx,digits=2)==35.32))
test_that("Diagnostic higher than noise 3",expect_true(res_higherThanNoise3$diagnosis=="ok"))

# Statistique de l'intensité observée sur un intervalle de bruit fixé
res_higherThanNoise4=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherThanNoise",multiplyNoiseBy=3,peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50,noisePeriod=c(20,30),statOfNoise="max")
test_that("Diagnostic higher than noise 4- rounded value",expect_true(round(res_higherThanNoise4$tx,digits=2)==35.26))
test_that("Diagnostic higher than noise 4",expect_true(res_higherThanNoise4$diagnosis=="ok"))

# changement de limite  pour la taille des pics détectés
res_higherThanNoise5=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherThanNoise",multiplyNoiseBy=3,peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=10,noisePeriod=startPeriod,statOfNoise="blperiod")
test_that("Diagnostic higher than noise 5- rounded value",expect_true(round(res_higherThanNoise5$tx,digits=2)==35.32))
test_that("Diagnostic higher than noise 5",expect_true(res_higherThanNoise5$diagnosis=="ok"))

# test detectStart
#=====================
res_startPeakProportion=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="startPeakProportion",proportionOfMax=0.001,peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50)
test_that("Start Peak Proportion",expect_true(round(res_startPeakProportion$tx,digits=2)==35.19))
test_that("Start Peak Proportion diagnosis",expect_true(res_startPeakProportion$diagnosis=="ok"))
res_startPeakProportion$gg

# test Derivative
res_higherDerivative=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherDerivative",proportionOfMax=0.001,peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50)
test_that("higher derivative",expect_true(round(res_higherDerivative$tx,digits=2)==35.87))
ggplotly(res_higherDerivative$gg)

res_higherDerivative2=ptrvDetectStart(res=longDf,order=2,starts=starts,startPeriod=startPeriod,method="higherDerivative",proportionOfMax=0.001,peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50)
test_that("higher derivative 2",expect_true(round(res_higherDerivative2$tx,digits=2)==35.63))

# test des cas particuliers
#=============================
res_higherThanNoise6=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherThanNoise",multiplyNoiseBy=1.5,noisePeriod=c(0,30),statOfNoise="max",peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50)
test_that("Diagnosis Intensity not reached",expect_true(res_higherThanNoise6$diagnosis=="IntensityNotReached"))
res_higherThanNoise7=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherThanNoise",multiplyNoiseBy=1.5,noisePeriod=c(0,30),statOfNoise="avg",peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50,detectionThreshold=250)
test_that("Diagnosis Max Peak Too Low",expect_true(res_higherThanNoise7$diagnosis=='maxPeakTooLow'))
res_higherThanNoise8=ptrvDetectStart(res=longDf,starts=starts,startPeriod=startPeriod,method="higherThanNoise",multiplyNoiseBy=1.5,noisePeriod=c(0,30),statOfNoise="avg",peakChoice="firstAmongHigh",nPoints=31,smooth=TRUE,firstAmongHighThreshold=0.01)
test_that("Diagnosis Time found after peak",expect_true(res_higherThanNoise8$diagnosis=="IntensityNotReached"))

ggplotly(res_higherThanNoise8$gg+xlim(startPeriod)+geom_vline(xintercept=34.7))
# Cas où on n'a pas de pic mais une croissance
#==============================================
time=1:1000
intensity=c(rep(0,500),1:500)+0.1
ion="ion"
res=data.frame(time,intensity,ion)
res_base1=ptrvDetectStart(res=res,starts="ion",startPeriod=c(1,1000),method="higherThanNoise",multiplyNoiseBy=1.5,noisePeriod=c(1,30),statOfNoise="avg",peakChoice="firstAmongHigh",nPoints=31,smooth=FALSE,firstAmongHighThreshold=50,detectionThreshold=250)
test_that("Lines without peak without smooth and firstAmongHigh",expect_true(round(res_base1$tx,digits=2)==500.05))
res_base2=ptrvDetectStart(res=res,starts="ion",startPeriod=c(1,1000),method="higherThanNoise",peakChoice="maxIntensity",multiplyNoiseBy=1.5,noisePeriod=c(1,30),statOfNoise="avg",nPoints=31,smooth=TRUE,firstAmongHighThreshold=50,detectionThreshold=250)
test_that("Lines without peak with smooth and maxIntensity",expect_true(round(res_base2$tx,digits=4)==496.1363 ))
res_base3=ptrvDetectStart(res=res,starts="ion",startPeriod=c(1,1000),method="higherThanNoise",peakChoice="maxIntensity",multiplyNoiseBy=1.5,noisePeriod=c(1,30),statOfNoise="avg",nPoints=0,smooth=FALSE,firstAmongHighThreshold=50,detectionThreshold=250)
test_that("Lines without peak without smooth and maxIntensity",expect_true(round(res_base3$tx,digits=2)==500.05))

