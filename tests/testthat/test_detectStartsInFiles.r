setwd("C:/Users/capeltier/Desktop/DataAnalysis/Old scripts/Christopher PTR/Caroline_Peltier/A1")
files=list.files()
file=files[1]
library(openxlsx)
e2mb="m131.09549..Ethyl.2methylbutyrate...Conc."
ptrvFile_i=read.table(file,sep="\t",header=T)
tempsSequences=read.xlsx(xlsxFile="C:/Users/capeltier/Desktop/DataAnalysis/Old scripts/Christopher PTR/Caroline_Peltier/Temps_sequence.xlsx",startRow=6)[1:2,]
startPeaks=t(as.vector(tempsSequences[tempsSequences[,"Temps.apparition"]==substr(file,8,11),-1]))
minTime=tempsSequences[tempsSequences[,"Temps.apparition"]==substr(file,8,11),2]

stat3=read.xlsx(xlsxFile="C:/Users/capeltier/Desktop/DataAnalysis/Old scripts/Christopher PTR/Caroline_Peltier/Statistique_3.xlsx")
manual=data.frame(X1=as.numeric(stat3[stat3[,"Fichier"]==substr(file,1,23),"Latence"])/1000)
#setwd("C:/INRA/PTRMSR/Data")
#save(manual,file="manual.RData")
#save(startPeaks,file="startPeaks.RData")
#save(ptrvFile_i,file="ptrvFile_i.RData")
data(manual)
#data(stimulation)
data(ptrvFile_i)
res=detectStartsInFile(ptrFile_i=ptrvFile_i,ion=e2mb,startPeaks=startPeaks,nPoints=31,multiplyNoiseBy=2.576 ,statOfNoise="blperiod",smooth=TRUE,proportionOfMax=0.005,order=2,peakChoice="firstAmongHigh")
res_manual=detectStartsInFile(ptrFile_i=ptrvFile_i,ion=e2mb,startPeaks=startPeaks,nPoints=31,multiplyNoiseBy=2.576 ,statOfNoise="blperiod",smooth=TRUE,proportionOfMax=0.005,order=2,peakChoice="firstAmongHigh",manual=manual)

test_that("detect with noise option", expect_true(round(median(abs(res_manual$dfres$diff_higherThanNoise),na.rm=T),digits=2)==0.04))
test_that("detect with intensity proportion option", expect_true(round(median(abs(res_manual$dfres$diff_startPeakProportion),na.rm=T),digits=2)==0.05))
test_that("detect with derivative", expect_true(round(median(abs(res_manual$dfres$diff_higherDerivative),na.rm=T),digits=2)==0.58))

test_that("size of diagnoses", expect_true(sum(dim(res$diagnosis)==c(3,20))==2))
test_that("size of diagnoses", expect_true(sum(dim(res_manual$diagnosis)==c(3,20))==2))
res_manual$gg

res_manual2=detectStartsInFile(ptrFile_i=ptrvFile_i,ion=e2mb,startPeaks=startPeaks,nPoints=31,multiplyNoiseBy=2.576 ,statOfNoise="blperiod",smooth=TRUE,proportionOfMax=0.005,order=2,peakChoice="firstAmongHigh",manual=manual,firstAmongHighThreshold=10)
#summary(abs(res_manual2$dfres[,-1]))

# only one method
res=detectStartsInFile(ptrFile_i=ptrvFile_i,ion=e2mb,startPeaks=startPeaks,method="higherThanNoise",nPoints=31,multiplyNoiseBy=2.576 ,statOfNoise="blperiod",smooth=TRUE,proportionOfMax=0.005,order=2,peakChoice="firstAmongHigh")
