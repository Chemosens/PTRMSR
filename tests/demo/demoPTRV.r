remove.packages("chemosensR")
library(devtools)
library(MSnbase)
library(reshape2)
library(pheatmap)
library(ggplot2)
library(chemosensR)
library(ellipse)
library(plotly)

#install_github("https://github.com/ChemoSens/ChemoSensPrivate/ChemosensR",auth_token="d92a1193900767184efad9d26606bb1db9ad6ba9")
wd="C:/INRA/Data/Données Cantin/Cantin-DTS-PTRviewer" 
setwd(wd)
listFiles=list.files(pattern="*.txt")[-1]
metaData2=read.table("metaData2.csv",sep=";",header=T)
head(metaData2[,-c(2:3)])

setwd(wd)
referenceBreath="m69.06906..69.06906...Conc."
sigIons=ptrvListSignificantSNRIons(listFiles=listFiles, referenceBreath =referenceBreath,noisePeriod=c(0,25))
ionSigUnique=sigIons$union

setwd(wd)
res_auc=ptrvListIntensityByTime(listFiles=listFiles,metaData=metaData2,ions=ionSigUnique)

res_auc$listRes
write.table(file="auc.csv",sep=";",res_auc$listRes,row.names=F)
res_tmax=ptrvListIntensityByTime(listFiles=listFiles,metaData=metaData2,ions=ionSigUnique,stat="tmax")

r3=analysis(x=res_auc,type="Intensity heatmap",ionToRemove=referenceBreath)

# For one file
setwd(wd)
file=listFiles[1]
dataset=read.table(file=file,header=TRUE,sep="\t")
report=ptrvReport(dataset,
                  listIons=ionSigUnique[1:3],
                  referenceBreath=referenceBreath,
                  methodDetectStart="startPeakProportion",
                  noisePeriodIBT=c(0,30),noisePeriodSig=c(0,30),
                  noisePeriodDS=c(0,30),
                  proportionOfMax=0.3,halfWindowSize=12,maxPeaks=30)
names(report$gg)

plot(report$gg$p_breath$p_cyclelimits)
plot(report$gg$p_breath$p_smoothbreath)
plot(report$gg$p_curves$p_raw)
plot(report$gg$p_curves$p_cycle)
plot(report$gg$p_curves$p_breath_raw)
plot(report$gg$p_curves$p_breath_cycle)
ggplotly(report$gg$p_curves$p_breath_raw)