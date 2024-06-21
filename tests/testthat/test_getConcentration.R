library(openxlsx)
library(rhdf5)
library(ggplot2)
library(PTRMSR)
library(plotly)
#===============================================================================
# First checkings consists in validating getConcentration from ptrmsViewer$raw to ptrmsViewer$corrected
#==================================================================================================

# First tests on Zoe datafile
#=============================
setwd("C:/Users/capeltier/Nextcloud/MyDrive/DataAnalysis/2022_PTR Comparison")
# Integration table
integrationTable = read.xlsx("table-ions-Zoe-selection.xlsx")
integrationTable=integrationTable[,1:4]
colnames(integrationTable)=c("name","inf","mz","sup")
integrationTable=rbind(integrationTable,c(69.06922,as.numeric(processedPeakData[2:4,14])))

integrationTable[,"name"]=as.numeric(substr(integrationTable[,"name"],1,8))
integrationTable[,"name"]=round(integrationTable[,"name"],digits=4)

usualResults0=read.table(file="CSBQ056_1402_IA_1-51-1_export.txt",sep="\t",header=T)
concentrationDataPTRV=h5read(file="h5files/CSBQ056_1402_IA_1-51-1.h5",name="PTR-Concentration")$TwData #126.4333

# Recovering concentration with ptrViewer parameters (processed=TRUE parameter)
calculatedConcentration=getConcentration(h5file="h5files/CSBQ056_1402_IA_1-51-1.h5",nTimePoints=2650,processed=TRUE,processedTransmission=NULL,processedRawData=NULL,primaryMultipliers=NULL,integrationTable=integrationTable)

#concentrationInfo=h5read(file="h5files/CSBQ056_1402_IA_1-51-1.h5",name="PTR-Concentration")$TwInfo
h5file="h5files/CSBQ056_1402_IA_1-51-1.h5"
processedRawInfo=h5read(file=h5file,name="PROCESSED")$TraceData$RawInfo
processedPeakData=h5read(file=h5file,name="PROCESSED")$TraceData$PeakTableData
processedPeakInfo=h5read(file=h5file,name="PROCESSED")$TraceData$PeakTableInfo
(t(processedPeakData[c(2,3,4),]))
colnames(usualResults0)
rownames(calculatedConcentration)
#"21.0218"   "45.03321"  "71.08494"  "87.08048"  "115.11033"
round(cor(calculatedConcentration["21.0218",],usualResults0[, "m21.02178..21.0218...Conc."]  ),digits=5)==1
round(cor(calculatedConcentration[ "45.0332",],usualResults0[, "m45.03321..45.03321...Conc." ])  ,digits=5)==1
round(cor(calculatedConcentration["69.0692" ,],usualResults0[,  "m69.06922..69.06922...Conc."] ) ,digits=5)==1
plot(calculatedConcentration["69.0692" ,],usualResults0[,  "m69.06922..69.06922...Conc."] )
# Ok, it worked for unconvoluted peaks, but not for convoluted peaks

# Same work but on raw data without using processed dataset (processed = FALSE)
#========================================================
processedRawData=ptrRead("h5files/CSBQ056_1402_IA_1-51-1.h5",integrationTable=integrationTable)
ptr=ptrRead(file="h5files/CSBQ056_1402_IA_1-51-1.h5")
integrationTable=integrationTable[,1:4]
integrationTable[,"name"]=paste0("m",round(as.numeric(integrationTable[,"mz"]),digits=3))
processedRawData_ptrmsr=ptrIntensityByTime(ptr,integrationTable=integrationTable)
df_ptrmsr=processedRawData_ptrmsr$df
# putting the long table into wide table
wide_ptr=reshape(df_ptrmsr[,c("time","intensity","name")],idvar="time",direction="wide",timevar="name")
colnames(wide_ptr)[-1]=substr(colnames(wide_ptr[,-1]),11,nchar(colnames(wide_ptr[,-1])))
# Getting the primary multipliers
primaryMultipliers=c(488,244)
names(primaryMultipliers)=c("m21.022","m39.032")
# Getting the transmission
processedTransmission=matrix(NA,2,8)
processedTransmission[1,]=c(21,79,93,107,113,121,147,181)
processedTransmission[2,]=c(0.07,0.6,0.64,0.75,0.76,0.84,0.91,1)

# Running getConcentration (processed option)
calculatedConcentration2=getConcentration(h5file,processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=processedTransmission,primaryMultipliers=primaryMultipliers,
                                          integrationTable=integrationTable)

# Running getConcentration (not processed option)
calculatedConcentration=getConcentration(h5file="h5files/CSBQ056_1402_IA_1-51-1.h5",nTimePoints=2650,processed=TRUE,processedTransmission=NULL,processedRawData=NULL,primaryMultipliers=NULL,integrationTable=integrationTable)

# Results
cor(calculatedConcentration2["m21.022",],usualResults0[, "m21.02178..21.0218...Conc."]  )
plot(calculatedConcentration2["m21.022",],usualResults0[, "m21.02178..21.0218...Conc."]  )

cor(calculatedConcentration2["m45.033",],usualResults0[, "m45.03321..45.03321...Conc."]  )
plot(calculatedConcentration2["m45.033",],usualResults0[, "m45.03321..45.03321...Conc."]  )

cor(calculatedConcentration2["m69.069",],usualResults0[, "m69.06922..69.06922...Conc."]  )
plot(calculatedConcentration2["m69.069",],usualResults0[, "m69.06922..69.06922...Conc."]  )
# Ok correlation very close to 1

#==================================================
# Checks on Alix data
#=================================================
repoData="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/allData"
workingRepo="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/Analyse_PERSISTANCE_févriermars_2024_1/"
repoTxt="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/Analyse_PERSISTANCE_févriermars_2024_1/fichiersTXT"
integrationTable=read.xlsx("P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/integrationTable_new.xlsx")

setwd(repoTxt)
ptr1=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),sumSpectraOnly=FALSE)
h5=paste0(workingRepo,"/CSDA83_B522-063_1.h5")

# loading txt
txt1=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-063_1.h5_export.txt"),sep="\t",header=T)
integrationTable=integrationTable[,1:4]
integrationTable[8,]=c("H3O+",21.022,21.022-0.1,21.022+0.1)
integrationTable[9,]=c("H3O+(H2O)",39.032,39.032-0.1,39.032+0.1)

# Getting intensity by time
numFile=1
ptribt1=ptrIntensityByTime(ptr1,rt=NULL,mz=NULL,integrationTable=integrationTable)
if(numFile==1){ptribt=ptribt1;txt=txt1}
processedRawData_ptrmsr=ptribt1
df_ptrmsr=processedRawData_ptrmsr$df

# Getting intensity by time (not concentrations for plots)
listIons=c("isoamylAcetate","isoprene","Terpene")
p1=p2=p3=list()
for(ion in listIons)
{
  print(ion)
  intensities=reshape(ptribt$df[,c("time","intensity","name")],direction="wide",timevar="name",idvar="time",v.names=NULL)
  ions=colnames(intensities)[-1]=substr(colnames(intensities)[-1],11,nchar(colnames(intensities)[-1]))
  intensities[,"AbsTime"]=intensities[,1]
  colnames(intensities)[1]="RelTime"
  intensities[,"Cycle"]=1
  intensities2=intensities[,c("AbsTime","RelTime","Cycle",ions)]
  if(ion=="isoamylAcetate")
  {numCol=12;intDf=data.frame(time=intensities2[,"RelTime"],intensity=intensities2[,"isoamylAcetate"])}
  if(ion=="isoprene"){numCol=8;intDf=data.frame(time=intensities2[,"RelTime"],intensity=intensities2[,"isoprene"])}
  if(ion=="benzaldehide"||ion=="Benzal"){numCol=9;intDf=data.frame(time=intensities2[,"RelTime"],intensity=intensities2[,"Benzal"])}
  if(ion=="Terpene"){numCol=13;intDf=data.frame(time=intensities2[,"RelTime"],intensity=intensities2[,"Terpene"])}

  txtDf=data.frame(time=txt[,"RelTime"],ion=txt[,numCol])
  p1[[ion]]=ggplot(data=intDf,aes(x=time,y=intensity))+geom_line()+ggtitle(paste0("h5:",ion))
  p2[[ion]]=ggplot(data=txtDf,aes(x=time,y=ion))+geom_line()+ggtitle(paste0("PTRViewer:",ion))
  p3[[ion]]=ggplot(data=data.frame(h5=intDf[,"intensity"],ptrv=txt[,numCol]),aes(x=h5,y=ptrv))+geom_point()+ggtitle("Correlations")
}

grid.arrange(p1[[listIons[[1]]]],p2[[listIons[[1]]]],p3[[listIons[[1]]]],
             p1[[listIons[[2]]]],p2[[listIons[[2]]]],p3[[listIons[[2]]]],
             p1[[listIons[[3]]]],p2[[listIons[[3]]]],p3[[listIons[[3]]]]
)

# Which parameters were used in ptrv ?
# Getting back the modified .h5 with PTRViewer to compare the parameters used in processed vd raw
#=====================================================================================
h5ls(paste0(workingRepo,"/CSDA83_B522-063_1.h5"))
h5ls("P:/PFLA/Caroline_Peltier/CSDA83_B522-063_1.h5")

primaryIonSettings=h5read(paste0(workingRepo,"/CSDA83_B522-063_1.h5"),name="/PTR-PrimaryIonSettings")
processedPrimaryIons_processed=h5read(file="P:/PFLA/Caroline_Peltier/CSDA83_B522-063_1.h5",name="PROCESSED")$PrimaryIonSettings
primaryIonSettings # 500  #1 multplier
processedPrimaryIons #250 # 2 multipliers

transmission=h5read(paste0(workingRepo,"/CSDA83_B522-063_1.h5"),name="PTR-Transmission")
processedTransmission_processed=h5read(file="P:/PFLA/Caroline_Peltier/CSDA83_B522-063_1.h5",name="PROCESSED")$Transmission
processedTransmission_processed$TransmissionData[,,1][1:9,]
transmission

traceData_processed=h5read(file="P:/PFLA/Caroline_Peltier/CSDA83_B522-063_1.h5",name="PROCESSED")$TraceData
names(traceData)
peakTable_processed=traceData$PeakTableData
integrationTable[,1:4]

integrationTable[1,"inf"]=69.0267
integrationTable[1,"sup"]=69.12270

integrationTable[8,"inf"]=21.00746
integrationTable[8,"sup"]=21.03474

integrationTable[9,"inf"]=39.01393
integrationTable[9,"sup"]=39.05359

integrationTable
# Correcting the intensities for obtaining concentrations (from counts)
#===========================================================================

# Issue: one of the primary ion is deconvoluted ! ! !
wide_ptr=reshape(df_ptrmsr[,c("time","intensity","name")],idvar="time",direction="wide",timevar="name")
colnames(wide_ptr)[-1]=substr(colnames(wide_ptr[,-1]),11,nchar(colnames(wide_ptr[,-1])))

# Getting primaryMultipliers
#primaryMultipliers=c(488,244)
primaryMultipliers2=c(500,250)
names(primaryMultipliers2)=c("H3O+","H3O+(H2O)")

primaryMultipliers=c(500)
names(primaryMultipliers)=c("H3O+")

# processedTransmission=matrix(NA,2,8)
# processedTransmission[1,]=c(21,79,93,107,113,121,147,181)
# processedTransmission[2,]=c(0.07,0.6,0.64,0.75,0.76,0.84,0.91,1)

# transmissionTable=matrix(NA,2,8)
# transmissionTable[1,]=c(21.02,42.03,59.04, 79.05, 93.06,107.08, 113.01,146.97)
# transmissionTable[2,]=c(0.09, 0.31,0.58,0.75, 0.88, 1, 0.98, 0.75)

transmissionTable=matrix(NA,2,8)
transmissionTable[1,]=c(21.02199936, 42.0340004, 59.0489998, 93.0699997, 107.086, 121.0650024 ,146.9770050, 180.9380035)
transmissionTable[2,]=c(0.09294234,  0.1377761,  0.6267083,  0.8630147  , 1.000 ,  0.9328507,   0.5873325 ,  0.4663464 )

paste0(workingRepo,"/CSDA83_B522-063_1.h5")
calculatedConcentration_mult1=getConcentration(h5file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=transmissionTable,primaryMultipliers=primaryMultipliers,
                                          integrationTable=integrationTable)

calculatedConcentration_mult2=getConcentration(h5file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=transmissionTable,primaryMultipliers=primaryMultipliers2,
                                          integrationTable=integrationTable)

calculatedConcentration_mult0=getConcentration(h5file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=transmissionTable,primaryMultipliers=NULL,
                                          integrationTable=integrationTable)

plot(calculatedConcentration_mult1["isoprene",],txt1[,"m69.070..isoprene...Conc."],xlab="calculated",ylab="ptrviewer",main="1 multiplier")
plot(calculatedConcentration_mult2["isoprene",],txt1[,"m69.070..isoprene...Conc."],xlab="calculated",ylab="ptrviewer",main="2 multipliers")
abline(b=1,a=0)
plot(calculatedConcentration_mult0["isoprene",],txt1[,"m69.070..isoprene...Conc."],xlab="calculated",ylab="ptrviewer",main="no multipliers")
abline(b=1,a=0)


plot(calculatedConcentration_mult2["Terpene",],txt1[,"m137.132..Terpene...Conc."])
plot(calculatedConcentration_mult1["isoamylAcetate",],txt1[,"m131.107..isoamyl.acetate...Conc."])
plot(calculatedConcentration_mult1["isoprene",],txt1[,"m69.070..isoprene...Conc."])
plot(calculatedConcentration_mult1["H3O+(H2O)",],txt1[,"m39.034..H2O.18OH3._Peak1...Conc."])
plot(calculatedConcentration_mult1["H3O+",],txt1[,"m21.022..H3O.18...Conc."])

