library(openxlsx)
library(rhdf5)
setwd("C:/Users/capeltier/Nextcloud/MyDrive/DataAnalysis/2022_PTR Comparison")
# Integration table
integrationTable = read.xlsx("table-ions-Zoe-selection.xlsx")
integrationTable=integrationTable[,1:4]
colnames(integrationTable)=c("name","inf","mz","sup")
usualResults0=read.table(file="CSBQ056_1402_IA_1-51-1_export.txt",sep="\t",header=T)
concentrationDataPTRV=h5read(file="h5files/CSBQ056_1402_IA_1-51-1.h5",name="PTR-Concentration")$TwData #126.4333
# Recovering concentration with ptrViewer parameters
calculatedConcentration=getConcentration(h5file="h5files/CSBQ056_1402_IA_1-51-1.h5",nTimePoints=2650,processed=TRUE,processedTransmission=NULL,processedRawData=NULL,primaryMultipliers=NULL,integrationTable=integrationTable)

#concentrationInfo=h5read(file="h5files/CSBQ056_1402_IA_1-51-1.h5",name="PTR-Concentration")$TwInfo
h5file="h5files/CSBQ056_1402_IA_1-51-1.h5"
processedRawInfo=h5read(file=h5file,name="PROCESSED")$TraceData$RawInfo
colnames(usualResults0)
rownames(calculatedConcentration)
#"21.0218"   "45.03321"  "71.08494"  "87.08048"  "115.11033"
round(cor(calculatedConcentration["21.0218",],usualResults0[, "m21.02178..21.0218...Conc."]  ),digits=5)==1
round(cor(calculatedConcentration[ "45.0332",],usualResults0[, "m45.03321..45.03321...Conc." ])  ,digits=5)==1
round(cor(calculatedConcentration["71.0849" ,],usualResults0[,  "m71.08494..71.08494...Conc."] ) ,digits=5)==1
round(cor(calculatedConcentration["87.0805",],usualResults0[, "m87.08048..87.08048...Conc." ])  ,digits=5)==1
round(cor(calculatedConcentration["115.1103",],usualResults0[, "m115.11033..115.11033...Conc."]  ),digits=5)==1

processedRawData=ptrRead("h5files/CSBQ056_1402_IA_1-51-1.h5",integrationTable=integrationTable)

ptr=ptrRead(file="h5files/CSBQ056_1402_IA_1-51-1.h5")

integrationTable=integrationTable[,1:4]
integrationTable[,"name"]=paste0("m",round(as.numeric(integrationTable[,"mz"]),digits=3))
processedRawData_ptrmsr=ptrIntensityByTime(ptr,integrationTable=integrationTable)
df_ptrmsr=processedRawData_ptrmsr$df
#df_ptrmsr[,"name"]=
#unique(df_ptrmsr[,"name"])
wide_ptr=reshape(df_ptrmsr[,c("time","intensity","name")],idvar="time",direction="wide",timevar="name")
colnames(wide_ptr)[-1]=substr(colnames(wide_ptr[,-1]),11,nchar(colnames(wide_ptr[,-1])))

primaryMultipliers=c(488,244)
names(primaryMultipliers)=c("m21.022","m39.032")

processedTransmission=matrix(NA,2,8)
processedTransmission[1,]=c(21,79,93,107,113,121,147,181)
processedTransmission[2,]=c(0.07,0.6,0.64,0.75,0.76,0.84,0.91,1)



calculatedConcentration2=getConcentration(h5file,processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=processedTransmission,primaryMultipliers=primaryMultipliers,
                                          integrationTable=integrationTable)



calculatedConcentration2[,1:2]

calculatedConcentration=getConcentration(h5file="h5files/CSBQ056_1402_IA_1-51-1.h5",nTimePoints=2650,processed=TRUE,processedTransmission=NULL,processedRawData=NULL,primaryMultipliers=NULL,integrationTable=integrationTable)

cor(calculatedConcentration2["m21.022",],usualResults0[, "m21.02178..21.0218...Conc."]  )
plot(calculatedConcentration2["m45.033",],usualResults0[, "m45.03321..45.03321...Conc."]  )
plot(calculatedConcentration2["m71.048",],usualResults0[, "m71.08494..71.08494...Conc."]  )
plot(calculatedConcentration2["m87.06",],usualResults0[, "m87.08048..87.08048...Conc."]  )
plot(calculatedConcentration2["m87.06",],usualResults0[, "m115.11033..115.11033...Conc."]  )

# Comparison with Isabelle (Alix data)
library(openxlsx)
library(ggplot2)
library(PTRMSR)
library(plotly)

repoData="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/allData"
workingRepo="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/Analyse_PERSISTANCE_févriermars_2024_1/"

repoTxt="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/Analyse_PERSISTANCE_févriermars_2024_1/fichiersTXT"
integrationTable=read.xlsx("P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/integrationTable_new.xlsx")

#transmission=read.table("2024-transmission2018txt.txt",sep=",",header=F)
resultatPTRV=read.xlsx("P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/CSDA73-S01-123_1_export.xlsx")


# Vérification Isabelle
#=========================
# loading ptr

# ptr0=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-123_1.h5"),sumSpectraOnly=TRUE)
repoData="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/allData"
workingRepo="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/Analyse_PERSISTANCE_févriermars_2024_1/"

repoTxt="P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/Analyse_PERSISTANCE_févriermars_2024_1/fichiersTXT"
integrationTable=read.xlsx("P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/integrationTable_new.xlsx")

#transmission=read.table("2024-transmission2018txt.txt",sep=",",header=F)
resultatPTRV=read.xlsx("P:/PFLA/P00199-PODCAST/PERSISTANCE fevrierMARS 2024/CSDA73-S01-123_1_export.xlsx")

setwd(repoTxt)
ptr1=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),sumSpectraOnly=FALSE)
# ptr2=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-092_1.h5"),sumSpectraOnly=FALSE)
# ptr3=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-123_1.h5"),sumSpectraOnly=FALSE)
# ptr4=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-158_1.h5"),sumSpectraOnly=FALSE)
# ptr5=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-301_2.h5"),sumSpectraOnly=FALSE)
# ptr6=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-638_1.h5"),sumSpectraOnly=FALSE)
# ptr7=ptrRead(file=paste0(workingRepo,"/CSDA83_B522-673_1.h5"),sumSpectraOnly=FALSE)
h5=(workingRepo,"/CSDA83_B522-063_1.h5")

# loading txt
txt1=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-063_1.h5_export.txt"),sep="\t",header=T)
# txt2=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-092_1.h5_export.txt"),sep="\t",header=T)
# txt3=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-123_1.h5_export.txt"),sep="\t",header=T)
# txt4=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-158_1.h5_export.txt"),sep="\t",header=T)
# txt5=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-301_2.h5_export.txt"),sep="\t",header=T)
# txt6=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-638_1.h5_export.txt"),sep="\t",header=T)
# txt7=file=read.table(file=paste0(repoTxt,"/CSDA83_B522-673_1.h5_export.txt"),sep="\t",header=T)

# getting intensity with integration table

# ptribt2=ptrIntensityByTime(ptr2,rt=NULL,mz=NULL,integrationTable=integrationTable)
# ptribt3=ptrIntensityByTime(ptr3,rt=NULL,mz=NULL,integrationTable=integrationTable)
# ptribt4=ptrIntensityByTime(ptr4,rt=NULL,mz=NULL,integrationTable=integrationTable)
# ptribt5=ptrIntensityByTime(ptr5,rt=NULL,mz=NULL,integrationTable=integrationTable)
# ptribt6=ptrIntensityByTime(ptr6,rt=NULL,mz=NULL,integrationTable=integrationTable)
# ptribt7=ptrIntensityByTime(ptr7,rt=NULL,mz=NULL,integrationTable=integrationTable)

# comparing
# if(numFile==2){ptribt=ptribt2;txt=txt2}
# if(numFile==3){ptribt=ptribt3;txt=txt3}
# if(numFile==4){ptribt=ptribt4;txt=txt4}
# if(numFile==5){ptribt=ptribt5;txt=txt5}
# if(numFile==6){ptribt=ptribt6;txt=txt6}
# if(numFile==7){ptribt=ptribt7;txt=txt7}

#
# ion="acetateIsoamyl"
# ion="isoprene"
# ion="benzaldehide"
#listIons=c("acetateIsoamyl","isoprene","benzaldehide")

integrationTable=integrationTable[,1:4]
integrationTable[8,]=c("H3O+",21.022,21.022-0.1,21.022+0.1)
integrationTable[9,]=c("H3O+(H2O)",39.032,39.032-0.1,39.032+0.1)

numFile=1
ptribt1=ptrIntensityByTime(ptr1,rt=NULL,mz=NULL,integrationTable=integrationTable)
if(numFile==1){ptribt=ptribt1;txt=txt1}

processedRawData_ptrmsr=ptribt1
df_ptrmsr=processedRawData_ptrmsr$df

listIons=c("isoamylAcetate","isoprene","Benzal")
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

  # intensities3=getConcentration(h5file,processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
  #                                                        processedTransmission=processedTransmission,primaryMultipliers=primaryMultipliers,
  #                                                        integrationTable=integrationTable)

  txtDf=data.frame(time=txt[,"RelTime"],ion=txt[,numCol])
  p1[[ion]]=ggplot(data=intDf,aes(x=time,y=intensity))+geom_line()+ggtitle(paste0("h5:",ion))
  p2[[ion]]=ggplot(data=txtDf,aes(x=time,y=ion))+geom_line()+ggtitle(paste0("PTRViewer:",ion))
  p3[[ion]]=ggplot(data=data.frame(h5=intDf[,"intensity"],ptrv=txt[,numCol]),aes(x=h5,y=ptrv))+geom_point()+ggtitle("Correlations")

}
grid.arrange(p1[[listIons[[1]]]],p2[[listIons[[1]]]],p3[[listIons[[1]]]],
             p1[[listIons[[2]]]],p2[[listIons[[2]]]],p3[[listIons[[2]]]],
             p1[[listIons[[3]]]],p2[[listIons[[3]]]],p3[[listIons[[3]]]]
)

# Correcting the intensities


#df_ptrmsr[,"name"]=
#unique(df_ptrmsr[,"name"])
wide_ptr=reshape(df_ptrmsr[,c("time","intensity","name")],idvar="time",direction="wide",timevar="name")
colnames(wide_ptr)[-1]=substr(colnames(wide_ptr[,-1]),11,nchar(colnames(wide_ptr[,-1])))

primaryMultipliers=c(488,244)
names(primaryMultipliers)=c("H3O+","H3O+(H2O)")

primaryMultipliers2=c(488)
names(primaryMultipliers2)=c("H3O+")

processedTransmission=matrix(NA,2,8)
processedTransmission[1,]=c(21,79,93,107,113,121,147,181)
processedTransmission[2,]=c(0.07,0.6,0.64,0.75,0.76,0.84,0.91,1)

paste0(workingRepo,"/CSDA83_B522-063_1.h5")
library(rhdf5)
calculatedConcentration2=getConcentration(h5file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=processedTransmission,primaryMultipliers=primaryMultipliers,
                                          integrationTable=integrationTable)

calculatedConcentration3=getConcentration(h5file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=processedTransmission,primaryMultipliers=primaryMultipliers2,
                                          integrationTable=integrationTable)

calculatedConcentration4=getConcentration(h5file=paste0(workingRepo,"/CSDA83_B522-063_1.h5"),processedRawData=t(wide_ptr[,-1]),nTimePoints=NULL,processed=FALSE,
                                          processedTransmission=processedTransmission,primaryMultipliers=NULL,
                                          integrationTable=integrationTable)

plot(calculatedConcentration2["Benzal",],calculatedConcentration4["Benzal",])
plot(calculatedConcentration2["Benzal",],txt1[,"m107.049..Benzaldehyde...Conc."])
plot(calculatedConcentration3["Benzal",],txt1[,"m107.049..Benzaldehyde...Conc."])

#plot(calculatedConcentration4["Benzal",],txt1[,"m107.049..Benzaldehyde...Conc."])

plot(calculatedConcentration2["Benzal",],txt1[,"m107.049..Benzaldehyde...Conc."])
plot(calculatedConcentration2["Terpene",],txt1[,"m137.132..Terpene...Conc."])
plot(calculatedConcentration2["isoamylAcetate",],txt1[,"m131.107..isoamyl.acetate...Conc."])
plot(calculatedConcentration2["isoprene",],txt1[,"m69.070..isoprene...Conc."])
plot(calculatedConcentration2["H3O+(H2O)",],txt1[,"m39.034..H2O.18OH3._Peak1...Conc."])
plot(calculatedConcentration2["H3O+",],txt1[,"m21.022..H3O.18...Conc."])
h5ls(paste0(workingRepo,"/CSDA83_B522-063_1.h5"))
transmision=h5read(paste0(workingRepo,"/CSDA83_B522-063_1.h5"),name="PTR-Transmission")
primaryIonSettings=h5read(paste0(workingRepo,"/CSDA83_B522-063_1.h5"),name="/PTR-PrimaryIonSettings")
