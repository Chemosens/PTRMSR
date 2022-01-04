# DEMO PTR
#==========
library(chemosensR)

# Loading data
#==========================
setwd("C:/INRA/Data/")
file="CSBQ056_1402_IA_1-51-1.h5" #Chocolat isabelle
ptr=ptrRead(file,sumSpectraOnly=FALSE)

# Displays the spectra for all ions
#--------------------------------------------
int_time=ptrIntensityByTime(ptr) # Somme de tous les ions
plot(int_time)

# Selection d'une masse
int_time=ptrIntensityByTime(ptr,mz=c(61.04,61.08)) # Somme de tous les ions
plot(int_time)
plot(int_time,spar=0.5,main="Intensity accross time")


int_time=ptrIntensityByTime(ptr,integrationTable="device")
plot(int_time,selec="Propanol")

head(int_time$df)

p=plot(int_time,legend=FALSE)

ggplotly(p)

int_mass=ptrIntensityByMass(ptr,integrationTable="device")

plot_fullspectra(int_mass$df[,"mb"],int_mass$df[,"intensity"])
plot(int_mass,type="bars")

hm=ptrHeatmap(ptr)
plot_ptrHeatmap(hm)
#----------------------------------------------------------------
# Integrate close masses together with the integration table
#--------------------------------------------------------------
p=plot_fullspectra(ptr$MassAxis,ptr$SumSpectrum)
res=plot_fullspectra(ptr$MassAxis,ptr$SumSpectrum,integrationTable)
res$p 
ggplotly(res$p)

spectra=ptrIntensityByMass(ptr,integrationTable)
# Catch only interesting ions


#-------------------------------
# Maximum en fonction des "ions"
#--------------------------------

spectraMax=sapply(spectra,max);names(spectraMax)=names(spectra)
dfMax=data.frame(Name=names(spectraMax),Max=spectraMax);
ggplot(data=dfMax)+geom_col(aes(y=Max,x=Name))+theme(axis.text.x = element_text(angle = 45,hjust=1))+ggtitle("Maximal")

spectraSum=sapply(spectra,sum);names(spectraMax)=names(spectra)
dfSum=data.frame(Name=names(spectraSum),Sum=spectraSum);
p=ggplotly(data=dfSum)+geom_col(aes(y=Sum,x=Name))+theme(axis.text.x = element_text(angle = 45,hjust=1))+ggtitle("Maximal")
ggplotly(p)

hist(dfSum[,"Sum"],breaks=100,main="Sum of counts by ion")
hist(log(dfMax[,"Max"]+1),breaks=20,main="Maximal peak log-distribution");abline(v=3,col="red")
hist(log(dfSum[,"Sum"]+1),breaks=20,main="Distribution of log of counts"); abline(v=10,col="red")
hist(dfSum[,"Sum"],breaks=10000,main="Sum of counts by ion");abline(v=exp(10)-1,col="red")

# Selection of interesting ions

spectraMax=sapply(spectra,function(x) return(max(x[1:50])));names(spectraMax)=names(spectra)
spectraMax2=sapply(spectra,function(x) return(max(x[51:300])));names(spectraMax)=names(spectra)
SignalSurBruit=spectraMax2/spectraMax
SignalSurBruit[is.infinite(SignalSurBruit)]=5
hist(SignalSurBruit)

max_ions=dfMax[log(dfMax[,"Max"])>3,"Name"]
sum_ions=dfSum[log(dfSum[,"Sum"]+1)>10,"Name"]

all_ions=names(spectra)
# ions_respiration=c("isoprene","acetone")
# ions_chocolat=c("acetic acid","Butanone")

selectedIons=sum_ions


#================
# Visualization
#================
label="acetic acid"
label="Butanone"
label="(18O) Acetic acid"

# Display the spectra of one ion
#-------------------
ion_df=data.frame(time=as.vector(TimingData$BufTimes),spectrum=spectra[[label]])
ggplot(data=ion_df, aes(x=time, y=spectrum)) +
  geom_line() +
  ggtitle(label)

# Tracer plusieurs ions sur le m?me graphique
#-------------------------------------------
df_long=get_ions_df(ions=selectedIons,spectra,TimingData)

plot_ions_df(df_long,legend=T,xmin=0,xmax=300)

# Correlations between ions
#---------------------------
wideTable=dcast(data=ions_df,time~ions_rep,function(x) mean(x,na.rm=T),value.var="intensite")

df_wide_all=get_ions_df(selectedIons,spectra,TimingData,type="wide")
summary(apply(df_wide_all,2,sum))
barplot(apply(df_wide_all,2,sum))

res=pheatmap(cor(df_wide_all[,-1]),cex=0.8,clustering_method="ward")
res=pheatmap(abs(cor(df_wide_all[,-1])),cex=0.8,clustering_method="ward")

plot(res$tree_row)
plot(1:length(res$tree_row$height),res$tree_row$height)
rescut=cutree(res$tree_row,k=3)
gp1=names(rescut[rescut==1])
gp2=names(rescut[rescut==2])
gp3=names(rescut[rescut==3])
gp4=names(rescut[rescut==4])# aroma 1
gp5=names(rescut[rescut==5])  #reactive ions
gp6=names(rescut[rescut==6])
gp7=names(rescut[rescut==7])

df_long=get_ions_df(ions=gp1,spectra,TimingData,type="long",spar=0.5)
plot_ions_df(df_long,legend=TRUE,xmin=0,xmax=300)

# Using MALDIquant

s <- createMassSpectrum(mass=as.numeric(FullSpectra$MassAxis), intensity=as.numeric(FullSpectra$SumSpectrum),
                        metaData=list(name="example"))
massPeak=detectPeaks(s)
binPeaks(massPeak,tolerance=0.002)
plot(massPeak)
labelPeak=labelPeaks(massPeak, index=1:5)
plot(labelPeak)
estimateNoise(s)
