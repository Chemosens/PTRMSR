# DEMO PTR
#==========
library(plotly)
library(rhdf5)
library(chemosensR)

#====================
# Loading data from one file
#==========================
setwd("C:/INRA/Data/")

file="CSBQ056_1402_IA_1-51-1.h5" #Chocolat isabelle

ptr=ptrRead(file,sumSpectraOnly=FALSE)

# 1. With an integration table
#===============================
# A. Intensity by time for each ion of interest
int_time_IT=ptrIntensityByTime(ptr,integrationTable="device")
head(int_time_IT$df)
dim(int_time_IT$df)

# B. Visualizing
#    i. Raw data
p=plot(int_time_IT,main="Raw data",groupOption="name",legend=FALSE)
ggplotly(p)

#    ii. Smooth data
p2=plot(int_time_IT,spar=0.5,groupOption="name",main="Intensity accross time",legend=FALSE)
ggplotly(p2)

#    iii. Some ions, for specific interval
p3=plot(int_time_IT,selec="Propanol",xlim=c(40,200))
ggplotly(p3)

# Aire sous la courbe
int_mass_IM=ptrIntensityByMass(ptr,integrationTable="device")
head(int_mass_IM$df)


plot(int_mass_IM,type="bar")
plot(int_mass_IM,type="bar",subset=c("acetic acid","Butanone"))

# Selection of a time interval
int_mass_IMRT=ptrIntensityByMass(ptr,rt=c(100,200),integrationTable="device")
head(int_mass_IMRT$df)
head(int_mass_IM$df)
#plot(int_mass_IMRT,type="bar")
#---------------------------
# 4. Lecture des différents fichiers
#----------------------------------
# Spectre de masse "commun"
setwd("C:/INRA/Data/Analyse par sujet/1")
listFiles=list.files()
#listPTRM=ptrReadListIBM(listFiles,sumSpectraOnly=TRUE,normalization="none",rt=NULL,integrationTable=NULL,breaks=breaks,by=0.1)
head(listPTRM)
# p4=plot_fullspectra(listPTRM[,"mz"],listPTRM[,"intensity"],listPTRM[,"file"],legend=F)
# ggplotly(p4)
# TODO
#plot(listPTRM,groupOption="file")

# Visualisation par fichier et temps

listPTRT=ptrReadListIBT(listFiles,normalization="none",rt=NULL,integrationTable="device",breaks=NULL,by=0.1,mz=NULL)
dfbut=listPTRT[listPTRT[,"name"]=="Butanone",]
p=plot_fullspectra(dfbut[,"time"],dfbut[,"intensity"],dfbut[,"file"],legend=F)

wideTable=dcast(data=listPTRT,time+file~name,function(x) mean(x,na.rm=T),value.var="intensity")
head(wideTable[,1:10])

# Analysis from this data table (with other packages)
#---------------------------
library(reshape2)
library(pheatmap)
library(RGCCA)
wideTable=dcast(data=int_time_IT$df,time~name,function(x) mean(x,na.rm=T),value.var="intensity")
head(wideTable[,1:10])

timeSel=wideTable[,1]<250&wideTable[,1]>60
bloc=wideTable[timeSel,-1];rownames(bloc)=wideTable[timeSel,1]
res=rgcca(list(bloc),type="pca",ncomp=2)
plot(res,type="both",resp=as.numeric(rownames(bloc)),main="PCA")
p=plot(int_time_IT,selec=gp3,spar=0.5,main="PCA")
ggplotly(p)
plot(int_time_IT,selec=gp2)
plot(int_time_IT,selec=gp3)

# Correction of a respiration effect
#-----------------------------------


indBlank=wideTable[,1]<60
reslm=lm(wideTable[indBlank,"Butanone"]~wideTable[indBlank,"isoprene"])

plot(wideTable[indBlank,1],wideTable[indBlank,"isoprene"],type="l")

newButanone=wideTable[,"Butanone"]-(coef(reslm)[1]+coef(reslm)[2]*wideTable[,"isoprene"])

plot(wideTable[,1],wideTable[,"Butanone"],type="l",col="red")
lines(wideTable[,1],newButanone,type="l",col="blue")


smooth.spline(wideTable[indBlank,1],wideTable[indBlank,"isoprene"])
plot(wideTable[indBlank,1],wideTable[indBlank,"Butanone"],type="l",col="red",ylim=c(-10,30))
lines(wideTable[indBlank,1],reslm$residuals,type="l",col="dodgerblue")
lines(wideTable[indBlank,1],reslm$residuals,type="l",col="dodgerblue")



# 4. Visualizing raw data
#=============================
#  B. Intensity by time
#       i. For all the masses
int_time_raw=ptrIntensityByTime(ptr) # Somme de tous les ions
p=plot(int_time_raw)
ggplotly(p)
#      ii. For a given interval of masses
int_time_m61=ptrIntensityByTime(ptr,mz=c(61.04,61.08)) # Somme de tous les ions
p=plot(int_time_m61)
ggplotly(p)
#      iii. For given times

#  B. Intensity by mass
#      i. For all the times, all the masses
int_mass_raw=ptrIntensityByMass(ptr,by=0.1)
p1=plot(int_mass_raw)

#     ii. For a given interval of time
int_mass_res=ptrIntensityByMass(ptr,rt=c(60,70))
p1=plot(int_mass_res)





res=pheatmap(cor(wideTable[,-1]),cex=0.8,clustering_method="average")
plot(res$tree_row)
rescut=cutree(res$tree_row,k=3)
plot(rescut)
gp1=names(rescut[rescut==1]) # Arome
gp2=names(rescut[rescut==2]) #Breath
gp3=names(rescut[rescut==3]) #Alternated breath
