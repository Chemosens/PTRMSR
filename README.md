# PTRMSR
PTRMSR package


## Installation
The installation of PTRMSR requires the installation of other packages
```R
if (!require("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
if (!require("devtools", quietly = TRUE)) {install.packages("devtools")}
BiocManager::install("rhdf5")
BiocManager::install("MSnbase")
install.packages(c("reshape2","pheatmap","ggplot2","ellipse","plotly","FactoMineR"))
devtools::install_github("https://github.com/ChemoSens/CSUtils")
devtools::install_github("https://github.com/ChemoSens/PTRMSR")
```
## Getting all intensities for a given integrationTable, a metadataTable and a repository containing .h5 files
This function returns a dataframe containing the intensities of the ions described in integrationTable (a file whose colnames are "name" "mz" "inf" and "sup"), for files that are in a repository.
### Loading libraries
```R
rm(list=ls())
library(ggplot2)
library(PTRMSR)
library(plotly)
library(openxlsx)
```
### Parameters to be modified 
```R
duration=0.5000145 # select the time period between two scans (in seconds)
nameFile=paste0("your_ptr_excel")
repoData=paste0("P:/Chemosens/Projets/Projets_En_Cours/1-Projets-Aromes/P00207_AlinoVeg/Donnees/CAROLINE") # To be modified : repoData should contain: a file metaData.xlsx 
saveRepo=repoData # To be modified
```
### Loading and checking integrationTable
```R
md=F;it=F
if("integrationTable.csv"%in% list.files()){it=T;integrationTable=read.csv("integrationTable.csv",sep=",")}
integrationTable

```
### Code to run without any change to obtain a dataframe with intensities values
```R
paramList=c("date","repo","repoData","saveRepo","duration")
date=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
paramValues=c(date,"",repoData,saveRepo,duration)
df_param=data.frame(param=paramList,value=paramValues)
setwd(repoData)
filesIA=list.files(pattern="*.h5")
t0=Sys.time()
ptrRes=ptrReadListIBT(filesIA,sumSpectraOnly=FALSE,mz=NULL,rt=NULL,integrationTable=integrationTable)
t1=Sys.time()
colnames(ptrRes)[4]="ion"
ptrRes[,"duration"]=duration
```

### Saving excel in save repo
```R
setwd(saveRepo)
wb <- createWorkbook()
addWorksheet(wb, "PTR") # Feuille de données

#addWorksheet(wb, "metaData")
addWorksheet(wb, "param")
writeData(wb, "PTR", ptrRes)
if(it){addWorksheet(wb, "integrationTable") ;writeData(wb, "integrationTable", integrationTable)}
writeData(wb, "param", df_param)
saveWorkbook(wb, file=paste0(nameFile,".xlsx"), overwrite = TRUE)
```

### First plot for observing kinetics of an ion 
The following command shows how to plot the kinetics of one specific ion on different files
```R
ion=unique(ptrRes[,"ion"][1])
p=ggplot(ptrRes[ptrRes[,"ion"]==ion,],aes(x=time,y=intensity,col=file))+geom_line()
ggplotly(p) # for interactive plot
```

## Getting the list of some statistics by file 
The area, maximal intensity (imax), time of maximal intensity(tmax) and the sum of signal (sum) can be computed by file.
```R
res=ptrvListIntensity(ptrRes,format="wide",stat="area", timePeriod = NULL, negativeAsNull = TRUE,fill=NULL)
``

## Getting the whole code (simply copy/paste it)
```R
# Installation
if (!require("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
if (!require("devtools", quietly = TRUE)) {install.packages("devtools")}
BiocManager::install("rhdf5")
BiocManager::install("MSnbase")
install.packages(c("reshape2","pheatmap","ggplot2","ellipse","plotly","FactoMineR"))
devtools::install_github("https://github.com/ChemoSens/CSUtils") # install remote: yes ! !
devtools::install_github("https://github.com/ChemoSens/PTRMSR")

# Getting user parameters for extracting info from .h5
repoData=paste0("P:/Chemosens/Projets/Projets_En_Cours/1-Projets-Aromes/P00207_AlinoVeg/Donnees/CAROLINE") # To be modified : repoData should contain: a file metaData.xlsx
setwd(repoData)
integrationTable=read.csv("integrationTable.csv",sep=",") # To be modified according to the name of integrationTable
summary(integrationTable)
duration=0.5000145 # select the time period between two scans (in seconds)
nameFile=paste0("preprocessedData")
saveRepo=repoData # To be modified
paramList=c("date","repo","repoData","saveRepo","duration")
date=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
paramValues=c(date,"",repoData,saveRepo,duration)
df_param=data.frame(param=paramList,value=paramValues)

# Running code (could be long)
setwd(repoData)
filesIA=list.files(pattern="*.h5")
t0=Sys.time()
ptrRes=ptrReadListIBT(filesIA,sumSpectraOnly=FALSE,mz=NULL,rt=NULL,integrationTable=integrationTable)
t1=Sys.time()
colnames(ptrRes)[4]="ion"
ptrRes[,"duration"]=duration

# Saving as excel
setwd(saveRepo)
wb <- createWorkbook()
addWorksheet(wb, "PTR") # Feuille de données
addWorksheet(wb, "param")
writeData(wb, "PTR", ptrRes)
if(it){addWorksheet(wb, "integrationTable") ;writeData(wb, "integrationTable", integrationTable)}
writeData(wb, "param", df_param)
saveWorkbook(wb, file=paste0(nameFile,".xlsx"), overwrite = TRUE)

# Reading the obtained file (after computation)
setwd(repoData)
ptrRes= read.xlsx(paste0(nameFile,".xlsx"))
# Getting some plots
ion=unique(ptrRes[,"ion"][1])
p=ggplot(ptrRes[ptrRes[,"ion"]==ion,],aes(x=time,y=intensity,col=file))+geom_line()
ggplotly(p) # for interactive plot

# Getting some statistics
res_auc=ptrvListIntensity(ptrRes,format="wide",stat="area", timePeriod = NULL, negativeAsNull = TRUE,fill=NULL)
res_max=ptrvListIntensity(ptrRes,format="wide",stat="max", timePeriod = NULL, negativeAsNull = TRUE,fill=NULL)
res_tmax=ptrvListIntensity(ptrRes,format="wide",stat="tmax", timePeriod = NULL, negativeAsNull = TRUE,fill=NULL)

# Writing the statistics in excel
wb <- createWorkbook()
addWorksheet(wb, "auc") # Feuille de données
writeData(wb, "auc", res_auc)
saveWorkbook(wb, file=paste0("auc.xlsx"), overwrite = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "auc") # Feuille de données
writeData(wb, "auc", res_auc)
saveWorkbook(wb, file=paste0("auc.xlsx"), overwrite = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "max") # Feuille de données
writeData(wb, "max", res_max)
saveWorkbook(wb, file=paste0("max.xlsx"), overwrite = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "tmax") # Feuille de données
writeData(wb, "tmax", res_tmax)
saveWorkbook(wb, file=paste0("tmax.xlsx"), overwrite = TRUE)



```


