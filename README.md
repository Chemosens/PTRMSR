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
