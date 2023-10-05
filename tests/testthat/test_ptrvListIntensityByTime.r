
#file1 = paste0(system.file(package = "PTRMSR"),"/extdata/CSCA098_S002__2.txt")
#file2 = paste0(system.file(package = "PTRMSR"),"/extdata/CSCA098_S002__2.txt")
wd=paste0(system.file(package = "PTRMSR"),"/extdata")
listFiles=c("file1.txt","file2.txt")

#wd="C:/INRA/PTRMSR/inst/extdata"
#setwd(wd)
# listFiles=list.files()
# f1=read.table(listFiles[1],header=T,sep="\t",check.names=F)
# f1=f1[,1:100]
# colnames(f1)=substr(colnames(f1),1,8)
# write.table(f1,"file1.txt",sep="\t",row.names=F)
#
#
# f2=read.table(listFiles[2],header=T,sep="\t",check.names=F)
# f2=f2[,1:100]
# colnames(f2)=substr(colnames(f2),1,8)
# write.table(f2,"file2.txt",sep="\t",row.names=F)

# sans metadata correction='none'
metaData=read.csv(file=paste0(wd,"/","metaData.csv"),header=T,sep=";")
colnames(metaData)

resNone=ptrvListIntensityByTime(listFiles=listFiles,metaData=metaData,timeCol="RelTime",colToRemove=c("AbsTime","Cycle"),
                        removeBlankTime=FALSE,ions=NULL,dec_vec=rep(".",length(listFiles)),
                        sep="\t",correction="none",timeBlank=c(0,30),halfWindowSize=5,
                        method="MAD",total=FALSE,breathRatio=FALSE,stat="area",minimalDuration=2,
                        smoothMethod="MovingAverage",minExpi=NULL,maxInspi=NULL,forMinExpiDivideMaxIntBy=5,
                        forMaxInspiDivideMaxIntBy=4,wd=wd)

# avec metadata correction=cycle
resCorrected=ptrvListIntensityByTime(listFiles=listFiles,metaData=metaData,timeCol="RelTime",colToRemove=c("AbsTime","Cycle"),
                        removeBlankTime=FALSE,ions=NULL,dec_vec=rep(".",length(listFiles)),
                        sep="\t",correction="cycle",timeBlank=c(0,30),halfWindowSize=5,
                        method="MAD",total=FALSE,breathRatio=FALSE,stat="area",minimalDuration=2,
                        smoothMethod="MovingAverage",minExpi=NULL,maxInspi=NULL,forMinExpiDivideMaxIntBy=5,
                        forMaxInspiDivideMaxIntBy=4,wd=wd)


resCorrected$totalIntensity
resCorrected$time
resCorrected$listRes

grid.arrange(grobs=resCorrected$cycleLimits)
