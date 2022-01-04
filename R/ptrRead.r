#' Read .h5 file from PTR and return a list containing useful results
#'@param wd path for the working directory
#'@param file file .h5 to read
#'@param mz If not NULL, vector of length 2 representing a relevant interval of mz to be studied
#'@param threshold threshold 
#'@param sumSpectraOnly load only sum spectra  
#'@param integrationTable integrationTable
#'@export
#'@importFrom rhdf5 h5read h5closeAll
#'@examples
#' #DONOTRUN
#' #file <- dir(system.file(package = "chemosensR", dir = "extdata"),full.name=TRUE,pattern="h5$")
#' #ptr=ptrRead(file)
ptrRead=function(file,wd=getwd(),mz=NULL,threshold=0,sumSpectraOnly=FALSE,integrationTable=NULL)
{
  h5closeAll()
  originalwd=getwd()
  setwd(wd)
  FullSpectra=h5read(file=file,name="FullSpectra")
  PeakData=h5read(file=file,name="PeakData")
  PeakData$PeakTable[PeakData$PeakTable[,"label"]=="cluster H2O"&round(PeakData$PeakTable[,"mass"])==55 ,"label"]="cluster H2O (trimere)"
  TimingData=h5read(file=file,name="TimingData")
  setwd(wd)
  toSelect=NULL
  if(!is.null(mz))
  {
      toSelect=FullSpectra$SumSpectrum>threshold&FullSpectra$MassAxis<mz[2]&FullSpectra$MassAxis>mz[1]
  }
  else
  {
    toSelect=FullSpectra$SumSpectrum>threshold
  }
  
    MassAxis=FullSpectra$MassAxis[toSelect]
    SumSpectrum=FullSpectra$SumSpectrum[toSelect]
    indexMass=(1:length(FullSpectra$MassAxis))[toSelect]
   

  PeakTable=PeakData$PeakTable
  Time=TimingData$Buff

  if(sumSpectraOnly)
  {
    res=list(MassAxis=MassAxis,SumSpectrum=SumSpectrum,PeakTable=PeakTable,indexMass=indexMass,Time=as.vector(TimingData$BufTimes))
  }
  else
  {
    TofData=FullSpectra$TofData[toSelect,,,]
    res=list(MassAxis=MassAxis,SumSpectrum=SumSpectrum ,TofData=TofData,PeakTable=PeakTable,indexMass=indexMass,Time=as.vector(TimingData$BufTimes),matTime=dim(TimingData$BufTimes))
  }
  
  if(!is.null(integrationTable))
  {
    spectrum=ptrIntensityByMass(MassAxis,TofData,integrationTable)
    res=list(MassAxis=MassAxis,SumSpectrum=SumSpectrum,spectrum=spectrum,PeakTable=PeakTable,indexMass=indexMass,Time=as.vector(TimingData$BufTimes),matTime=dim(TimingData$BufTimes))
  }
  return(res)
}
