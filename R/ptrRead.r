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
#' #file <- dir(system.file(package = "PTRMSR", dir = "extdata"),full.name=TRUE,pattern="h5$")
#' #ptr=ptrRead(file)
ptrRead=function(file,wd=getwd(),mz=NULL,threshold=0,sumSpectraOnly=FALSE,integrationTable=NULL)
{
  spectrum=NULL
  h5closeAll()
  originalwd=getwd()
  setwd(wd)
  print("Loading sum spectrum")
  sumSpectrum=h5read(file=file,name="FullSpectra/SumSpectrum")
  print("Loading mass axis")
  massAxis=h5read(file=file,name="FullSpectra/MassAxis")

  print("Loading peak data")
  PeakData=h5read(file=file,name="PeakData")
  PeakData$PeakTable[PeakData$PeakTable[,"label"]=="cluster H2O"&round(PeakData$PeakTable[,"mass"])==55 ,"label"]="cluster H2O (trimere)"
  print("Loading timing data")
  TimingData=h5read(file=file,name="TimingData/BufTimes")
  setwd(wd)
  toSelect=NULL
  if(!is.null(mz))
  {
      toSelect=sumSpectrum>threshold&massAxis<mz[2]&massAxis>mz[1]
  }
  else
  {
    toSelect=sumSpectrum>threshold
  }

    MassAxis=massAxis[toSelect]
    SumSpectrum=sumSpectrum[toSelect]
    indexMass=(1:length(massAxis))[toSelect]


  PeakTable=PeakData$PeakTable
  Time=TimingData

  if(sumSpectraOnly)
  {
    print("ok")
    res=list(MassAxis=MassAxis,SumSpectrum=SumSpectrum,PeakTable=PeakTable,indexMass=indexMass,Time=as.vector(TimingData))
  }
  else
  {
    print("loading tof")
    tof=h5read(file=file,name="FullSpectra/TofData")
    TofData=tof[toSelect,,,]
    res=list(MassAxis=MassAxis,SumSpectrum=SumSpectrum ,TofData=TofData,PeakTable=PeakTable,indexMass=indexMass,Time=as.vector(TimingData),matTime=(TimingData))
  }

  if(!is.null(integrationTable))
  {
    # print("ptrIntensityByMass")
    # spectrum=ptrIntensityByMass(MassAxis,TofData,time=Time,integrationTable,type="integration")
    res=list(MassAxis=MassAxis,SumSpectrum=SumSpectrum,spectrum=spectrum,PeakTable=PeakTable,indexMass=indexMass,Time=as.vector(TimingData),matTime=TimingData)
  }
  return(res)
}
