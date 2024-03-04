#'@title ptrIntensityByTime
#'@param ptr a ptr object (from ptrRead function)
#'@param rt A numeric(2) or two-column matrix defining the lower and upper boundary for the retention time range/window(s) for the chromatogram(s). If a matrix is provided, a chromatogram is extracted for each row. If not specified, a chromatogram representing the full retention time range is extracted.
#'@param mz A numeric(2) or two-column matrix defining the mass-to-charge (mz) range(s) for the chromatogram(s). For each spectrum/retention time, all intensity values within this mz range are aggregated to result in the intensity value for the spectrum/retention time. If not specified, the full mz range is considered.
#'@param integrationTable If not NULL, gives the intensity by time according to an integrationTable
#'@export
#' @title ptrvIntensity
ptrIntensityByTime=function(ptr,rt=NULL,mz=NULL,integrationTable=NULL,concentration=NULL,primaryIons=NULL)
{
  Time=ptr$Time
  duplicatedTime=duplicated(Time)
  Time=Time[!duplicatedTime]
  tofData=ptr$TofData
  MassAxis=ptr$MassAxis
  df=NULL
  if(is.null(integrationTable))
  {
    if(is.null(mz))
    {
      if(is.null(rt))
      {
        print("Sum spectra is returned")
        result=apply(tofData,2:3,sum)
      }
      if(length(rt)==2)
      {
        #TODO
#        tofSubset=tofData[which(MassAxis>mz[1]&MassAxis<mz[2]),,]
#        result=apply(tofSubset,2:3,sum)
      }
      df=data.frame("time"=Time,"intensity"=as.vector(result)[!duplicatedTime])
    }
    if(length(mz)==2)
    {
      if(length(rt)==0)
      {
        tofSubset=tofData[which(MassAxis>mz[1]&MassAxis<mz[2]),,]
        result=apply(tofSubset,2:3,sum)
        df=data.frame(time=Time,intensity=as.vector(result)[!duplicatedTime])
      }
      if(length(rt)==2)
      {

      }
  }

  }
  else
  {
    if(length(integrationTable)==1)
    {
      if(integrationTable=="device")
      {
        integrationTable=ptr$PeakTable
        colnames(integrationTable)=c("name","mz","inf","sup")

      }
    }


    for(i in 1:dim(integrationTable)[1])
    {
#      message(paste0(i,"/",dim(integrationTable)[1]))
      masses_label=MassAxis[ MassAxis>=integrationTable[i,"inf"] &MassAxis<=integrationTable[i,"sup"]]
      tofSubset=tofData[which(MassAxis%in%masses_label),,]
      if(!is.null(rt))
      {
        #TODO

      }
      sumSubset=apply(tofSubset,2:3,sum)
      dfi=data.frame("time"=Time,"intensity"=as.vector(sumSubset)[!duplicatedTime],"mz"=integrationTable[i,"mz"],name=integrationTable[i,"name"])
      df=rbind(df,dfi)
    }
  }
  res=list(df=df)
  class(res)="ibt"
  return(res)
}