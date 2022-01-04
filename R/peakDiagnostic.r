#' @export
peakDiagnosis=function(resionperiod,minimalIntensity=0,threshold=50,nPoints=101)
{
  problematic="no problem"
  resionperiod[resionperiod[,"intensity"]<0,"intensity"]=0
  respeak=pickingPeaks(resionperiod,col="time",signalPercentage=50,nPoints=nPoints,refineMz="descendPeak",method="MAD")
  p=ggplot(respeak$smooth,aes(x=time,y=intensity))+geom_line()

  interestingPeaks=respeak$df[respeak$df[,"relative"]>threshold&respeak$df[,"intensity"]>minimalIntensity,]
  print(nrow(interestingPeaks))
  if(nrow(interestingPeaks)>1)
  {
    problematic=nrow(interestingPeaks)
  }
  if(nrow(interestingPeaks)==0)
  {
    problematic="no peak"
  }
  return(list(problematic=problematic,p=p,interestingPeaks=interestingPeaks))
}

