#' peakDiagnosis
#' Returns potential problematic peaks: when one single peak is exepected, the number of candidate is displayed. When no peak is detected, no peak is displayed.
#' @param df data frame whose colnames are time and intensity
#' @param minimalIntensity minimal intensity required to be a peak of interest
#' @param threshold percentage of relative intensity of the maximal peak to be reached.
#' @param nPoints number of points required in the smoothing
#'  @export
peakDiagnosis=function(df,minimalIntensity=0,threshold=50,nPoints=101)
{
  time=intensity=NULL
  problematic="no problem"
  df[df[,"intensity"]<0,"intensity"]=0
  respeak=pickingPeaks(df,col="time",signalPercentage=50,nPoints=nPoints,refineMz="descendPeak",method="MAD")
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

