#'@export
timeForPropPeak=function(time,intensity,proportion=0.7,timing="first")
{
  match.arg(timing,c("first","last"))
  #"beforeClose","afterClose"
  if(length(time)!=length(intensity)){stop("time should have the same length as intensity")}
  if(any(diff(time)<0))
  {
    warning("time are not ordered")
    indexOrder=order(time)
    time=time[indexOrder]
    intensity=intensity[indexOrder]
  }
  intensity=intensity-min(intensity)
  imax=max(intensity,na.rm=T)
  indexmax=which.max(intensity)
  intensityToReach=proportion*imax
  potentialIndex=which(intensity>intensityToReach)
  if(length(potentialIndex)!=0)
  {
    if(timing=="first")
    {
      reach=potentialIndex[1]
    }
    if(timing=="last")
    {
      reach=potentialIndex[length(potentialIndex)]
    }
    # if(timing=="beforeClose")
    # {
    #   potentialIndexInf=potentialIndex[potentialIndex<indexmax]
    #   reach=potentialIndexInf[1]
    # }
    # if(timing=="afterClose")
    # {
    #   potentialIndexSup=potentialIndex[potentialIndex>indexmax]
    #   reach=potentialIndexSup[length(potentialIndexSup)]
    # }
    timeReach=time[reach]
  }
  else{timeReach=NA}

  return(timeReach)
}

