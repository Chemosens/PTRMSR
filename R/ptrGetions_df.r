#'@importFrom stats smooth.spline
#'@importFrom reshape2 dcast
get_ions_df=function(ions,spectra,time,type="long",spar=0)
{
  nbIons=length(ions)
  
  if(spar>0)
  {
    spectral=lapply(spectra,function(x)    {
      smoothres=smooth.spline(x=time,y=x,spar=spar)
      return(smoothres$y)
    })
    timesmooth=lapply(spectra,function(x)
    {
      smoothres=smooth.spline(time,x,spar=spar)
      return(smoothres$x)
    }  )
  }
  
  if(spar>0)
  {
    intensite=Reduce(c,(spectral[ions]))
    timeline=Reduce(c,(timesmooth[ions]))
    
  }
  else
  {
    intensite=Reduce(c,(spectra[ions]))
    timeline=rep(as.vector(time),nbIons)
  }
  
  
  ions_rep=rep(ions,each=length(timeline)/nbIons)
  ions_df=data.frame(intensite=intensite,time= timeline,ions_rep)
  if(type=="wide")
  {
    wideTable=dcast(data=ions_df,time~ions_rep,function(x) mean(x,na.rm=T),value.var="intensite")
    ions_df=wideTable   
  }
  return(ions_df)
}