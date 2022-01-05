#' smoothRes
#' Returns a list of all graph smoothed
#' @param df data.frame with ion, time, intensity as columns
#' @param smooth number between 0 and 1 indicating the degree of smoothing -same as smooth.spline
#' @importFrom stats smooth.spline
smoothRes=function(df,smooth=0.7)
{ time=intensity=x=y=NULL
  p=list()
  ions=unique(df[,"ion"])
  for(ion in ions)
  {
    resion=df[df[,"ion"]==ion,]
    resion2=resion[order(resion[,"time"]),]
    ggplot(resion2,aes(x=time,y=intensity))+geom_line()
    smdata=smooth.spline(x=resion2[,"time"],y=resion2[,"intensity"],w=resion2[,"duration"],spar=smooth)
    plot(resion2[,"time"],y=resion2[,"intensity"],type="l")
    lines(smdata$x,smdata$y,type="l",col="red")
    df=data.frame(x=smdata$x,y=smdata$y,ion=ion)
    p[[ion]]=ggplot(resion2,aes(x=time,y=intensity))+geom_line()+geom_line(data=df,aes(x=x,y=y),color="red",inherit.aes = FALSE)+theme_bw()
  }
  return(p)
}
