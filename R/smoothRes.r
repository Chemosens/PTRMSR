smoothRes=function(result,smooth)
{
  p=list()
  ions=unique(result[,"ion"])
  for(ion in ions)
  {
    resion=result[result[,"ion"]==ion,]
    resion2=resion[order(resion[,"time"]),]
    ggplot(resion2,aes(x=time,y=intensity))+geom_line()
    smdata=smooth.spline(x=resion2[,"time"],y=resion2[,"intensity"],w=resion2[,"duration"],spar=0.7)
    plot(resion2[,"time"],y=resion2[,"intensity"],type="l")
    lines(smdata$x,smdata$y,type="l",col="red")
    df=data.frame(x=smdata$x,y=smdata$y,ion=ion)
    p[[ion]]=ggplot(resion2,aes(x=time,y=intensity))+geom_line()+geom_line(data=df,aes(x=x,y=y),color="red",inherit.aes = FALSE)+theme_bw()
  }
  return()
}
