#'@title plot.ibm
#'@export
#'@param x an ibm object (from intensityByMass functions)
#'@param subset vector containing some names present in the "name" column
#'@inheritParams plot_fullspectra
#'@param groupOption option for grouping the data "name" or "no"
#'@param type "l" for line, "bar" for barplot
#'@examples
#'#data(lcms)
#'#res=lcmsIntensityByMass(lcms)
#'#plot(res)
plot.ibm=function(x,subset=NULL,main="Mass spectra",xlab="Mass",ylab="Intensity",groupOption="no",xlim=NULL,ylim=NULL,type="l",...)
{
  name=NULL
  if(!is.null(subset)){x$df=x$df[x$df[,"name"] %in% subset,]}
  if(groupOption=="name"){group=x$df[,"name"]}
  if(groupOption=="no"){group=NULL}
  if(!groupOption%in%c("name","no")){group=groupOption}
  if(type=="l")
  {
    p2 <- plot_fullspectra(x=x$df[,"mz"],y=x$df[,"intensity"],group=group,main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
  }
  if(type=="bar")
  {
    p2=ggplot(x$df,aes(x=name,y=intensity,fill=name))+geom_col(position="dodge")+theme_bw()+theme(legend.position="none")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
   return(p2)
}