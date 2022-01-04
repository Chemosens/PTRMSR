#'@title plot.ibt
#'@inheritParams plot_fullspectra
#'@param selec vector containing the names of the spectra to be displayed
#'@param groupOption "no", "name" or file.
#'@export
#'@import ggplot2
plot.ibt=function(x,main="Chromatogram",xlab="time",ylab="intensity",ylim=NULL,xlim=NULL,selec=NULL,groupOption="no",...)
{
  if(!is.null(selec)){x$df=x$df[x$df[,"name"]%in%selec,]}
  group=NULL
  if(groupOption=="name" & "name"%in%colnames(x$df))
  {
    group=x$df[,"name"]
  }
  #else
  #{
  #  group=groupOption
  #}
  
  p1 <- plot_fullspectra(x=x$df[,"time"],y=x$df[,"intensity"],group=group,main=main,xlab=xlab,xlim=xlim,ylim=ylim,...)
   return(p1)
}