#' This function allows to draw spectra according to a group and potential smoothing
#' @param x abscissa of the spectra
#' @param y ordinate of the spectra
#' @param group group for each spectra
#' @param spar if 0, no smoothing. Ifelse the smoothing parameter is between 0 and 1 and indicates a smoothing.
#' @param integrationTable NULL
#' @param xlim vector of two numbers representing the limits of x axis.
#' @param ylim vector of two numbers representing the limits of y axis.
#' @param xlab character indicating the label on the x axis
#' @param ylab character indicating the label on the y axis
#' @param legend if TRUE the legend is displayed
#' @param threshold NULL
#' @param main title of the graph
#'@param ... Further parameters in plots such as xlim,xlab,ylim,ylab
#' @export
#' @import ggplot2
#' @importFrom stats smooth.spline
plot_fullspectra=function(x,y,group=NULL,spar=0,integrationTable=NULL,xlim=NULL,ylim=NULL,threshold=0,main="Spectra",xlab="Mass",ylab="Intensity",legend=TRUE,...)
{
  ind=!is.na(x)&!is.na(y)
  x=x[ind]
  y=y[ind]
  group=group[ind]
   if(is.null(group)){ df_gg=data.frame(x=x,y=y)
  }
  else{ df_gg=data.frame(x=x,y=y,group=group)
  }
 
  
  if(spar!=0)
  {
    if(is.null(group))
    {
      smoothres=smooth.spline(x=x,y=y,spar=spar)
      df_gg=data.frame(x=smoothres$x,y=smoothres$y)
    }
    if(!is.null(group))
    {
      group=factor(group)
      groups=levels(group)
      df_gg=NULL
      
      for(i in 1:length(groups))
      {
        smoothres=smooth.spline(x=x[group==groups[i]],y=y[group==groups[i]],spar=spar)
        df_ggi=data.frame(x=smoothres$x,y=smoothres$y,group=rep(groups[i],length(smoothres$x)))
        df_gg=rbind(df_gg,df_ggi)
      }
    }
   
   
    
    
  }
  if(is.null(xlim)){xlim=c(min(df_gg[,"x"]),max(df_gg[,"x"]))}
  if(is.null(ylim)){ylim=c(min(df_gg[,"y"]),max(df_gg[,"y"]))}
  if(is.null(group))
    {p<-p<-ggplot(df_gg,aes(x=x,y=y))}
    else{
      p<-ggplot(df_gg,aes(x=x,y=y,group=group,color=as.factor(group)))
    }
#  if(type=="l")
#  {
    p<- p+geom_line(size=0.5)+xlim(xlim[1],xlim[2])+ylim(ylim[1],ylim[2])+theme_bw()
#  }
#  if(type=="bar")
#  {
#    p<-p+geom_col(na.rm=T,position="dodge")
#  }
  p<-p+labs(title=main,x=xlab,y=ylab)
 # if(!is.null(integrationTable))
  # {
  #    decreasingIntIndex=order(y,decreasing=T)
  #    orderedSumy=y[decreasingIntIndex]
  #    orderedx=x[decreasingIntIndex]
  #    orderedIons=rep(NA,length(decreasingIntIndex))
  #    for(label in integrationTable[,"name"])
  #    {
  #      x_index=which(
  #        orderedx>=integrationTable[integrationTable[,"name"]==label,"inf"] 
  #        &orderedx<=integrationTable[integrationTable[,"name"]==label,"sup"]
  #      )
  #      orderedIons[x_index]=label
  #      df_i=data.frame(x=orderedx[x_index],y=orderedSumy[x_index])
  #      
  #      
  #      
  #      p<- p + geom_line(data=df_i,aes(x=x,y=y),color="green")
  #      # p<- p + geom_point(data=df_i,aes(x=x,y=y,text=label),color="green")
  #    }
  # #   orderdf=data.frame(x=orderedx,Sumy=orderedSumy,orderedIons=orderedIons)
  # #   return(list(df=orderdf[orderdf[,"Sumy"]>threshold,],p=p))
  #    return(p)
  #  }
  # else
  # {
  
  if(!legend)
  {
    p=p+ theme(legend.position = "none")
  }
  
    return(p)
  #}
  
}