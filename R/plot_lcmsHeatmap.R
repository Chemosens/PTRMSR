#'@title plot_lcmsHeatmap
#'@param x result of lcmsHeatmap
#'@param scale_limits a numeric vector of size two for the limits of the color scale
#'@param log if TRUE, the data is logtransformed before plotting
#'@inheritParams plot_fullspectra
#'@export
#'@import ggplot2 
#'@importFrom MSnbase mz
plot.hm=function(x,main="pheatmap",log=FALSE,scale_limits=NULL,...)
{
  int=rt=NULL
  if(log)
  {
   x$df[,"int"]=log(x$df[,"int"]+1)
   x$df[x$df[,"int"]<scale_limits[1],"int"]=scale_limits[1]
   x$df[x$df[,"int"]>scale_limits[2],"int"]=scale_limits[2]
    
    g1 <- ggplot(x$df, aes(x = rt, mz)) +
      geom_tile(aes(fill = int))+   theme(axis.text.x=element_blank(),axis.text.y=element_blank())
  }
  else
  {
    x$df[x$df[,"int"]<scale_limits[1],"int"]=scale_limits[1]
    x$df[x$df[,"int"]>scale_limits[2],"int"]=scale_limits[2]
    g1 <- ggplot(x$df, aes(x = rt, mz)) +
      geom_tile(aes(fill = int))+   theme(axis.text.x=element_blank(),axis.text.y=element_blank())
  }
  g1<-g1+  scale_fill_gradientn(colours=c("dark blue","blue", "green","yellow","red"),limits=scale_limits)
  g1<- g1 + labs(title=main)
  return(g1)
}
