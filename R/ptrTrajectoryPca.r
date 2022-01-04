#' Trajectory PCA
#' 
#' Computes a PCA of trajectories. Time is decomposed in time periods. The evaluation*periods are then used as individuals to produce a PCA.  These individuals are plotted then linked by a segments for successive periods.
#' @param tds a tds object \link[chemosensR]{tdsRead}
#' @param axes list of pairs of axes (for example list(c(1,2),c(1,3),c(2,3)). Default to list(c(1,2))
#' @param periods number of periods required in the cutting in time-periods
#' @param individual representation of individuals Default to "arrows"
#' @param smooth TRUE if the data should be smoothed, FALSE ifelse. Default to FALSE
#' @param title title of the graph
#' @param fontSizeCex size of graphical textual elements (integers)
#' @return A list containing the coordinates of PCA, then output containing the ggplot object required for the plot
#' @export
ptrTrajectoryPca=function(tds,axes=list(c(1,2)),title="Trajectory PCA",periods=10,individual="arrows",fontSizeCex=1,smooth=FALSE,expandBiplot=NULL,parameters=NULL)
{
  if (!is.null(parameters$axes)) { axes=parameters$axes }
  if (!is.null(parameters$periods)) { periods=parameters$periods }
  if (!is.null(parameters$individual)) { individual=parameters$individual }
  if (!is.null(parameters$fontSizeCex)) { fontSizeCex=parameters$fontSizeCex }
  if (!is.null(parameters$title)) { title=parameters$title }
  if (!is.null(parameters$smooth)) { smooth=parameters$smooth }
  if(class(tds)=="tds"){df=tds$stdDominances}else{df=tds}
  # Calcul sur temps standardisés
  
  df$period=((df$time-0.01)%/%(1/periods))+1
  df$period=sprintf("%02d", df$period)
  df$product=paste(df$product,df$period,sep="_period")
  df2=aggregate(score~product+subject+descriptor,df,sum)
  listCoord=PCAgg(df2,axes=axes,representation="DistanceBiplot",expandBiplot=expandBiplot)
  product_period=t(as.data.frame(stringr::str_split(listCoord$indivCoord$name,"_period")))
  listCoord$indivCoord$product=product_period[,1]
  listCoord$indivCoord$period=product_period[,2]
  listCoord$indivCoord$name2=listCoord$indivCoord$product
  listCoord$indivCoord$name2[listCoord$indivCoord$period!="1"]=""
  
  
  # color_group <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666",
  #                  "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3",
  #                   "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
  #                   "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6",
  #                   "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC",
  #                   "#CCCCCC", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
  #                   "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
  #                   "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
  
  
  gg = gMapPlot(listCoord,title=title,facet="grid",cols="axes",type=individual,ellipses=FALSE,output="var", fontSizeCex=fontSizeCex) 
  gg = gg 
  if (smooth==FALSE) {
    gg = gg + ggplot2::geom_point(data = listCoord$indivCoord, ggplot2::aes_string(x = "x", y = "y",col="product"),show.legend = FALSE) +
      ggplot2::geom_path(data = listCoord$indivCoord,ggplot2::aes_string(x = "x", y = "y",col="product"), arrow=ggplot2::arrow(length = ggplot2::unit(0.2, "cm")))
  } else {
    gg = gg + ggplot2::geom_smooth(data = listCoord$indivCoord,ggplot2::aes_string(x = "x", y = "y",col="product"),se = FALSE) +
      ggplot2::update_geom_defaults("smooth", list(size = 0.6))
  }
  gg = gg + ggrepel::geom_text_repel(data = listCoord$indivCoord, ggplot2::aes_string(x = "x", y = "y", label="name2",colour="product"), nudge_y = 0.5,size=3*fontSizeCex,segment.color = NA,show.legend = FALSE) 
  #ggplot2::scale_colour_manual(values=color_group)
  gg
  
  #bug avec inertie
  
  result=list()
  result[["output"]][["biplot"]]=gg
  result[["coordinates"]]=listCoord
  return (result)
}
