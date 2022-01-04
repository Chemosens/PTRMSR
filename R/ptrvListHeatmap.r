#' @param df data frame with "ion","intensity","product","subject" columns,
#' @param log boolean indicated if the intensities should be logged (TRUE) or not (FALSE)
#' @param ionToRemove vector of character containing ions to remove from the analysis
#' @param normalizeByEval Boolean. By default TRUE
#' @param formula as.formula(product+subject+rep~ion) formula to be taken into account
#' @param fun.aggregate "mean", "max" or "sum" for regrouping according to the frmula
#' @param clusterRows Boolean indicating whether the rows should be clustered
#' @param clusterCols Boolean indicating whether the columns should be clustered
#' @param showRownames Boolean indicating whether the rownames should be displayed
#' @export
ptrvListHeatmap=function(df,log=FALSE,ionToUse=NULL,normalizeByEval=FALSE,ionToRemove=NULL,formula=as.formula(product+subject+rep~ion),fun.aggregate="mean",normalization="none",breaks=NULL,clusterRows=F,clusterCols=F,showRownames=F,showColnames=T,annotationRow="product",scale="column",cex=0.8,transpose=FALSE)
{

  if(log) { print("log"); df[,"intensity"]=log(df[,"intensity"])    }
  ionToUse=unique(df[,"ion"]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
  if(normalizeByEval){df=normalizeByEval(df,ionToUse)$intens}

  if(fun.aggregate=="mean")
  {
    dcdf=dcast(df,formula=formula,value.var="intensity",fun.aggregate=mean)
  }
  if(fun.aggregate=="sum")
  {
    dcdf=dcast(df,formula=formula,value.var="intensity",fun.aggregate=sum)
  }
  if(fun.aggregate=="max")
  {
    dcdf=dcast(df,formula=formula,value.var="intensity",fun.aggregate=max)
  }
  toHm=dcdf[,-which(colnames(dcdf)%in%c("product","subject","rep"))]
  if(normalizeByEval)
  {
      toHm=as.matrix(toHm)
      toHm=sweep(toHm,1,apply(toHm,1,function(x){return(sum(x,na.rm=T))}),"/")
  }
  if(!"rep" %in% colnames(dcdf))
  {
    rownames(toHm)=paste0(dcdf[,"product"],"_",dcdf[,"subject"])
  }
  if("rep" %in% colnames(dcdf))
  {
    rownames(toHm)=paste0(dcdf[,"product"],"_",dcdf[,"subject"],"_",dcdf[,"rep"])
  }
  toHm[is.na(toHm)]=min(0,min(toHm,na.rm=T))
  if(transpose){toHm=t(toHm)}

  if(annotationRow=="product")
  {
    if(!transpose)
    {
      annotation_row=data.frame(prod=factor(dcdf[,"product"]))
      annotation_col=NULL
      rownames(annotation_row)=rownames(toHm)

    }
    if(transpose)
    {
      annotation_row=NULL
      annotation_col=data.frame(prod=factor(dcdf[,"product"]))
      rownames(annotation_col)=colnames(toHm)

    }

  }
  if(annotationRow=="subject")
  {
    if(!transpose)
    {
      annotation_row=data.frame(prod=factor(dcdf[,"subject"]))
      annotation_col=NULL
      rownames(annotation_row)=rownames(toHm)
    }
   if(transpose)
   {
     annotation_col=data.frame(prod=factor(dcdf[,"subject"]))
     annotation_row=NULL
     rownames(annotation_col)=colnames(toHm)
   }
   }
  if(is.null(breaks))
  {
    if(scale=="none")
      {
       breaks=seq(min(toHm),max(toHm),length.out=100)
       }
      if(scale=="column"||scale=="row")
      {
        breaks=seq(-2,2,length.out=100)
      }

  }
  cols=rainbow(length(breaks),start=0,end =0.8)
  ph=pheatmap(toHm,annotation_row=annotation_row,annotation_col=annotation_col,cluster_rows=clusterRows,clustering_distance_cols="correlation",cluster_cols=clusterCols,cex=cex,scale=scale,breaks=breaks,color=cols,show_rownames=showRownames,show_colnames=showColnames,main=fun.aggregate)
  return(ph)
}
