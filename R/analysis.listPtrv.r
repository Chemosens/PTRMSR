#' @title analysis.listPtrv
#' @param type  Choose among: "CVA of abundances","ANOVA of intensities","PCA of abundances","Intensity Heatmap"
#' @param ionToRemove vector of character containing ions to remove from the analysis
#' @param normalizeByEval Boolean. By default TRUE
#' @param log boolean indicated if the intensities should be logged (TRUE) or not (FALSE)
#' @param ... Further analysis parameters
#' @inheritParams analysis
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom utils tail
#' @importFrom grDevices rainbow
#' @importFrom chemosensR PCAgg gMapPlot CVAgg gDistributionPlot gGetLegend anovaTable gBarPlot runBy

analysis.ptrvList=function(x,type,ionToRemove=NULL,normalizeByEval=TRUE, log=FALSE,outputFormat="", outputFile="", title="", runBy="", selection="", ...)
{
  time=NULL
  ptrvListPca=function(df,parameters)
  {
    name=y=NULL
    if (is.null(parameters$fontSizeCex)) { parameters$fontSizeCex=1 }
    if(is.null(parameters$representation)){representation="twoMaps"}else{representation=parameters$representation}
    if (is.null(parameters$title)) { title="PCA" } else { title=parameters$title }
    if (is.null(parameters$axes)) { axes=list(c(1,2)) } else { axes=parameters$axes }
    if(is.null(parameters$option)){option="Correlation"}else{option=parameters$option}
    colnames(df)[colnames(df)=="ion"]="descriptor"
    colnames(df)[colnames(df)=="intensity"]="score"

    listCoord=PCAgg(df,option=option,representation=representation,axes=axes)

    gg1 = gMapPlot(listCoord,title=title,facet="grid",cols="axes",type="points",output="ind",indsup=F)
    gg2 = gMapPlot(listCoord,title=title,facet="grid",cols="axes",type="points",output="var",indsup=T)
    varcor=listCoord$varCoord
    varcor_order1=varcor[order(abs(varcor[,"x"]),decreasing=F),]
    varcor_order2=varcor[order(abs(varcor[,"y"]),decreasing=F),]
    varcor_order1[,"name"]=factor(varcor_order1[,"name"],levels=varcor_order1[,"name"])
    varcor_order2[,"name"]=factor(varcor_order2[,"name"],levels=varcor_order2[,"name"])
    gg3=ggplot2::ggplot(data=tail(varcor_order1,n=10),ggplot2::aes(x = x, y = name))+ggplot2::geom_col()+ggplot2::ggtitle("Best correlations with axis 1")+ggplot2::theme_bw()
    gg4=ggplot2::ggplot(data=tail(varcor_order2,n=10),ggplot2::aes(x = y, y = name))+ggplot2::geom_col()+ggplot2::ggtitle("Best correlations with axis 2")+ggplot2::theme_bw()

    gg5 = gMapPlot(listCoord,title=title,facet="grid",cols="axes",type="points",fontSizeCex=parameters$fontSizeCex)

    result=list()
    result[["output"]][["indplot"]]=gg1
    result[["output"]][["varplot"]]=gg2
    result[["output"]][["barplot1"]]=gg3
    result[["output"]][["barplot2"]]=gg4
    result[["output"]][["biplot"]]=gg5
    result[["coordinates"]]=listCoord
    return (result)
  }

  ptrvListIntensityDistribution=function(df, parameters) {
    gg <- gDistributionPlot(df,plot=c("violin","boxplot"), y="intensity", x="product", fill="product", rows=".",cols=".", wrap="ion", facet="wrap", title=parameters$title, labX = NULL, labY=NULL, grid=FALSE, labels=NULL, colors=NULL,scales="free_y",vnames=NULL)
    result <- list()
    result[["output"]][["durationsDistribution"]] <- gg
    return (result)
  }

  ptrvListCurves=function(df, parameters) {
    result = list()
    if (is.null(parameters$rows)) { rows="product" } else { rows=parameters$rows }
    if (is.null(parameters$cols)) { cols="." } else { cols=parameters$cols }
    if (is.null(parameters$color)) { color="ion" } else { color=parameters$color }
    if (is.null(parameters$smooth)) { smooth=TRUE } else { smooth=parameters$smooth }
    if (is.null(parameters$stopTime)) { stopTime=NULL } else { stopTime=parameters$stopTime }
    if (is.null(parameters$metaData)) { stop("metaData is required as parameter") } else { metaData=parameters$metaData }
    if (is.null(parameters$npoints)) { npoints=1000 } else { npoints=parameters$npoints }
   # df2=ptrvListComparableData(df=df,metaData=metaData)
    df3=ptrvSameTimePoints(df,npoints=npoints)
    #gg = gCurvePlot(df3, x="time",y="intensity", color=color, title=title, facet="grid", rows=rows, cols=cols, wrap=NULL, labX = "Time", labY="Intensity", grid=FALSE, labels=NULL, colors=NULL,scales="fixed",vnames=NULL, smooth=smooth)
    product=ion=NULL
    result=list()
    gg = ggplot(df3,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+facet_grid(rows=vars(product),cols=vars(rep))+theme_bw()
    result[["output"]][["Legend"]]= gGetLegend(gg)
    result[["parameters"]] = parameters
    result[["output"]][["Curves"]] = gg+theme(legend.position="none")
    return (result)
  }

  ptrvListScaledCurves=function(df, parameters) {
    product=ion=NULL
    result = list()
    if (is.null(parameters$rows)) { rows="product" } else { rows=parameters$rows }
    if (is.null(parameters$cols)) { cols="." } else { cols=parameters$cols }
    if (is.null(parameters$color)) { color="ion" } else { color=parameters$color }
    if (is.null(parameters$smooth)) { smooth=TRUE } else { smooth=parameters$smooth }
    if (is.null(parameters$stopTime)) { stopTime=NULL } else { stopTime=parameters$stopTime }
    if (is.null(parameters$metaData)) { stop("metaData is required as parameter") } else { metaData=parameters$metaData }
    if (is.null(parameters$npoints)) { npoints=30 } else { npoints=parameters$npoints }

    df6=ptrvSameTimePoints(df,npoints=npoints,normalizeByTime=T)
    # df2=ptrvListComparableData(df=df,metaData=metaData)
#gg = gCurvePlot(df3, x="time",y="intensity", color=color, title=title, facet="grid", rows=rows, cols=cols, wrap=NULL, labX = "Time", labY="Intensity", grid=FALSE, labels=NULL, colors=NULL,scales="fixed",vnames=NULL, smooth=smooth)
    gg = ggplot(df6,aes(x=time,y=intensity,group=ion,color=ion))+geom_line()+facet_grid(rows=vars(product))+theme_bw()
    result[["output"]][["Legend"]]= gGetLegend(gg)
    result[["parameters"]] = parameters
    result[["output"]][["Scaled Curves"]] = gg+theme(legend.position="none")

    return (result)
  }

  ptrvListDurationsClusters=function(df, parameters) {

    formula="subject+rep~product+descriptor"
    meltMat=reshape2::dcast(df,as.formula(formula))
    lp=strsplit(formula, "~")
    elements=strsplit(lp[[1]][1],"+",fixed=TRUE)[[1]]
    rownames(meltMat)=apply(meltMat[,elements],1,paste, collapse = "-")
    res.pca=FactoMineR::PCA(meltMat[,!(colnames(meltMat) %in% elements)], graph = FALSE)
    res.hcpc <- FactoMineR::HCPC(res.pca, graph = FALSE)
    gg <- factoextra::fviz_dend(res.hcpc, cex = 0.7, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 0.8, main = parameters$title, horiz=TRUE)
    result <- list()
    result[["output"]][["durationsClusters"]] <- gg
    return (result)
  }

  ptrvListSequences=function(df, parameters) {

    #time2=NULL
    #df[,"time2"]=df[,"time"]-df[,"duration"]/2
    tmp=dcast(df,product+subject+rep+time+duration~ion,value.var="intensity",fun.aggregate=mean)
    tmp[tmp<0]=0
    ion=colnames(tmp)[-c(1:5)]
    res_tmp=reshape(tmp,direction="long",varying=list(ion),times=ion,timevar="ion")
    colnames(res_tmp)=c("product","subject","rep","time","duration","ion","intensity","id")
    res_tmp[,"time1"]=res_tmp[,"time"]-res_tmp[,"duration"]/2
    res_tmp[,"time2"]=res_tmp[,"time"]+res_tmp[,"duration"]/2

    tmp_bis=sweep(tmp[,-c(1:5)],1,apply(tmp[,-c(1:5)],1,sum),"/")
    tmp_bis[,c("product","subject","rep","time","duration")]=tmp[,c("product","subject","rep","time","duration")]
    res_sum=reshape(tmp_bis,direction="long",varying=list(ion),times=ion,timevar="ion")
    colnames(res_sum)=c("product","subject","rep","time","duration","ion","intensity","id")

      gg2 = gBasePlot(title=title, rows="subject + rep", cols="product") +
     geom_col(data=res_sum,aes(x=time,y=intensity,group=ion,fill=ion),width=res_sum[res_sum[,"ion"]!="sum","duration"])+theme_bw()+ggtitle("Divided by sum")
     gg1 = gBasePlot(title=title, rows="subject + rep", cols="product")+
       geom_col(data=res_tmp,aes(x=time,y=intensity,group=ion,fill=ion),width=res_sum[res_sum[,"ion"]!="sum","duration"])+theme_bw()+ggtitle("Original data")


      result=list()
      result[["output"]][["Legend"]]= gGetLegend(gg1); plot( result[["output"]][["Legend"]])
    result[["output"]][["normalized sequences"]]=gg1+theme(legend.position="none")
    result[["output"]][["sequences"]]=gg2+theme(legend.position="none")
    return (result)
  }

  ptrvListCorrelation=function(df,parameters)
  {
    tmp=dcast(df,product+subject+rep+time+duration~ion,value.var="intensity",fun.aggregate=mean)
    tmp[tmp<0]=0
    ions=unique(df[,"ion"])
    correl=cor(tmp[,ions])
    p=pheatmap(correl)
    return(p)
  }


  ptrvListBarplot=function(df, parameters) {
    if (is.null(parameters$rows)) { parameters$rows="." }
    if (is.null(parameters$cols)) { parameters$cols="." }
    if (is.null(parameters$confInt)) { parameters$confInt=0.95 }
    if (is.null(parameters$errorBars)) { parameters$errorBars="CI" }
    if (is.null(parameters$title)) { parameters$title="Barplot" }
    gg = gBarPlot(df, y="intensity", x="ion", group="product", fill="product", groupvars=c("product","ion"), confInt=parameters$confInt, errorBars=parameters$errorBars, facet="grid", rows=parameters$rows,cols=parameters$cols, wrap=NULL, title=parameters$title, labX = NULL, labY=NULL, grid=FALSE, labels=NULL, colors=NULL,scales="fixed",vnames=NULL)
    gg=gg+ theme(axis.text.x = element_text(angle = 90))


    result=list()
    result[["output"]][["barplot"]]=gg
    return (result)
  }

  ptrvListIntensityHeatmap=function(df,parameters)
  {
    result=list()
    if (is.null(parameters$rows)) { parameters$rows="." }
    if (is.null(parameters$cols)) { parameters$cols="." }
    dcdf=dcast(df,formula=product+subject+rep~ion,value.var="intensity",aggreage="mean")

    toHm=dcdf[,-which(colnames(dcdf)%in%c("product","subject","rep"))]
  #  toHm=as.matrix(toHm)
  #  toHm=sweep(toHm,1,apply(toHm,1,function(x){return(sum(x,na.rm=T))}),"/")
    rownames(toHm)=paste0("E",1:nrow(toHm))
    toHm[is.na(toHm)]=min(0,min(toHm,na.rm=T))
    annotation_row=data.frame(prod=factor(dcdf[,"product"]))

    # annotation_row=data.frame(prod=factor(dcdf[,"product"]),suj=factor(dcdf[,"subject"]))
    rownames(annotation_row)=rownames(toHm)

    nprod=length(unique(dcdf[,"product"]))
   # ann_colors=list(prod=rainbow(nprod))
    cols=rainbow(length(seq(-2,2,0.01)),start=0,end =0.8,s=0.2+0.8*1/4*abs(seq(-2,2,0.01))^2)
    ph=pheatmap(toHm,annotation_row=annotation_row,cluster_rows=F,clustering_distance_cols="correlation",cluster_cols=T,,cex=0.9,scale="column",breaks=seq(-2,2,0.01),color=cols,show_rownames=FALSE)
    ph
    result[["output"]][["pheatmap"]]=ph
    return(result)
  }


  parameters = list(...)

  if (title=="") {
    title=type
  }

  if("intensity"%in%colnames(df)){colnames(df)[colnames(df)=="intensity"]="score"}


  if (type == "Barplot of intensity") {
    df=x$totalIntensity

    if(log) { df[,"intensity"]=log(df[,"intensity"])    }
    if(normalizeByEval){df=normalizeByEval(df)$intens}
    result=runBy(df=df, runBy=runBy, selection=selection, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListBarplot, parameters=parameters)
  }

  if (type == "Curves") {
    df=x$time
    ionToUse=unique(df[,"ion"]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
    df=df[df[,"ion"]%in%ionToUse,]
    parameters$metaData=x$metaData
    if(log) { df[,"intensity"]=log(df[,"intensity"])}
    result=runBy(df=df, runBy=runBy, selection=selection, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListCurves, parameters=parameters)
  }

  if (type == "Scaled Curves") {
    df=x$time
    ionToUse=unique(df[,"ion"]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
    df=df[df[,"ion"]%in%ionToUse,]
    parameters$metaData=x$metaData
    if(log) { df[,"intensity"]=log(df[,"intensity"])}
    result=runBy(df=df, runBy=runBy, selection=selection, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListScaledCurves, parameters=parameters)
  }


  if (type == "Intensity distribution") {
    df=x$totalIntensity
    if(log) { df[,"intensity"]=log(df[,"intensity"])    }
    if(normalizeByEval){df=normalizeByEval(df)$intens}
    result=runBy(df=df, runBy=runBy, selection=selection, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListIntensityDistribution, parameters=parameters)
  }

  if (type == "Intensity heatmap") {
    df=x$totalIntensity
    if(log) { print("log"); df[,"intensity"]=log(df[,"intensity"])    }
    ionToUse=unique(df[,"ion"]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
    if(normalizeByEval){df=normalizeByEval(df,ionToUse)$intens}
    result=runBy(df=df, runBy=runBy, selection=selection, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListIntensityHeatmap, parameters=parameters)
  }

  if (type == "Individual sequences") {
    df=x$time
    result=runBy(df=df, runBy=runBy, selection=selection, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListSequences, parameters=parameters)
  }


  if (type=="ANOVA of intensities") {
      df=x$totalIntensity
      if(log) { df[,"intensity"]=log(df[,"intensity"])    }
      ionToUse=unique(df[,"ion"]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
      if(normalizeByEval){df=normalizeByEval(df,ionToUse)$intens}
      unit="ion"
     result=runBy(df=df, runBy=runBy, selection=selection, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListAnova, parameters=parameters)
  return(result)
      }

  if(type=="PCA of abundances")
  {
    df=x$totalIntensity
    if(log) { df[,"intensity"]=log(df[,"intensity"])    }
    ionToUse=unique(df[,"ion"]); ionToUse=ionToUse[!ionToUse%in%ionToRemove]
    if(normalizeByEval){df=normalizeByEval(df,ionToUse)$intens}
    parameters$option="correlation"
    parameters$title="Correlation PCA"
    result=runBy(df=df, runBy=runBy, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListPca, parameters=parameters)
  }
  if(type=="CVA of abundances")
  {
    df=x$totalIntensity
    if(log) { df[,"intensity"]=log(df[,"intensity"])    }
    if(normalizeByEval){df=normalizeByEval(df)$intens}

    result=runBy(df=df, runBy=runBy, outputFormat=outputFormat, outputFile=outputFile, title=title, fun=ptrvListCva, parameters=parameters)
  }
  return(result)
}
