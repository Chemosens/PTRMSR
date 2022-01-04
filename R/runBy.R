#' @title Run analyses by batch.
#' @description Run analyses by batch.
#' @param df Dataframe.
#' @param outputFormat Format of output, "" (no output), "html", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp, "svg" or "wmf" (windows only)
#' @param outputFile Name of the file (if outputFormat !="").
#' @param title Title of the output.
#' @param runBy Split x in several datasets corresponding to the combinations of each factor of runBy, then run a separate analysis for each dataset.
#' @param selection Run the analysis for a subset of x. Use syntax of sqldf.
#' @param fun Function
#' @param parameters Parameters for fun.
#' @return List.
#' @export
runBy=function(df, runBy="", selection="", outputFormat="", outputFile="", title="", fun, parameters=list()) {
  
  # Analyses groupées par runBy
  listDf=list()
  
  if (runBy != "") {
    combBy=list()
    for (cc in runBy) {
      combBy[[cc]]=unique(df[,cc])
    }
    combBy=as.data.frame(expand.grid(combBy))
    
    for(i in 1:nrow(combBy)) {
      d=df
      for(j in 1:ncol(combBy)) {
        d=d[d[,runBy[j]]==combBy[i,j],]
      }
      combName=paste(colnames(combBy), combBy[i,],collapse=", ")
      
      if (selection !="") {
        d=sqldf::sqldf(x = paste("SELECT * FROM d WHERE",selection))
      }

      listDf[[combName]]=d
    }
  } else {
    if (selection !="") {
      df=sqldf::sqldf(x = paste("SELECT * FROM df WHERE",selection))
    }
    
    listDf[[1]]=df
  }
  

  #result = lapply(listDf, fun, parameters)
  # TODO : gestion des , si byrep=""
   result = lapply(seq_along(listDf), function(listDf, i) { 
    if (length(listDf)>1) {
      parameters[["title"]]<<-paste(title, names(listDf)[i], sep=", ")
    }
    else {
      parameters[["title"]]<<-title
    }
    fun(listDf[[i]], parameters)
  }, listDf=listDf)
  
  for (i in 1:length(result)) {
    for (j in 1:length(result[[i]][["output"]])) {
      output=result[[i]][["output"]][[j]]
      
      if (outputFormat != "" && outputFile != "") 
      {
        outputFileName=paste(outputFile,"_",i,"_",names(result[[i]][["output"]])[j],sep="")
        
        if ("ggplot" %in% class(output)) {
          gSavePlot(output, format=outputFormat, fileName=outputFileName)
        }
        if ("kableExtra" %in% class(output)) {
          kableExtra::save_kable(output,paste(outputFileName,".",outputFormat,sep=""))
        }
      }
      if ("ggplot" %in% class(output)) {
        #X11()
        grDevices::dev.new()
      }
      print(output)
    }
  }
  
  return (result)
  
}