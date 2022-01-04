#' @title ptrReadListIBM
#' This function aims to read a list of files of PTR type and to return an ibm object containing a dataframe with "file", "mz","intensity","file" columns
#' @param listFiles vector of characters representing the names of the files to be loaded
#' @param sumSpectraOnly TRUE by default. Faster if no time is required in the analysis.
#' @inheritParams ptrIntensityByMass
#' @param normalization "none" # TODO
#' @return a ibm object
#' @export 
#' @examples 
#' # files <- dir(system.file(package = "chemosensR", dir = "extdata"),full.name=TRUE,pattern="h5$")
#' # ptr_ibm=ptrReadListIBM(files)
ptrReadListIBM=function(listFiles,sumSpectraOnly=TRUE,normalization="none",rt=NULL,integrationTable=NULL,breaks=NULL,by=0.1,type="sumSpectrum")
{
  df=NULL
  #if(sumSpectraOnly&is.null(rt)){typeForPibm="sumSpectrum"}else{typeForPibm="full"}
  for(i in 1:length(listFiles))
  {
      print(paste0(listFiles[i],"(",i,"/",length(listFiles),")"))
      ptr=ptrRead(file=listFiles[i],sumSpectraOnly=sumSpectraOnly)
      print("Reading OK.")
      ag=ptrIntensityByMass(ptr,type=type,integrationTable=integrationTable,breaks=breaks,by=by,rt=rt)
      dfi=ag$df
      dfi[,"file"]=rep(listFiles[i],dim(dfi)[1])
      df=rbind(df,dfi)
  }
  return(df)
}