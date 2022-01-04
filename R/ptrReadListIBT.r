#' This function aims to read a list of files of PTR type and to return an ibm object containing a dataframe with "name", "mz","intensity","file" columns
#' @param listFiles vector of characters representing the names of the files to be loaded
#' @param sumSpectraOnly TRUE by default. Faster if no time is required in the analysis.
#' @inheritParams ptrIntensityByTime
#' @return a ibt object
#' @export 
ptrReadListIBT=function(listFiles,sumSpectraOnly=TRUE,mz=NULL,rt=NULL,integrationTable=NULL)
{
  df=NULL
  if(sumSpectraOnly){typeForPibm="sumSpectrum"}
  for(i in 1:length(listFiles))
  {  # h5closeAll()
      print(paste0(i,"/",length(listFiles)))
      ptr=ptrRead(file=listFiles[i],sumSpectraOnly=FALSE)
      print("Reading OK.")
      ag=ptrIntensityByTime(ptr,integrationTable=integrationTable,mz=mz,rt=rt)
      dfi=ag$df
      dfi[,"file"]=rep(listFiles[i],dim(dfi)[1])
      df=rbind(df,dfi)
  }
  return(df)
}