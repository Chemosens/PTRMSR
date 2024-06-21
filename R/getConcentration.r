#' @param h5file file .h5 to read
#' @param processedTransmission a datatable with two lines; the first one contains the m/z of the ions of the transmission to get a curve, the second one contain the transmission.
#' @param processedRawData  a data frame with as many columns as measured times and rows as ions (with names that are used in processedTransmission or processedMultipliers)
#' @param primaryMultipliers a vector whose names are names of ions that are rownames in processedRawData
#' @param ki reaction rate constant for reaction of R with H3O
#' @param V_0 normal volume in cm3 (default to 22400)
#' @param reducedIonMobility default to 2.8  cm^2V^(-1)s^(-1)
#' @export
getConcentration=function(h5file,nTimePoints=NULL,processed=TRUE,processedTransmission=NULL,processedRawData=NULL,primaryMultipliers=NULL,integrationTable=NULL,
                          ki=2*0.000000001, Ld=9.6,reducedIonMobility=2.8,V_0=22400   )
{

  if(processed){
    # Loading from PTRViewer
    processedPrimary=h5read(file=h5file,name="PROCESSED")$PrimaryIonSettings
    processedTransmission=h5read(file=h5file,name="PROCESSED")$Transmission$TransmissionData
    processedRawData=h5read(file=h5file,name="PROCESSED")$TraceData$RawData
    processedRawInfo=h5read(file=h5file,name="PROCESSED")$TraceData$RawInfo
    rownames(processedRawData)=round(as.numeric(processedRawInfo),digits=4)
    nameIons=rownames(processedRawData)
    primaryIons=as.character(round(as.numeric(processedPrimary$PIMasses),digits=4))
    primaryMultipliers=processedPrimary$PIMultipliers
    names(primaryMultipliers)=primaryIons
  }
  if(!processed)
  {
    if(!is.null(primaryMultipliers))
    {
      # print(rownames(processedRawData))
      # print(names(primaryMultipliers)%in%rownames(processedRawData))
      if(any(!names(primaryMultipliers)%in%rownames(processedRawData))){stop("primary ions are not colnames of processedRawData")}
    }
    nameIons=rownames(processedRawData)
  }
  # Loading from PTR data
  addTraces=h5read(file=h5file,name="AddTraces")
  Udrift=addTraces[["PTR-Reaction"]]$TwData[1,,]
  if(is.null(nTimePoints)){nTimePoints=dim(processedRawData)[2]}
  pDrift=addTraces[["PTR-Reaction"]]$TwData[2,,]
  tDrift=addTraces[["PTR-Reaction"]]$TwData[3,,]
  Udx=addTraces[["PTR-Instrument"]]$TwData[4,,]
  U=c(Udrift)[1:nTimePoints] + c(Udx)[1:nTimePoints]# drift voltage
  Td=c(tDrift)[1:nTimePoints]# drift tube temperature in degrees
  inPPBV=(10^9)
  #length of the drift tube in cm
  avogadroNumber=6.022e23
  T_0=273.15#in K
  pd=c(pDrift)[1:nTimePoints]#

  # product ion signal
  p_0=1013 #(mbar)


  #dealingWithPrimaryIons
  PI=rep(0,nTimePoints)
  # Transmission table
  mzIntegration=integrationTable[,"mz"];names(mzIntegration)=integrationTable[,"name"]
  transmission_table=spline(processedTransmission[1,],processedTransmission[2,],xout=mzIntegration,method='natural')
  transmission_v=transmission_table$y;
  names(transmission_v)=names(mzIntegration)
  # print(transmission_v)
  # print(names(primaryMultipliers))
  if(length(primaryMultipliers)>0)
  {
    for(i in 1:length(primaryMultipliers))
    {
      #  print("ta")
      #  print(processedRawData[names(primaryMultipliers)[i],])
      #  print("da")
      #  print(transmission_v[names(primaryMultipliers)[i]])
      # # print(processedRawData[names(primaryMultipliers)[i],])
      #  print("boum")
      #  print(primaryMultipliers[names(primaryMultipliers)[i]])
      PI=PI+processedRawData[names(primaryMultipliers)[i],]/transmission_v[names(primaryMultipliers)[i]]*primaryMultipliers[names(primaryMultipliers)[i]]
    }
  }else{PI=1}

  concentration_calculated=processedRawData
  for(k in 1:length(nameIons))
  {
    if(nameIons[k] %in% names(primaryMultipliers))
    {
      concentration_calculated[nameIons[k],]=processedRawData[nameIons[k],]/transmission_v[nameIons[k]]*primaryMultipliers[nameIons[k]]
    }
    else
    {
      RH=processedRawData[nameIons[k],]
      trRH=transmission_v[nameIons[k]]
      concentration_calculated[nameIons[k],]=(RH/trRH)*(1/PI)*inPPBV*U*
        (p_0^2*(T_0+Td)^2)/(pd^2*T_0^2)*
        (reducedIonMobility*V_0/(Ld^2*ki*avogadroNumber))
    }
  }
  return(concentration_calculated)
}


