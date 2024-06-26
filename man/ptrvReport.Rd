% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ptrvReport.r
\name{ptrvReport}
\alias{ptrvReport}
\title{Returns a report for a given dataset}
\usage{
ptrvReport(
  dataset,
  selecIons = "evolving",
  listIons = NULL,
  referenceBreath,
  smoothMethod = "MovingAverage",
  methodDetectStart = "startPeakProportion",
  noisePeriodIBT = c(0, 30),
  noisePeriodSig = c(0, 30),
  noisePeriodDS = c(0, 30),
  proportionOfMax = 0.3,
  halfWindowSize = 12,
  startPeriod = c(20, 60),
  detectingStart = FALSE,
  minimalDuration = 2,
  minExpi = NULL,
  maxInspi = NULL,
  forMinExpiDivideMaxIntBy = 4,
  forMaxInspiDivideMaxIntBy = 5
)
}
\arguments{
\item{dataset}{whose names are timeCol, Ion1,... ,Ionp.}

\item{selecIons}{if "evolving", the selected ions are the ones found as significant in ptrvSignificantSNRIons with noisePeriodSig and max method.If "namely", only the ions present in listIons are selected}

\item{listIons}{list of ions to be presented in the report}

\item{referenceBreath}{name of the ion taken as reference for breathing}

\item{smoothMethod}{NULL, "SavitzkyGolay" or "MovingAverage"}

\item{methodDetectStart}{method to detect start (see \link{ptrvDetectStart}). If numeric, the}

\item{noisePeriodIBT}{noise period used during intensity by time calculation}

\item{noisePeriodSig}{noise period used during significant ions calculation}

\item{noisePeriodDS}{noise period used during the start detection}

\item{proportionOfMax}{Number between 0 and 1. If method startPeakProportion is chosen and length(starts)==1, the starting time will be the first time that the intensity reaches this proportion of the maximale intensity of the peak. If length(starts) > 1, the starting time will be the minimum value obtained with this method for all starts.}

\item{halfWindowSize}{parameter used to determine the smooth for the peak-picking used in break detection}

\item{startPeriod}{Vector of two numbers. The starting time obtained has to be in this interval.}

\item{detectingStart}{boolean specifying wether the detect start procedure should be done (TRUE) or not (FALSE)}

\item{minimalDuration}{minimalDuration of a breathing cycle (2 by default - as a accelerated breathing frequency corresponds to 20 cycles and more by minut and a low frequency is 12 and less -)}

\item{minExpi}{minimal value for an expiry intensity to be detected as an expiry (blue dotted line in the plot).}

\item{maxInspi}{maximal intensity of the original curve required to be considered as a breathing peak (purple dotted line in the plot)}

\item{forMinExpiDivideMaxIntBy}{default to 5. When minExpi is null, it is evaluated as the ratio of the maximal intensity and forMinIntensityDivideMaxIntBy}

\item{forMaxInspiDivideMaxIntBy}{default to 5. When minExpi is null, it is evaluated as the ratio of the maximal intensity and forMinIntensityDivideMaxIntBy}
}
\value{
a list containing: the final data start0 (without breathing correction), start1 (with breathing correction) and a list of plots)
}
\description{
Returns a report for a given dataset
}
