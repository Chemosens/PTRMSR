#' @title Generic function
#' @description Generic function.
#' @param x Object of class "tds" or "fc" or "profile".
#' @param outputFormat Format of output, "" (no output), "html", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp, "svg" or "wmf" (windows only)
#' @param outputFile Name of the file (if outputFormat !="").
#' @param title Title of the output.
#' @param runBy Split x in several datasets corresponding to the combinations of each factor of runBy, then run a separate analysis for each dataset.
#' @param selection Run the analysis for a subset of x. Use syntax of sqldf.
#' @param ... Further parameters
#' @return A list, with elements depending on type (see details).
#' @export
analysis <- function(x,outputFormat="",outputFile="output",title="analysis",runBy="",selection=NULL,...) {
  UseMethod("analysis", x)
}