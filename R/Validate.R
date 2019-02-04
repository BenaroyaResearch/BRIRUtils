#
# Validate functions
# Scott Presnell, SPresnell@benaroyaresearch.org
# February 4, 2019
#
#
#' Validate Library names/locations in a Dataframe
#'
#' validate standard RNA-seq data: counts, design, metrics, and summary files.
#'
#' @param counts counts dataframe
#' @param design design dataframe
#' @param countsField counts library field - usually column names
#' @param designField design library field - either the row names, or a column in the dataframe
#' @param metrics metrics dataframe (optional)
#' @param summary summary dataframe (optional)
#' @param metricsField metrics library field - either the row names, or a column in the dataframe
#' @param summaryField metrics library field - either the row names, or a column in the dataframe
#' 
#' @return boolean
#' 
#' @author Scott R Presnell, \email{SPresnell@@benaroyaresearch.org}
#' 
#' @export
#' 
validateLibraryFrame <- function(counts, design, countsField="colnames", designField="libraryId",
                                   metrics=NULL, summary=NULL, metricsField="libraryId", summaryField="libraryId") {
  
    validate_library_frame(counts=counts, design=design, countsField=countsField, designField=designField,
                           metrics=metrics, summary=summary, metricsField=metricsField, summaryField=summaryField)
}

#' Validate Library names/locations in a Dataframe
#'
#' validate standard RNA-seq data: counts, design, metrics, and summary files.
#'
#' @param counts counts dataframe
#' @param design design dataframe
#' @param countsField counts library field - usually column names
#' @param designField design library field - either the row names, or a column in the dataframe
#' @param metrics metrics dataframe (optional)
#' @param summary summary dataframe (optional)
#' @param metricsField metrics library field - either the row names, or a column in the dataframe
#' @param summaryField metrics library field - either the row names, or a column in the dataframe
#' 
#' @return boolean
#' 
#' @author Scott R Presnell, \email{SPresnell@@benaroyaresearch.org}
#' 
#' @export
validate_library_frame <- function(counts, design, countsField="colnames", designField="lib",
                                   metrics=metrics, summary=summary, metricsField="libid", summaryField="libid") {
  
  metricsNames <- NULL
  summaryNames <- NULL
  
  if (countsField == "colnames") {
    countsNames <- colnames(counts)
  } else if (countsField == "rownames")
    countsNames <- rownames(counts)
  
  if (designField == "rownames") {
    designNames <- rownames(design)
  } else {
    designNames <- design[[designField]]
  }

  if (!missing(metrics)) {
    if (metricsField == "rownames") {
      metricsNames <- rownames(metrics)
    } else {
      metricsNames <- metrics[[metricsField]]
    }
  }

  if (!missing(summary)) {
    if (summaryField == "rownames") {
      summaryNames <- rownames(summary)
    } else {
      summaryNames <- summary[[summaryField]]
    }
  }
  
  validate_library_names(countsNames, designNames, metricsNames, summaryNames)

}

#' Validate Library names/locations by name
#'
#' validate standard RNA-seq data: counts, design, metrics, and summary files.
#'
#' @param countsNames counts name list
#' @param designNames design name list
#' @param metricsNames metrics name list
#' @param summaryNames summary name list
#' 
#' @return boolean
#' 
#' @author Scott R Presnell, \email{SPresnell@@benaroyaresearch.org}
#' 
#' @export
validateLibraryNames <- function(countsNames, designNames, metricsNames = NULL, summaryNames = NULL) {
  
  validate_library_names(countsNames=countsNames, designNames=designNames,
                         metricsNames = metricsNames, summaryNames = summaryNames)

}

#' Validate Library names/locations by name
#'
#' validate standard RNA-seq data: counts, design, metrics, and summary files.
#'
#' @param countsNames counts name list
#' @param designNames design name list
#' @param metricsNames metrics name list
#' @param summaryNames summary name list
#' 
#' @return boolean
#' 
#' @author Scott R Presnell, \email{SPresnell@@benaroyaresearch.org}
#' 
#' @export
validate_library_names <- function(countsNames, designNames, metricsNames = NULL, summaryNames = NULL) {
  
  if (!all(countsNames == designNames)) {
    print("counts names:"); print(countsNames)
    print("design names:"); print(designNames)
    stop("library field in counts is not aligned with library field in design")
  }

  if (!all(countsNames == metricsNames)) {
    print("counts names:"); print(countsNames)
    print("metrics names:"); print(metricsNames)
    stop("library field in counts is not aligned with library field in metrics")
  }
  
  if (!all(countsNames == summaryNames)) {
    print("counts names:"); print(countsNames)
    print("summary names:"); print(summaryNames)
    stop("library field in counts is not aligned with library field in summary")
  }
  
  invisible(TRUE)

}