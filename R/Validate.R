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
#' @param metrics metrics dataframe (optional)
#' @param summary summary dataframe (optional)
#' @param countsField counts library field - usually column names
#' @param designField design library field - either the row names, or a column in the dataframe
#' @param metricsField metrics library field - either the row names, or a column in the dataframe
#' @param summaryField metrics library field - either the row names, or a column in the dataframe
#' 
#' @return boolean - side effect of stopping if names are not aligned.
#' 
#' @author Scott R Presnell, \email{SPresnell@@benaroyaresearch.org}
#' 
#' @examples
#' \donttest{
#' counts  <- read.csv(file="data/P239-1_CCLKLANXX_180806_combined_counts.csv", row.names=1)
#' counts  <- counts[,order(colnames(counts))]
#' design  <- read.csv(file="data/P239-1 Final Adjusted Annotation.csv", stringsAsFactors = F)
#' libpattern <- "lib[0-9]+"
#' colnames(counts)  <- str_extract(colnames(counts), libpattern)
#' validateLibraryFrame(counts, design)
#' }
#' @export
#' 
validateLibraryFrame <- function(counts, design, metrics=NULL, summary=NULL,
                                 countsField="colnames", designField="libraryId",
                                 metricsField="libraryId", summaryField="libraryId") {
  
    validate_library_frame(counts=counts, design=design, metrics=metrics, summary=summary,
                           countsField=countsField, designField=designField,
                           metricsField=metricsField, summaryField=summaryField)
}

#' Validate Library names/locations in a Dataframe
#'
#' validate standard RNA-seq data: counts, design, metrics, and summary files.
#'
#' @param counts counts dataframe
#' @param design design dataframe
#' @param metrics metrics dataframe (optional)
#' @param summary summary dataframe (optional)
#' @param countsField counts library field - usually column names
#' @param designField design library field - either the row names, or a column in the dataframe
#' @param metricsField metrics library field - either the row names, or a column in the dataframe
#' @param summaryField metrics library field - either the row names, or a column in the dataframe
#' 
#' @return boolean
#' 
#' @author Scott R Presnell, \email{SPresnell@@benaroyaresearch.org}
#' 
#' @export
validate_library_frame <- function(counts, design, metrics=NULL, summary=NULL,
                                   countsField="colnames", designField="libid",
                                   metricsField="libid", summaryField="libid") {
  
  metricsNames <- NULL
  summaryNames <- NULL

  if (countsField == "colnames") {
    countsNames <- colnames(counts)
  } else if (countsField == "rownames") {
    countsNames <- rownames(counts)
  } else {
    countsNames <- counts[[countsField]]
  }
    
  if (designField == "rownames") {
    designNames <- rownames(design)
  } else {
    designNames <- design[[designField]]
  }

  if (!is.null(metrics)) {
    if (metricsField == "rownames") {
      metricsNames <- rownames(metrics)
    } else {
      metricsNames <- metrics[[metricsField]]
    }
  }

  if (!is.null(summary)) {
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
  
  if (!all(countsNames == designNames) || is.null(countsNames) || is.null(designNames) ||
        is.na(countsNames) || is.na(designNames)) {
    print("counts names:"); print(countsNames)
    print("design names:"); print(designNames)
    stop("library field in counts is not aligned with library field in design")
  }

  if (!is.null(metricsNames)) {
    if (!all(countsNames == metricsNames) || is.null(countsNames) || is.null(metricsNames) ||
        is.na(countsNames) || is.na(metricsNames)) {
      print("counts names:"); print(countsNames)
      print("metrics names:"); print(metricsNames)
      stop("library field in counts is not aligned with library field in metrics")
    }
  }
  
  if (!is.null(summaryNames)) {
    if (!all(countsNames == summaryNames)  || is.null(countsNames) || is.null(summaryNames) ||
        is.na(countsNames) || is.na(metricsNames)) {
      print("counts names:"); print(countsNames)
      print("summary names:"); print(summaryNames)
      stop("library field in counts is not aligned with library field in summary")
    }
  }
  
  invisible(TRUE)

}