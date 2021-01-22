#' @title 
#' Named assay getters and setters
#'
#' @description
#' These are methods for getting or setting \code{assay(se, i=X, ...)} 
#' where \code{se} is a \linkS4class{SpatialExperiment} object and \code{X} is 
#' the name of the method.
#' For example, \code{molecules} will get or set \code{X="molecules"}.
#' This provides some convenience for users as well as encouraging standardization 
#' of assay names across packages.
#'
#' @section Available methods:
#' In the following code snippets, \code{x} is a \linkS4class{SpatialExperiment} 
#' object, \code{value} is a BumpyMatrix-like object with the same 
#' dimensions as \code{x}, and \code{...} are further arguments passed to 
#' \code{\link{assay}} (for the getter) or \code{\link{assay<-}} (for the setter).
#' 
#' \describe{
#' \item{\code{molecules(x, ...)}, \code{molecules(x, ...) <- value}:}{
#' Get or set a BumpyMatrix of raw count data, e.g., number of reads or transcripts.
#' }
#' }
#' 
#' @author
#' Dario Righelli
#' 
#' @seealso
#' \code{\link{assay}} and \code{\link{assay<-}}, for the wrapped methods.
#' 
#' @examples
#' 
#' 
#' 
#' @name SparialExperiment-assays
#' @rdname SpatialExperiment-assays
#' @docType methods
#' @aliases
#' molecules
#' molecules<-
#' molecules,SingleCellExperiment-method
#' molecules<-,SingleCellExperiment-method
NULL


GET_FUN <- function(exprs_values, ...) {
    (exprs_values) # To ensure evaluation
    function(x, ...) {
        assay(x, i=exprs_values, ...)
    }
}

SET_FUN <- function(exprs_values, ...) {
    (exprs_values) # To ensure evaluation
    function(x, ..., value) {
        assay(x, i=exprs_values, ...) <- value
        return(x)
    }
}

#' @export
setMethod("molecules", "SpatialExperiment", GET_FUN("molecules"))


#' @export
#' @importFrom BumpyMatrix BumpyMatrix
setReplaceMethod("molecules", c("SingleCellExperiment", "BumpyMatrix"), SET_FUN("molecules"))
