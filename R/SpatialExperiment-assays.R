#' @name SpatialExperiment-assays
#' 
#' @title Methods for named assays
#' 
#' @aliases
#' molecules
#' molecules<-
#' molecules,SpatialExperiment-method
#' molecules<-,SpatialExperiment-method
#' 
#' @description
#' The \code{\link{SpatialExperiment}} class provides methods for getting or
#' setting named \code{\link{assays}}. For example, \code{molecules(spe)} will
#' get or set an \code{assay} named \code{molecules} from object \code{spe},
#' equivalent to \code{assay(spe, i = "molecules")}. This provides a convenient
#' interface for users and encourages standardization of assay names across
#' packages.
#' 
#' @section Available methods:
#' In the following code, \code{spe} is a \code{\link{SpatialExperiment}}
#' object, \code{value} is a \code{BumpyMatrix}-like object with the same
#' dimensions as \code{spe}, and \code{...} are further arguments passed to
#' \code{\link{assay}} (for the getter) or \code{\link{assay<-}} (for the
#' setter).
#' 
#' \describe{
#' \item{\code{molecules(x, ...)}, \code{molecules(x, ...) <- value}:}{
#' Get or set an assay named \code{molecules}, which is usually assumed to be a
#' \code{BumpyMatrix}-formatted object containing spatial coordinates (and any
#' other information) of the individual molecules per gene per cell.
#' }
#' }
#' 
#' @author Dario Righelli
#' 
#' @seealso
#' \code{\link{assay}} and \code{\link{assay<-}}
#' 
#' @examples
#' example(SpatialExperiment)
#' molecules(spe_mol)
NULL

#' @importFrom SummarizedExperiment assay
GET_FUN <- function(exprs_values, ...) {
    (exprs_values) # To ensure evaluation
    function(x, ...) {
        assay(x, i=exprs_values, ...)
    }
}

#' @importFrom SummarizedExperiment assay<-
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
setReplaceMethod("molecules", c("SingleCellExperiment", "ANY"), SET_FUN("molecules"))
