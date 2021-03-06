#' @name SpatialExperiment-misc
#' @title Miscellaneous \code{SpatialExperiment} methods
#' 
#' @description 
#' Miscellaneous methods for the \code{\link{SpatialExperiment}} class
#' and its descendants that do not fit in any other documentation category
#' such as, for example, show methods.
#' 
#' @param object a \code{SpatialExperiment}.
#' 
#' @return none.
#' 
#' @author Dario Righelli & Helena L. Crowell

# SpatialExperiment show method ------------------------------------------------

#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
.spe_show <- function(object) {
    callNextMethod()
    coolcat("spatialData names(%d) : %s\n", spatialDataNames(object))
    coolcat("imgData names(%d): %s\n", names(imgData(object)))
    
}

#' @rdname SpatialExperiment-misc
setMethod("show", "SpatialExperiment", .spe_show)

# SpatialImage show method -----------------------------------------------------

#' @name SpatialImage-misc
#' @title Miscellaneous \code{SpatialImage} methods
#' 
#' @description 
#' Miscellaneous methods for the \code{\link{SpatialImage}} class that do not 
#' fit in any other documentation category such as, for example, show methods.
#' 
#' @param object a \code{SpatialImage}.
#' 
#' @return none.
#' 
#' @author Helena L. Crowell

.spi_show <- function(object) {
    dim <- paste(dim(object), collapse=" x ")
    cat("A", dim, class(object), "\n")
    str <- imgSource(object)
    if (!is.na(str)) {
        if (nchar(str) > 50) {
            ss <- strsplit(str, "")[[1]]
            ss <- split(ss, ceiling(seq_along(ss)/40))
            str <- paste(
                lapply(ss, paste, collapse=""),
                collapse="\n  ")
        }
        cat("imgSource(): \n ", str)
    }
}

#' @rdname SpatialImage-misc
setMethod("show", "SpatialImage", .spi_show)
