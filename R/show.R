#' @name SpatialExperiment-misc
#' 
#' @title Miscellaneous SpatialExperiment methods
#' 
#' @description
#' Miscellaneous methods for the \code{\link{SpatialExperiment}} class and its
#' descendants that do not fit into any other documentation category such as,
#' for example, show methods.
#' 
#' @param object a \code{\link{SpatialExperiment}} object
#' 
#' @return Returns NULL
#' 
#' @author Dario Righelli and Helena L. Crowell
#' 
#' @examples
#' example(read10xVisium)
#' spe
NULL

# SpatialExperiment show method ------------------------------------------------

#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
.spe_show <- function(object) {
    callNextMethod()
    coolcat("spatialData names(%d) : %s\n", spatialDataNames(object))
    coolcat("spatialCoords names(%d) : %s\n", spatialCoordsNames(object))
    coolcat("imgData names(%d): %s\n", names(imgData(object)))
}

#' @rdname SpatialExperiment-misc
setMethod("show", "SpatialExperiment", .spe_show)

# SpatialImage show method -----------------------------------------------------

#' @name SpatialImage-misc
#' @title Miscellaneous SpatialImage methods
#' 
#' @description
#' Miscellaneous methods for the \code{\link{SpatialImage}} class that do not
#' fit into any other documentation category such as, for example, show methods.
#' 
#' @param object a \code{SpatialImage} object
#' 
#' @return none
#' 
#' @author Helena L. Crowell
NULL

.spi_show <- function(object) {
    # dim <- paste(dim(object), collapse=" x ")
    dim <- as.character(dim(object))
    str <- paste0(dim[2], " x ", dim[1], " (width x height) ", 
                  class(object), "\n")
    cat(str)
    str <- imgSource(object)
    if (!is.na(str)) {
        if (nchar(str) > 80) {
            ss <- strsplit(str, "")[[1]]
            ss <- split(ss, ceiling(seq_along(ss)/80))
            str <- paste(lapply(ss, paste, collapse=""), collapse="\n  ")
        }
        cat("imgSource(): \n ", str, "\n")
    }
}

#' @rdname SpatialImage-misc
setMethod("show", "SpatialImage", .spi_show)
