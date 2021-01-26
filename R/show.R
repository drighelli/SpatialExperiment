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
    grob <- imgGrob(object)
    path <- imgPath(object)
    url <- imgUrl(object)
    
    n <- sum(!c(is.null(grob), is.null(path), is.null(url)))
    cat(sprintf("A SpatialImage with %s source(s):\n", n))
    cat(sprintf("      >%s loaded\n", ifelse(is.null(grob), " not", "")))

    # TODO: better show method for path/URL?
    if (!is.null(path)) {
        if (nchar(path) > 60) {
            ss <- strsplit(path, "")[[1]]
            ss <- split(ss, ceiling(seq_along(ss)/50))
            pstr <- paste(
                lapply(ss, paste, collapse=""),
                collapse="\n      ")
        } else pstr <- path
    }
    if (!is.null(url)) {
        if (nchar(url) > 60) {
            ss <- strsplit(url, "")[[1]]
            ss <- split(ss, ceiling(seq_along(ss)/50))
            ustr <- paste(
                lapply(ss, paste, collapse=""),
                collapse="\n      ")
        } else ustr <- url
    }
    
    cat(sprintf("grob: %s\n", ifelse(is.null(grob), "NA", "Av")))
    cat(sprintf("path: %s\n", ifelse(is.null(path), "NA", pstr)))
    cat(sprintf(" url: %s\n", ifelse(is.null(url), "NA", ustr)))
}

#' @rdname SpatialImage-misc
setMethod("show", "SpatialImage", .spi_show)
