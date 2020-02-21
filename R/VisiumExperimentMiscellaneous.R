#' @importFrom S4Vectors coolcat
.ve_show <- function(object) {
    callNextMethod()
    coolcat("spatialCoordinates(%d): %s\n", spatialCoordsNames(object))
    coolcat("reducedDimNames(%d): %s\n", reducedDimNames(object))
    coolcat("spikeNames(%d): %s\n", suppressWarnings(spikeNames(object)))
    coolcat("altExpNames(%d): %s\n", altExpNames(object))
}

#' @export
setMethod("show", "VisiumExperiment", .ve_show)
