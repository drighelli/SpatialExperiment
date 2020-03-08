#' @importFrom S4Vectors coolcat
.se_show <- function(object) {
    callNextMethod()
    coolcat("spatialCoordinates(%d): %s\n", spatialCoordsNames(object))
    coolcat("reducedDimNames(%d): %s\n", reducedDimNames(object))
    coolcat("spikeNames(%d): %s\n", suppressWarnings(spikeNames(object)))
    coolcat("altExpNames(%d): %s\n", altExpNames(object))
}

#' @export
setMethod("show", "SpatialExperiment", .se_show)
