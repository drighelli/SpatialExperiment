#' @importFrom S4Vectors coolcat
.se_show <- function(object) {
    callNextMethod()
    coolcat("spatialCoordinates(%d): %s\n", spatialCoordsNames(object))
}

#' @export
setMethod("show", "SpatialExperiment", .se_show)

.ve_show <- function(object) {
    callNextMethod()
    coolcat("inTissue(%d): %s\n", sum(isInTissue(object)))
}
#' @export
setMethod("show", "VisiumExperiment", .ve_show)
