#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
.se_show <- function(object) {
    callNextMethod()
    coolcat("spatialCoordinates(%d): %s\n", spatialCoordsNames(object))
}

#' @export
setMethod("show", "SpatialExperiment", .se_show)

#' @importFrom methods callNextMethod
.ve_show <- function(object) {
    callNextMethod()
    coolcat("inTissue(%d): %s\n", sum(isInTissue(object)))
}
#' @export
setMethod("show", "VisiumExperiment", .ve_show)
