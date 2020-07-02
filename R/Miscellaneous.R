#' show
#' @description a method for showing the SpatialExperiment
#' @param object a SpatialExperiment object instance
#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
#' @aliases show
#' @examples
#' example(SpatialExperiment, echo=FALSE) # using class example
#' show(se)
.se_show <- function(object) {
    callNextMethod()
    coolcat("spatialCoordinates(%d): %s\n", spatialCoordsNames(object))
}

#' @export
setMethod("show", "SpatialExperiment", .se_show)

#' show
#' @description a method for showing the VisiumExperiment
#' @param object a VisiumExperiment object instance
#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
#' @aliases show
#' @examples 
#' example(VisiumExperiment, echo=FALSE) #using class example
#' show(ve)
.ve_show <- function(object) {
    callNextMethod()
    coolcat("inTissue(%d): %s\n", sum(isInTissue(object)))
}

#' @export
setMethod("show", "VisiumExperiment", .ve_show)
