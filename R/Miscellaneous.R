.se_show <- function(object) {
    callNextMethod()
    coolcat("spatialCoordinates(%d): %s\n", spatialCoordsNames(object))
}

#' SpatialExperiment show method
#' @description a method for showing the SpatialExperiment
#' @param object a SpatialExperiment object instance
#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
#' @return none
#' @aliases show
#' @examples
#' example(SpatialExperiment, echo=FALSE) # using class example
#' show(se)
setMethod("show", "SpatialExperiment", .se_show)

.ve_show <- function(object) {
    callNextMethod()
    coolcat("inTissue(%d): %s\n", sum(isInTissue(object)))
    coolcat("imagePaths(%d): %s\n", imagePaths(object))
}

#' VisiumExperiment show method
#' @description a method for showing the VisiumExperiment
#' @param object a VisiumExperiment object instance
#' @importFrom S4Vectors coolcat
#' @importFrom methods callNextMethod
#' @return none
#' @aliases .ve_show
#' @examples 
#' example(VisiumExperiment, echo=FALSE) #using class example
#' show(ve)
setMethod("show", "VisiumExperiment", .ve_show)
