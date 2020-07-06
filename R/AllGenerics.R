#  @export
# setGeneric(name="checkSpatialCoords", def=function(se, spatialCoords) 
#     standardGeneric("checkSpatialCoords"))
#  @export
# setGeneric(name="checkVisiumSpatialCoords",
#     def=function(ve, spatialCoords) 
#     standardGeneric("checkVisiumSpatialCoords"))

#### Getters/Setters
#' @export
setGeneric("getCellID", function(x) standardGeneric("getCellID"))
#' @export
setGeneric("scaleFactors", function(x, value) standardGeneric("scaleFactors"))
#' @export
setGeneric("scaleFactors<-", function(x, value)
    standardGeneric("scaleFactors<-"))
#' @export
setGeneric(name="spatialCoords", def=function(x) 
    standardGeneric("spatialCoords"))
#' @export
setGeneric("spatialCoords<-", function(x, value)
    standardGeneric("spatialCoords<-"))
#' @export
setGeneric("isInTissue", function(x) standardGeneric("isInTissue"))
#' @export
setGeneric("spatialCoordsNames", function(x) 
    standardGeneric("spatialCoordsNames"))
