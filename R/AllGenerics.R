#' @export
setGeneric(name="addScaleFactors",
        def=function(ve, scaleFactors=NULL) standardGeneric("addScaleFactors"))
#' @export
setGeneric(name="checkSpatialCoords", def=function(se, spatialCoords) 
    standardGeneric("checkSpatialCoords"))
#' @export
setGeneric(name="checkVisiumSpatialCoords",
    def=function(ve, spatialCoords) 
    standardGeneric("checkVisiumSpatialCoords"))

#### Getters/Setters
#' @export
setGeneric("scaleFactors", function(x, value) standardGeneric("scaleFactors"))
#' @export
setGeneric("scaleFactors<-", function(x, value)
    standardGeneric("scaleFactors<-"))
#' @export
setGeneric(name="spatialCoords", def=function(x) 
    standardGeneric("spatialCoords"))
#' @export
setGeneric(name="spatialCoords<-", def=function(x, value) 
    standardGeneric("spatialCoords<-"))
#' @export
setGeneric(name="isInTissue", def=function(x) standardGeneric("isInTissue"))
#' @export
setGeneric(name="spatialCoordsNames", def=function(x) 
    standardGeneric("spatialCoordsNames"))
