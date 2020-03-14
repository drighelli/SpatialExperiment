# 
setGeneric(name="addScaleFactors",
        def=function(ve, scaleFactors=NULL) standardGeneric("addScaleFactors"))

setGeneric(name="checkSpatialCoords", def=function(se, spatialCoords) 
    standardGeneric("checkSpatialCoords"))

setGeneric(name="checkVisiumSpatialCoords",
    def=function(ve, spatialCoords) 
    standardGeneric("checkVisiumSpatialCoords"))

#### Getters/Setters
setGeneric("scaleFactors", function(x, value) standardGeneric("scaleFactors"))

setGeneric("scaleFactors<-", function(x, value)
    standardGeneric("scaleFactors<-"))

setGeneric(name="spatialCoords", def=function(x) 
    standardGeneric("spatialCoords"))

setGeneric(name="spatialCoords<-", def=function(x, value) 
    standardGeneric("spatialCoords<-"))

setGeneric(name="isInTissue", def=function(x) standardGeneric("isInTissue"))

setGeneric(name="spatialCoordsNames", def=function(x) 
    standardGeneric("spatialCoordsNames"))
