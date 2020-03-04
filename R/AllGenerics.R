
setGeneric(name="addScaleFactors", valueClass="VisiumExperiment",
        def=function(ve, scaleFactors=NULL) standardGeneric("addScaleFactors"))

setGeneric("checkSpatialCoords", function(ve, spatialCoords) 
    standardGeneric("checkSpatialCoords"))


#### Getters/Setters

setGeneric("scaleFactors", function(x, value) standardGeneric("scaleFactors"))

setGeneric("scaleFactors<-", function(x, value) 
    standardGeneric("scaleFactors<-"))

setGeneric("spatialCoords", function(x) standardGeneric("spatialCoords"))

setGeneric("spatialCoords<-", function(x, value) 
    standardGeneric("spatialCoords<-"))

setGeneric("isInTissue", function(x) standardGeneric("isInTissue"))

setGeneric("spatialCoordsNames", function(x) 
    standardGeneric("spatialCoordsNames"))
