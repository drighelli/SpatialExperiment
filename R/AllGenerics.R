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
setGeneric("spatialCoords<-", function(x, value)
    standardGeneric("spatialCoords<-"))
#' @export
setGeneric("isInTissue", function(x) standardGeneric("isInTissue"))
#' @export
setGeneric("spatialCoordsNames", function(x) 
    standardGeneric("spatialCoordsNames"))
#' @export
setGeneric(name="imagePaths", def=function(x) standardGeneric("imagePaths"))
#' @export
setGeneric("imagePaths", function(x, value) standardGeneric("imagePaths"))
#' @export
setGeneric("imagePaths<-", function(x, value)
    standardGeneric("imagePaths<-"))
