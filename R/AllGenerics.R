# imgData methods --------------------------------------------------------------

#' @export
setGeneric("imgData", function(x) standardGeneric("imgData"))

#' @export
setGeneric("imgData<-", function(x, value) standardGeneric("imgData<-"))

#' @export
setGeneric("imgRaster", function(x, ...) standardGeneric("imgRaster"))

#' @export
setGeneric("imgRaster<-", function(x, value) standardGeneric("imgRaster<-"))

#' @export
setGeneric("imgSource", function(x, ...) standardGeneric("imgSource"))

#' @export
setGeneric("imgSource<-", function(x, value) standardGeneric("imgSource<-"))

#' @export
setGeneric("getImg", function(x, ...) standardGeneric("getImg"))

#' @export
setGeneric("addImg", function(x, ...) standardGeneric("addImg"))

#' @export
setGeneric("rmvImg", function(x, ...) standardGeneric("rmvImg"))

#' @export
setGeneric("rotateImg", function(x, ...) standardGeneric("rotateImg"))

#' @export
setGeneric("mirrorImg", function(x, ...) standardGeneric("mirrorImg"))

# SpatialExperiment methods ----------------------------------------------------

#' @export
setGeneric("spatialCoords", function(x, ...) standardGeneric("spatialCoords"))

#' @export
setGeneric("spatialCoords<-", function(x, value) standardGeneric("spatialCoords<-"))

#' @export
setGeneric("spatialCoordsNames", function(x) standardGeneric("spatialCoordsNames"))

#' @export
setGeneric("spatialCoordsNames<-", function(x, value) standardGeneric("spatialCoordsNames<-"))

#' @export
setGeneric("scaleFactors", function(x, ...) standardGeneric("scaleFactors"))

#' @export
setGeneric("molecules", function(x, ...) standardGeneric("molecules"))

#' @export
setGeneric("molecules<-", function(x, ..., value) standardGeneric("molecules<-"))

# SpatialExperiment methods to rotate/mirror coordinates -----------------------

#' @export
setGeneric("rotateCoords", function(x, ...) standardGeneric("rotateCoords"))

#' @export
setGeneric("mirrorCoords", function(x, ...) standardGeneric("mirrorCoords"))

#' @export
setGeneric("rotateObject", function(x, ...) standardGeneric("rotateObject"))

#' @export
setGeneric("mirrorObject", function(x, ...) standardGeneric("mirrorObject"))
