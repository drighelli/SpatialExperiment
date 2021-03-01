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


# SpatialExperiment general methods --------------------------------------------

#' @export
setGeneric("scaleFactors", function(x, ...) standardGeneric("scaleFactors"))

#' @export
setGeneric("spatialCoords", function(x, ...) standardGeneric("spatialCoords"))

#' @export
setGeneric("spatialData", function(x, ...) standardGeneric("spatialData"))

#' @export
setGeneric("spatialData<-", function(x, value) standardGeneric("spatialData<-"))

#' @export
setGeneric("spatialDataNames", function(x) standardGeneric("spatialDataNames"))

#' @export
setGeneric("spatialCoordsNames", function(x) standardGeneric("spatialCoordsNames"))

#' @export
setGeneric("spatialCoordsNames<-", function(x, value) standardGeneric("spatialCoordsNames<-"))

#' @export
setGeneric("inTissue", function(x, ...) standardGeneric("inTissue"))

# SpatialExperiment assays methods ---------------------------------------------

#' @export
setGeneric("molecules", function(x, ...) standardGeneric("molecules"))

#' @export
setGeneric("molecules<-", function(x, ..., value) standardGeneric("molecules<-"))
