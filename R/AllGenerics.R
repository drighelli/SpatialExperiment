# imgData methods --------------------------------------------------------------

#' @export
setGeneric("imgData", function(x) standardGeneric("imgData"))
#' @export
setGeneric("imgData<-", function(x, value) standardGeneric("imgData<-"))

#' @export
setGeneric("imgGrob", function(x, ...) standardGeneric("imgGrob"))
#' @export
setGeneric("imgGrob<-", function(x, value) standardGeneric("imgGrob<-"))

#' @export
setGeneric("imgPath", function(x, ...) standardGeneric("imgPath"))
#' @export
setGeneric("imgPath<-", function(x, value) standardGeneric("imgPath<-"))

#' @export
setGeneric("imgUrl", function(x, ...) standardGeneric("imgUrl"))
#' @export
setGeneric("imgUrl<-", function(x, value) standardGeneric("imgUrl<-"))

#' @export
setGeneric("loadImg", function(x, ...) standardGeneric("loadImg"))

#' @export
setGeneric("unloadImg", function(x, ...) standardGeneric("unloadImg"))

#' @export
setGeneric("addImg", function(x, ...) standardGeneric("addImg"))

#' @export
setGeneric("removeImg", function(x, ...) standardGeneric("removeImg"))


# SpatialExperiment defaults methods getters -----------------------------------

#' @export
setGeneric("scaleFactors", function(x, ...) standardGeneric("scaleFactors"))

# #' @export
# setGeneric("scaleFactors<-", function(x, value)
#     standardGeneric("scaleFactors<-"))
#

#' @export
setGeneric(name="spatialCoords", def=function(se, ...)
    standardGeneric("spatialCoords"))

#' @export
setGeneric(name="spatialData", def=function(se, ...)
    standardGeneric("spatialData"))

#' @export
setGeneric("spatialData<-", function(x, value)
    standardGeneric("spatialData<-"))

#' @export
setGeneric("spatialCoordsNames<-", function(x, value)
    standardGeneric("spatialCoordsNames<-"))

#' @export
setGeneric("spatialCoordsNames", function(x)
    standardGeneric("spatialCoordsNames"))

#' @export
setGeneric("inTissue", function(x, ...) standardGeneric("inTissue"))

#' @export
setGeneric("spatialDataNames", function(x) standardGeneric("spatialDataNames"))

# SpatialExperiment assays methods ---------.-----------------------------------
#' @export
setGeneric("molecules", function(x, ...) standardGeneric("molecules"))

#' @export
setGeneric("molecules<-", function(x, ..., value) standardGeneric("molecules<-"))
