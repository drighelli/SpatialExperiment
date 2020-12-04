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

#' @export
setGeneric("scaleFactors", function(x, ...) standardGeneric("scaleFactors"))

#' #' @export
#' setGeneric("scaleFactors<-", function(x, value)
#'     standardGeneric("scaleFactors<-"))

#' @export
setGeneric(name="spatialCoords", def=function(se, ...)
    standardGeneric("spatialCoords"))

#' @export
setGeneric(name="spatialCoordsMtx", def=function(se, ...)
    standardGeneric("spatialCoordsMtx"))

#' @export
setGeneric("spatialCoords<-", function(se, coords, ...)
    standardGeneric("spatialCoords<-"))
#' #' @export
#' setGeneric("isInTissue", function(x) standardGeneric("isInTissue"))
#' #' @export
#' setGeneric("spatialCoordsNames", function(x) 
#'     standardGeneric("spatialCoordsNames"))
#' #' @export
#' setGeneric(name="imagePaths", def=function(x) standardGeneric("imagePaths"))
#' #' @export
#' setGeneric("imagePaths", function(x, value) standardGeneric("imagePaths"))
#' #' @export
#' setGeneric("imagePaths<-", function(x, value)
#'     standardGeneric("imagePaths<-"))
