#' @rdname SpatialExperiment
#' @exportClass SpatialExperiment SpatialExperiment
#' @importFrom SingleCellExperiment SingleCellExperiment
setClass("SpatialExperiment",
    contains="SingleCellExperiment")

#' @export
#' @importClassesFrom S4Vectors Annotated
setClass("VirtualSpatialImage", 
    contains=c("VIRTUAL", "Annotated"))

#' @export
setClass("LoadedSpatialImage", 
    contains="VirtualSpatialImage", 
    slots=c(image="ANY"))

#' @export
setClass("StoredSpatialImage", 
    contains="VirtualSpatialImage", 
    slots=c(path="character"))

#' @export
setClass("RemoteSpatialImage", 
    contains="VirtualSpatialImage", 
    slots=c(url="character"))
