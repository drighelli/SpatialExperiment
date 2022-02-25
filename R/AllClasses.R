#' @rdname SpatialExperiment
#' @exportClass SpatialExperiment SpatialExperiment
#' @importFrom SingleCellExperiment SingleCellExperiment
setClass("SpatialExperiment",
    contains="SingleCellExperiment")

#' @export
setClass("VirtualSpatialImage", 
    contains="VIRTUAL")

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
