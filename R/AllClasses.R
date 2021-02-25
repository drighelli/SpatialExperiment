#' @rdname SpatialExperiment
#' @importFrom SingleCellExperiment SingleCellExperiment
setClass("SpatialExperiment",
        contains="SingleCellExperiment",
        slots=c(
            spatialData="DataFrame",
            spaCoordsNms="character"))

#' @export
setClass("SpatialImage", 
    contains="VIRTUAL")

#' @export
setClass("LoadedSpatialImage", 
    contains="SpatialImage", 
    slots=c(image="ANY"))

#' @export
setClass("StoredSpatialImage", 
    contains="SpatialImage", 
    slots=c(path="character"))

#' @export
setClass("RemoteSpatialImage", 
    contains="SpatialImage", 
    slots=c(url="character"))
