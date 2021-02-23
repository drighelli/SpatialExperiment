#' @rdname SpatialExperiment
#' @importFrom SingleCellExperiment SingleCellExperiment
setClass("SpatialExperiment",
        contains = "SingleCellExperiment",
        slots=c(
            spatialData="DataFrame",
            spaCoordsNms="character"
        )
)

#' @importFrom grid grob
#' @importFrom methods setOldClass
setOldClass(
    Classes="rastergrob")

#' @importFrom methods setClassUnion
setClassUnion(
    name="grobOrNULL",
    members=c("rastergrob", "NULL"))

#' @importFrom methods setClassUnion
setClassUnion(
    name="charOrNULL",
    members=c("character", "NULL"))

#' @export
setClass(Class="SpatialImage", contains="VIRTUAL")

#' @export
setClass("LoadedSpatialImage", contains="SpatialImage", slots=c(image="ANY"))

#' @export
setClass("FileSpatialImage", contains="SpatialImage", slots=c(path="character"))

#' @export
setClass("RemoteSpatialImage", contains="SpatialImage", slots=c(url="character"))
