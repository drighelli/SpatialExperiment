#' @rdname SpatialExperiment
#' 
#' @importFrom SingleCellExperiment SingleCellExperiment
setClass("SpatialExperiment",
        contains = "SingleCellExperiment"
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
setClass(
    Class="SpatialImage",
    contains="list",
    slots=c(
        grob="grobOrNULL",
        path="charOrNULL",
        url="charOrNULL"))

#' #' @export
#' #' @rdname VisiumExperiment
#' #' @slot scaleFactors list
#' #' @slot imagePaths list
#' setClass("VisiumExperiment",
#'     slots=c(
#'         scaleFactors="list",
#'         imagePaths="character"
#'     ),
#'     contains = "SpatialExperiment"
#' )