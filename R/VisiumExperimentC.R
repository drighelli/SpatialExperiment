
setClass("VisiumExperiment",
         slots=c(
             scaleFactors="list",
             int_spcIdx="integer"
         ),
         contains = "SingleCellExperiment"#,
         # prototype = prototype(
         #     int_metadata=list(
         #         version=packageVersion("VisiumExperiment")
         #     )
         # )
)


VisiumExperiment <- function(..., spatialCoords=data.frame(), 
                             scaleFactors=list())
{
    args <- list(...)
    stopifnot(sum(c("rowData", "colData", "assays") %in% names(args)) == 3)
    sce <- SingleCellExperiment::SingleCellExperiment(
        rowData=as(args$rowData, "DataFrame"),
        colData=as(args$colData, "DataFrame"),
        assays=args$assays)
    return(.sce_to_ve(sce, spatialCoords=spatialCoords, 
                      scaleFactors=scaleFactors))
}

.sce_to_ve <- function(sce, spatialCoords=data.frame(), scaleFactors=list()) 
{
    ve <- new("VisiumExperiment", sce)
    
    .Object <- checkSpatialCoords(ve, spatialCoords)
    .Object <- addScaleFactors(.Object, scaleFactors)
    return(.Object)
}

setAs("SingleCellExperiment", "VisiumExperiment", function(from) 
{
    .sce_to_ve(from)
})