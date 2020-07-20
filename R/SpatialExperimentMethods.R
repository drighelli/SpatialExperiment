#' spatialCoords-getter
#' @description a getter method which returns the spatial coordinates previously
#' stored in a SpatialExperiment class object.
#' @param x A SpatialExperiment class object.
#'
#' @return a DataFrame within the spatial coordinates.
#'
#' @export
#' @aliases spatialCoords
#' @examples
#' example(SpatialExperiment)
#' spatialCoords(se)
setMethod(f="spatialCoords", signature="SpatialExperiment", function(x)
{
    return(int_colData(x)$spatial)
    
})

#' spatialCoords-setter
#' @description a setter method which sets/replaces the spatial coordinate in a
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object
#' @param value a DataFrame with the new spatial coordinates to set.
#' @return none
#' @importFrom SingleCellExperiment int_colData int_colData<-
#' @importFrom S4Vectors nrow SimpleList
#' @importFrom methods is
#' @aliases spatialCoords<-
#' @export
#' @examples
#' example(SpatialExperiment)
#' fakeFishCoords <- cbind(fishCoordinates[,c(1:3)], fishCoordinates[,3])
#'         colnames(fakeFishCoords) <- c("MyCell_ID", "Irrelevant", "x", "y")
#' spatialCoords(se) <- fakeFishCoords
#' spatialCoords(se)
setReplaceMethod(f="spatialCoords", signature="SpatialExperiment", function(x, value=DataFrame())
{
    # stopifnot(!sum(S4Vectors::isEmpty(value)))
    if(!is(value, "DataFrame")) 
    {
        value <- DataFrame(value)
    }
    if(sum(dim(value)==c(0,0))<2)
    {
        int_colData(x)$spatial <- value
    } else {
        int_colData(x)$spatial <- DataFrame(row.names=colnames(x))
    }
    

    return(x)
})


#' spatialCoordsNames-getter
#' @description getter method for the spatial coordinates names in a
#' SpatialExperiment class object.
#' @param x a SpatialExperiment class object.
#'
#' @return a vector with the colnames of the spatial coordinates.
#' @export
#' @aliases spatialCoordsNames
#' @examples
#' example(SpatialExperiment)
#' spatialCoordsNames(se)
setMethod(f="spatialCoordsNames", signature="SpatialExperiment", function(x)
{
    return(colnames(int_colData(x)$spatial))
})
