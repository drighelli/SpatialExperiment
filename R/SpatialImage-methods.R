## For documentation see imgData-methods
## author Helena L. Crowell


# getters ----------------------------------------------------------------------


#' @rdname imgData-methods
#' @export
setMethod("imgPath", "SpatialImage", function(x) x@path)

#' @rdname imgData-methods
#' @export
setMethod("imgGrob", "SpatialImage", function(x) x@grob)


#' @rdname imgData-methods
#' @export
setMethod("imgUrl", "SpatialImage", function(x) x@url)

# setters ----------------------------------------------------------------------


#' @rdname imgData-methods
#' @export
setReplaceMethod("imgPath", c("SpatialImage", "character"), 
                 function(x, value) 
                 { 
                     .path_validity(value)
                     x@path <- value
                     return(x) 
                 })

#' @rdname imgData-methods
#' @importFrom grid grob
#' @export
setReplaceMethod("imgGrob", c("SpatialImage", "rastergrob"), 
    function(x, value) { x@grob <- value; return(x) })


#' @rdname imgData-methods
#' @export
setReplaceMethod("imgUrl", c("SpatialImage", "character"), 
    function(x, value) 
    { 
        .url_validity(value)
        x@url <- value
        return(x) 
    })

# loadImage --------------------------------------------------------------------

#' @rdname imgData-methods
#' @export
setMethod("loadImg", "SpatialImage",
    function(x) { .load_img(x)$data[[1]] })

# unloadImg --------------------------------------------------------------------

# TODO: How to handle cached images?
# add a flag cached=TRUE/FALSE in the class,
# and drop path if cached=TRUE?

#' @rdname imgData-methods
#' @export
setMethod("unloadImg", "SpatialImage",
    function(x) { x@grob <- NULL; return(x) })
