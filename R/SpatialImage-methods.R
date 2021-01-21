#' @name SpatialImage-methods
#' @title SpatialImage methods
#' @aliases imgGrob imgPath imgUrl imgGrob<- imgPath<- imgUrl<- 
#' 
#' @description 
#' Getters, setters and additional methods 
#' for the \code{\link{SpatialImage}} class.
#' 
#' @param x a \code{\link{SpatialImage}}
#' @param value a \code{grob} for \code{imgGrob}, 
#'   a character string for \code{imgPath/Url}
#' 
#' @details
#' In the following examples, \code{x} is a \code{\link{SpatialImage}}.
#' \describe{
#' \item{getters & setters:}{
#' \itemize{
#' \item{\code{imgGrob(x), imgGrob(x) <- value} \cr gets/sets the image's \code{grob}}
#' \item{\code{imgPath(x), imgPath(x) <- value} \cr gets/sets the image's file path}
#' \item{\code{imgUrl(x), imgUrl(x) <- value} \cr gets/sets the image's source URL}
#' }  
#' }
#' \item{image methods:}{
#' \itemize{
#' \item{\code{loadImg(x)} loads an image as a \code{grob} from its path or URL}
#' \item{\code{unloadImg(x)} unloads an image by dropping the \code{grob}}
#' } 
#' }
#' }
#' 
#' @examples
#' # construct 'SpatialImage'
#' dir <- file.path("extdata", "10xVisium", "section1", "spatial")
#' fnm <- file.path(dir, "tissue_lowres_image.png")
#' imgPath <- system.file(fnm, package = "SpatialExperiment")
#' (img <- SpatialImage(path = imgPath))
#' 
#' # load image as a 'grob'
#' img <- loadImg(img)
#' 
#' # accessors
#' imgGrob(img)
#' imgPath(img)
#' imgUrl(img)
#' 
#' # draw the image
#' grb <- imgGrob(img)
#' grid::grid.draw(grb)
#' 
#' # unload it, i.e. the 'grob'
#' img <- unloadImg(img)
#' imgGrob(img)
#'   
#' @author Helena L. Crowell

# getters ----------------------------------------------------------------------

#' @rdname SpatialImage-methods
#' @export
setMethod("imgGrob", "SpatialImage", function(x) x@grob)

#' @rdname SpatialImage-methods
#' @export
setMethod("imgPath", "SpatialImage", function(x) x@path)

#' @rdname SpatialImage-methods
#' @export
setMethod("imgUrl", "SpatialImage", function(x) x@url)

# setters ----------------------------------------------------------------------

#' @rdname SpatialImage-methods
#' @importFrom grid grob
#' @export
setReplaceMethod("imgGrob", c("SpatialImage", "rastergrob"), 
    function(x, value) { x@grob <- value; return(x) })

#' @rdname SpatialImage-methods
#' @export
setReplaceMethod("imgPath", c("SpatialImage", "character"), 
    function(x, value) 
    { 
        .path_validity(value)
        x@path <- value
        return(x) 
    })

#' @rdname SpatialImage-methods
#' @export
setReplaceMethod("imgUrl", c("SpatialImage", "character"), 
    function(x, value) 
    { 
        .url_validity(value)
        x@url <- value
        return(x) 
    })

# loadImage --------------------------------------------------------------------

#' @rdname SpatialImage-methods
#' @export
setMethod("loadImg", "SpatialImage",
    function(x) { .load_img(x)$data[[1]] })

# unloadImg --------------------------------------------------------------------

# TODO: How to handle cached images?
# add a flag cached=TRUE/FALSE in the class,
# and drop path if cached=TRUE?

#' @rdname SpatialImage-methods
#' @export
setMethod("unloadImg", "SpatialImage",
    function(x) { x@grob <- NULL; return(x) })
