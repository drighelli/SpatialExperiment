.remote_file_cache <- function(URL, cache) 
{
    # perhaps can be more tolerant of missing a BiocFileCache installation?
    if (is.null(cache)) {
        args <- list(ask=FALSE)
        cache.dir <- getOption("SpatialExperiment.remote.cache.path", NULL)
        if (!is.null(cache.dir))
            args$cache <- cache.dir
        cache <- do.call(BiocFileCache::BiocFileCache, args)
    }
    BiocFileCache::bfcrpath(cache, URL)
}

image.cache <- new.env()
image.cache$cached <- list()

#' @importFrom utils head tail object.size
.get_from_cache <- function(ID, FUN) 
{
    img <- image.cache$cached[[ID]]
    if (!is.null(img)) {
        # if we get the image, we rotate it to the end of the cache so that it
        # is less likely to be evicted; we aim to preserve frequently used images
        IDs <- c(setdiff(names(image.cache$cached), ID), ID)
        image.cache$cached <- image.cache$cached[IDs]
    } else if (!is.null(FUN)) {
        img <- FUN()
        current <- as.double(object.size(img))
        
        # adding the image. Evicting old images until space is available
        used <- lapply(image.cache$cached, FUN=function(x) as.double(object.size(x)))
        used2 <- c(used, current)
        cum.used <- rev(cumsum(rev(used2)))
        
        # default to unlimited cache to mimic autoloading
        keep <- cum.used <= getOption("SpatialExperiment.cache.size", 2^32)
        image.cache$cached <- image.cache$cached[head(keep, -1)]
        if (tail(keep, 1))
            image.cache$cached[[ID]] <- img
    }
    img
}

.flush_cache <- function() 
{
    # currently for use in tests only,
    # but may possibly want to export
    image.cache$cached <- list()
}

# (counter-)clockwise 90 degree rotation
.rotate <- function(x, degrees) {
    # 'degrees' should be a single numeric divisible by 90
    stopifnot(
        length(degrees) == 1,
        is.numeric(degrees), 
        degrees %% 90 == 0)
    s <- sign(degrees)
    f <- ifelse(s == 1, 
        \(x) t(apply(x, 2, rev)), # clockwise
        \(x) apply(x, 1, rev))    # counter-clockwise
    n <- abs(degrees / 90)
    for (i in seq_len(n)) 
        x <- f(x)
    as.raster(x)
}

# flip over horizontal/vertical axis
.mirror <- function(x, axis) {
    x <- if (axis == "h") {
        apply(x, 2, rev)
    } else {
        t(apply(x, 1, rev))
    }
    as.raster(x)
}
