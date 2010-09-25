

## copy of unexported graphics:::.smoothScatterCalcDensity()

.smoothScatterCalcDensity <- function(x, nbin, bandwidth, range.x)
{
    if (!("KernSmooth" %in% loadedNamespaces())) {
        ns <- try(loadNamespace("KernSmooth"))
        if (isNamespace(ns))
            message("(loaded the KernSmooth namespace)")
        else stop("panel.smoothScatter() requires the KernSmooth package, but unable to load KernSmooth namespace")
    }
    if (length(nbin) == 1)
        nbin <- c(nbin, nbin)
    if (!is.numeric(nbin) || (length(nbin)!=2)) stop("'nbin' must be numeric of length 1 or 2")
    if (missing(bandwidth)) {
        bandwidth <- diff(apply(x, 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)) / 25
    } else {
        if(!is.numeric(bandwidth)) stop("'bandwidth' must be numeric")
    }
    bandwidth[bandwidth==0] <- 1
    ## create density map
    if(missing(range.x))
        rv <- KernSmooth::bkde2D(x, gridsize=nbin, bandwidth=bandwidth)
    else
        rv <- KernSmooth::bkde2D(x, gridsize=nbin, bandwidth=bandwidth, range.x=range.x) 
    rv$bandwidth <- bandwidth
    return(rv)
}


mosaiq.smoothScatter <-
    function(x, y = NULL,
             nbin = 64,
             cuts = 255,
             bandwidth,
             
             nrpoints = 100,
             transformation = function(x) x^0.25,
             cex = 1,
             col = "black",
             range.x,
             ..., colramp, painter, panel.env)
{
    ## print(ls.str(panel.env))
    if (!is.numeric(nrpoints) | (nrpoints < 0) | (length(nrpoints) != 1))
        stop("'nrpoints' should be numeric scalar with value >= 0.")
    if (missing(colramp))
        colramp <- colorRampPalette(c("white", "#F7FBFF", "#DEEBF7",
                                      "#C6DBEF", "#9ECAE1", "#6BAED6",
                                      "#4292C6", "#2171B5", "#08519C",
                                      "#08306B"))
    ## perform expensive calculations only the first time
    if (is.null(panel.env$panel.specific))
    {
        panel.specific <- panel.env$panel.specific <- new.env(parent = emptyenv())
        x <- as.numeric(x)
        y <- as.numeric(y)
        xy <- xy.coords(x, y, recycle = TRUE)
        x <- cbind(xy$x, xy$y)[!(is.na(xy$x) | is.na(xy$y)), ]
        map <- .smoothScatterCalcDensity(x, nbin, bandwidth, range.x)
        dens <- as.numeric(transformation(map$fhat))
        lcols <-
            lattice:::level.colors(dens,
                                   at = seq(from = 0, to = 1.01 * max(dens), length = cuts + 2),
                                   col.regions = colramp(cuts + 1), colors = TRUE)
        if (nrpoints != 0)
            xpoints <- 
                with(map,
                 {
                     ix1 <- round((x[, 1] - x1[1])/(x1[length(x1)] - x1[1]) *
                                  (length(x1) - 1))
                     ix2 <- round((x[, 2] - x2[1])/(x2[length(x2)] - x2[1]) *
                                  (length(x2) - 1))
                     idens <- dens[1 + ix2 * length(x1) + ix1]
                     nrpoints <- min(nrow(x), ceiling(nrpoints))
                     sel <- order(idens, decreasing = FALSE)[1:nrpoints]
                     x[sel, 1:2]
                 })
        ## save these for use in later invocations
        panel.specific$map <- map
        panel.specific$dens <- dens
        panel.specific$lcols <- lcols
        panel.specific$xpoints <- xpoints
    }
    else 
    {
        map <- panel.env$panel.specific$map
        dens <- panel.env$panel.specific$dens
        lcols <- panel.env$panel.specific$lcols
        xpoints <- panel.env$panel.specific$xpoints
    }

    keep <- zapsmall(map$fhat) > 0
    ## message(sum(keep))
    with(map,
         mosaiq.rect(x = rep(x1, length(x2))[keep],
                     y = rep(x2, each = length(x1))[keep],
                     width = diff(head(x1, 2)),
                     height = diff(head(x2, 2)),
                     col = lcols[keep],
                     fill = lcols[keep],
                     painter = painter))
    if (!is.null(xpoints))
        mosaiq.points(xpoints,
                      pch = qglyphSquare(1),
                      cex = cex,
                      col = "black", fill = "black",
                      painter = painter)
}

