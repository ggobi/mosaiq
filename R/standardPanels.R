
## simple helper function to generate new functions from old ones by
## overriding certain arguments.

customize <- function(fun, ...)
{
    args <- list(...)
    function(...) {
        dots <- list(...)
        do.call(fun, modifyList(dots, args))
    }
}


null.limits <- function()
{
    list(xlim = c(NA_real_, NA_real_),
         ylim = c(NA_real_, NA_real_))
}

default.limits <- function(x, y)
{
    if (!any(is.finite(as.numeric(x)) & is.finite(as.numeric(y))))
        return(null.limits())
    ans <- 
        list(xlim = range(as.numeric(x), na.rm = TRUE),
             ylim = range(as.numeric(y), na.rm = TRUE))
    if (is.factor(x))
    {
        ans$xat <- as.numeric(unique(x))
        ans$xlabels <- as.character(unique(x))
    }
    if (is.factor(y))
    {
        ans$yat <- as.numeric(unique(y))
        ans$ylabels <- as.character(unique(y))
    }
    ans
}

## FIXME: have prepanels use generalized 'range' function (for
## numeric, factors, time) when computing limits.

## density plots

panel.mosaiq.densityplot <-
    function(panel.vars = list(x = NULL, weights = NULL, groups = NULL),
             which.packet, packets,
             data,
             enclos,
             give.limits = FALSE,
             ...,
             shared.env)
{
    packet <- packets[[which.packet]]
    x <- evaluate(panel.vars$x, data = data, subset = packet, enclos = enclos)
    if (is.null(x)) stop("'x' cannot be NULL")
    x <- as.numeric(x)
    nobs <- sum(is.finite(x))
    if (nobs < 1)
    {
        if (give.limits) return(null.limits())
        else return()
    }
    compute.xy <- function(u)
    {
        if (any(is.finite(u)))
            with(suppressWarnings(density(u, ...)),
                 data.frame(x = x, y = y))
        else data.frame(x = numeric(0), y = numeric(0))
    }
    groups <- evaluate(panel.vars$groups, data = data, subset = packet, enclos = enclos)
    xy <-
        if (is.null(groups)) compute.xy(x)
        else 
        {
            tmp <- lapply(split(x, groups), compute.xy)
            ans <- do.call(rbind, tmp)
            ans$groups <-
                rep(factor(names(tmp), levels = names(tmp)),
                    sapply(tmp, nrow))
            ans
        }
    if (give.limits)
        return(with(xy,
                    list(xlim = range(x),
                         ylim = c(0, max(y)))))
    ## else
    limits <- shared.env$limits[[which.packet]]
    xr <- limits$xlim
    yr <- limits$ylim
    id <- with(xy, (x > min(xr) & x < max(xr) &
                    y > min(yr) & y < max(yr)))
    if (any(!id)) xy <- xy[id, ]
    with(xy, 
         mosaiq.superpose(x = x, y = y, groups = groups,
                          panel.groups = mosaiq.points, type = "l",
                          ...))
}

prepanel.mosaiq.densityplot <- function(..., give.limits)
{
    panel.mosaiq.densityplot(..., give.limits = TRUE)
}

mosaiq.superpose <-
    function(x, y = NULL, groups = NULL, type = "p", 
             ## subscripts, 
             panel.groups = mosaiq.points,
             col = theme$default$col,
             fill = theme$default$fill,
             ...,
             theme = mosaiq.theme(),
             distribute.type = FALSE)
{
    if (distribute.type)
    {
        ## In this case the 'type' argument behaves like other
        ## graphical parameters, i.e., it is repeated to be as long as
        ## the number of groups, and one used for each group.
        type <- as.list(type)
    }
    else
    {
        type <- list(unique(type))
    }
    if (length(x) > 0)
    {
        vals <-
            if (is.null(groups)) NULL
            else if (is.factor(groups)) levels(groups)
            else sort(unique(groups))
        nvals <- if (is.null(vals)) 1L else length(vals)
        col <- rep(col, length = nvals)
        fill <- rep(fill, length = nvals)
        type <- rep(type, length = nvals)
        for (i in seq_len(nvals))
        {
            id <-
                if (is.null(vals)) TRUE
                else which(groups == vals[i]) # handles NA
            if (any(id))
            {
                panel.groups(x = x[id], y = y[id],
                             col = col[i],
                             fill = fill[i],
                             type = type[[i]],
                             ...)
            }
        }
    }
}


## scatter plots

panel.mosaiq.xyplot <-
    function(panel.vars = list(x = NULL, y = NULL, groups = NULL),
             which.packet, packets,
             data, enclos,
             give.limits = FALSE,
             type = "p", grid = FALSE,
             ...,
             panel.groups = mosaiq.points,
             horizontal = FALSE,
             shared.env)
{
    packet <- packets[[which.packet]]
    x <- evaluate(panel.vars$x, data = data, subset = packet, enclos = enclos)
    y <- evaluate(panel.vars$y, data = data, subset = packet, enclos = enclos)
    if (give.limits) return(default.limits(x, y))
    if (is.logical(grid))
    {
        grid <- if (grid) list(h = -5, v = -5) else list(h = 0, v = 0)
    }
    ## mosaiq.fill(col = "yellow", border = "green", ...)
    if (grid$h != 0 || grid$v != 0) mosaiq.grid(h = grid$h, v = grid$v, col = "grey", ...)
    groups <- evaluate(panel.vars$groups, data = data, subset = packet, enclos = enclos)
    if (missing(horizontal)) 
    {
        horizontal <- if (is.factor(y)) TRUE else FALSE
    }
    if (is.null(x) || is.null(y)) stop("'x' or 'y' cannot be NULL")
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (any(is.finite(x) & is.finite(y)))
    {
        limits <- shared.env$limits[[which.packet]]
        xr <- limits$xlim
        yr <- limits$ylim
        id <- (x > min(xr) & x < max(xr) &
               y > min(yr) & y < max(yr))
        ## id <- TRUE
        mosaiq.superpose(x = x[id], y = y[id],
                         groups = groups[id],
                         panel.groups = panel.groups,
                         type = type, 
                         horizontal = horizontal, ...)
    }
}

prepanel.mosaiq.xyplot <- function(..., give.limits)
    panel.mosaiq.xyplot(..., give.limits = TRUE)


## QQ plots

panel.mosaiq.qqmath <-
    function(panel.vars = list(x = NULL, groups = NULL),
             which.packet, packets,
             data,
             enclos,
             give.limits = FALSE,
             ...,
             qtype = 7,
             f.value = NULL,
             distribution = qnorm,
             shared.env)
{
    packet <- packets[[which.packet]]
    x <- evaluate(panel.vars$x, data = data, subset = packet, enclos = enclos)
    if (is.null(x)) stop("'x' cannot be NULL")
    x <- as.numeric(x)
    nobs <- sum(!is.na(x))
    if (nobs < 1)
    {
        if (give.limits) return(null.limits())
        else return()
    }
    compute.xy <- function(u)
    {
        n <- sum(!is.na(u))
        if (is.null(f.value)) # exact data instead of quantiles
        {
            data.frame(x = distribution(ppoints(n)),
                       y = sort(u))
        }
        else
        {
            probs <- if (is.numeric(f.value)) f.value else f.value(n)
            data.frame(x = distribution(probs),
                       y = quantile(u, probs, names = FALSE,
                                    type = qtype, na.rm = TRUE))
        }
    }
    groups <- evaluate(panel.vars$groups, data = data, subset = packet, enclos = enclos)
    xy <-
        if (is.null(groups)) compute.xy(x)
        else 
        {
            tmp <- lapply(split(x, groups), compute.xy)
            ans <- do.call(rbind, tmp)
            ans$groups <-
                rep(factor(names(tmp), levels = names(tmp)),
                    sapply(tmp, nrow))
            ans
        }
    if (give.limits) return(with(xy, default.limits(x, y)))
    ## else
    limits <- shared.env$limits[[which.packet]]
    xr <- limits$xlim
    yr <- limits$ylim
    id <- with(xy, (x > min(xr) & x < max(xr) &
                    y > min(yr) & y < max(yr)))
    if (any(!id)) xy <- xy[id, ]
    with(xy, 
         mosaiq.superpose(x = x, y = y, groups = groups,
                          panel.groups = mosaiq.points,
                          ...))
}


prepanel.mosaiq.qqmath <- function(..., give.limits)
    panel.mosaiq.qqmath(..., give.limits = TRUE)



## histograms

do.breaks <- 
    function (endpoints, nint)
{
    if (length(endpoints) != 2)
        stop("error")
    endpoints[1] + diff(endpoints) * 0:nint/nint
}

hist.constructor <- 
    function (x, breaks, include.lowest = TRUE, right = TRUE, ...)
{
    if (is.numeric(breaks) && length(breaks) > 1)
        hist(as.numeric(x), breaks = breaks, plot = FALSE,
             include.lowest = include.lowest,
             right = right)
    else hist(as.numeric(x), breaks = breaks, right = right,
              plot = FALSE)
}


panel.mosaiq.histogram <-
    function(panel.vars = list(x = NULL),
             which.packet, packets,
             data,
             enclos,
             give.limits = FALSE,
             ...,
             theme = mosaiq.theme(),
             col = theme$polygon$col[1],
             fill = theme$polygon$fill[1],
             breaks = NULL, equal.widths = TRUE,
             nint = round(log2(length(x)) + 1),
             type = c("density", "count", "percent"),
             shared.env)
{
    packet <- packets[[which.packet]]
    xorig <- evaluate(panel.vars$x,
                      data = data, subset = packet,
                      enclos = enclos)
    if (is.null(xorig)) stop("'x' cannot be NULL")
    x <- as.numeric(xorig)
    if (any(is.finite(x)))
    {
        type <- match.arg(type)
        if (is.null(breaks))
        {
            breaks <-
                if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
                else if (equal.widths) do.breaks(range(x, finite = TRUE), nint)
                else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <- hist.constructor(x, breaks = breaks, ...)
        y <-
            switch(type,
                   count = h$counts,
                   percent = 100 * h$counts/length(x),
                   density = h$intensities)
        breaks <- h$breaks
        nb <- length(breaks)
        if (length(y) != nb-1)
            warning("problem with 'hist' computations")
        if (give.limits)
        {
            ans <- list(xlim = range(x, h$breaks, na.rm = TRUE),
                        ylim = range(0, y, finite = TRUE))
            if (is.factor(xorig)) 
            {
                ans$xat <- as.numeric(unique(xorig))
                ans$xlabels <- as.character(unique(xorig))
            }
            return (ans)
        }
        ## else
        limits <- shared.env$limits[[which.packet]]
        xr <- limits$xlim
        yr <- limits$ylim
        if (nb > 1)
        {
            mosaiq.rect(xleft = breaks[-nb],
                        ybottom = 0,
                        xright = breaks[-1],
                        ytop = y,
                        col = col, fill = fill,
                        ...)
        }
    }
    else 
    {
        if (give.limits) return(null.limits())
    }
}

prepanel.mosaiq.histogram <- function(..., give.limits)
    panel.mosaiq.histogram(..., give.limits = TRUE)


## dotplot

panel.mosaiq.dotplot <-
    function(panel.vars = list(x = NULL, y = NULL, groups = NULL),
             which.packet, packets,
             data, enclos,
             give.limits = FALSE,
             ...,
             theme = mosaiq.theme(),
             col = theme$dot.symbol$col,
             fill = theme$dot.symbol$fill,
             ## col.ref = theme$dot.line$col,
             horizontal = TRUE,
             shared.env)
{
    packet <- packets[[which.packet]]
    col.ref <- theme$dot.line$col
    x <- evaluate(panel.vars$x, data = data, subset = packet, enclos = enclos)
    y <- evaluate(panel.vars$y, data = data, subset = packet, enclos = enclos)
    if (give.limits) return(default.limits(x, y))
    if (missing(horizontal)) 
    {
        horizontal <- if (is.factor(x)) FALSE else TRUE
    }
    if (horizontal)
        mosaiq.abline(h = unique(as.numeric(y)), col = col.ref, ...)
    else 
        mosaiq.abline(v = unique(as.numeric(x)), col = col.ref, ...)
    panel.mosaiq.xyplot(panel.vars = panel.vars,
                        which.packet = which.packet, packets = packets,
                        data = data, enclos = enclos,
                        give.limits = FALSE,
                        horizontal = horizontal, ...,
                        shared.env = shared.env,
                        theme = theme, col = col, fill = fill)
}








prepanel.mosaiq.dotplot <- function(..., give.limits)
    panel.mosaiq.dotplot(..., give.limits = TRUE)


## bwplot


panel.mosaiq.bwplot <-
    function(panel.vars = list(x = NULL, y = NULL, groups = NULL),
             which.packet, packets,
             data,
             enclos,
             give.limits = FALSE,
             ...,
             theme = mosaiq.theme(),
             col = theme$box.rectangle$col[1],
             fill = theme$box.rectangle$fill[1],
             pch = 1, ## or "|"
             stats = boxplot.stats,
             coef = 1.5, do.out = TRUE,
             varwidth = FALSE,
             notch = FALSE,
             notch.frac = 0.5,
             horizontal = FALSE,
             box.ratio = 1, box.width = box.ratio / (1 + box.ratio),
             shared.env)
{
    packet <- packets[[which.packet]]
    x <- evaluate(panel.vars$x,
                  data = data, subset = packet,
                  enclos = enclos)
    y <- evaluate(panel.vars$y,
                  data = data, subset = packet,
                  enclos = enclos)
    if (give.limits) return(default.limits(x, y))
    if (missing(horizontal)) 
    {
        horizontal <- if (is.factor(y)) TRUE else FALSE
    }
    if (is.null(x) || is.null(y)) stop("'x' or 'y' cannot be NULL")
    x <- as.numeric(x)
    y <- as.numeric(y)

    if (!notch) notch.frac <- 0

    ## qv.setPar(par = lapply(theme$box.rectangle, "[", 1))

    if (horizontal)
    {
        levels.fos <- sort(unique(y))
        blist <-
            tapply(x, factor(y, levels = levels.fos),
                   stats,
                   coef = coef,
                   do.out = do.out)
        blist.stats <- t(sapply(blist, "[[", "stats"))
        blist.out <- lapply(blist, "[[", "out")
        blist.height <- box.width # box.ratio / (1 + box.ratio)
        if (varwidth)
        {
            maxn <- max(table(y))
            blist.n <- sapply(blist, "[[", "n")
            blist.height <- sqrt(blist.n / maxn) * blist.height
        }

        mosaiq.rect(xleft = blist.stats[, 2],
                    xright = blist.stats[, 4],
                    y = levels.fos, 
                    height = blist.height,
                    col = col, fill = fill,
                    ...)

        ## whiskers

        mosaiq.segments(c(blist.stats[, 2], blist.stats[, 4]),
                        rep(levels.fos, 2),
                        c(blist.stats[, 1], blist.stats[, 5]),
                        rep(levels.fos, 2),
                        col = col, ...)
        mosaiq.segments(c(blist.stats[, 1], blist.stats[, 5]),
                        levels.fos - blist.height / 2,
                        c(blist.stats[, 1], blist.stats[, 5]),
                        levels.fos + blist.height / 2,
                        col = col, ...)

        ## dot

        if (all(pch == "|"))
        {
            mult <- if (notch) 1 - notch.frac else 1
            mosaiq.segments(blist.stats[, 3],
                            levels.fos - mult * blist.height / 2,
                            blist.stats[, 3],
                            levels.fos + mult * blist.height / 2,
                            col = col, ...)
        }
        else
        {
            mosaiq.points(x = blist.stats[, 3],
                          y = levels.fos,
                          col = col, fill = col, ...)
        }

        ## outliers
        mosaiq.points(x = unlist(blist.out),
                      y = rep(levels.fos, sapply(blist.out, length)),
                      col = col, ...)
    }
    else
    {
        levels.fos <- sort(unique(x))
        blist <-
            tapply(y, factor(x, levels = levels.fos),
                   stats,
                   coef = coef,
                   do.out = do.out)
        blist.stats <- t(sapply(blist, "[[", "stats"))
        blist.out <- lapply(blist, "[[", "out")
        blist.height <- box.width # box.ratio / (1 + box.ratio)
        if (varwidth)
        {
            maxn <- max(table(x))
            blist.n <- sapply(blist, "[[", "n")
            blist.height <- sqrt(blist.n / maxn) * blist.height
        }

        mosaiq.rect(ybottom = blist.stats[, 2],
                    ytop = blist.stats[, 4],
                    x = levels.fos, 
                    width = blist.height,
                    col = col, fill = fill, ...)

        ## whiskers

        mosaiq.segments(rep(levels.fos, 2),
                        c(blist.stats[, 2], blist.stats[, 4]),
                        rep(levels.fos, 2),
                        c(blist.stats[, 1], blist.stats[, 5]),
                        col = col, ...)
        mosaiq.segments(levels.fos - blist.height / 2,
                        c(blist.stats[, 1], blist.stats[, 5]),
                        levels.fos + blist.height / 2,
                        c(blist.stats[, 1], blist.stats[, 5]),
                        col = col, ...)

        ## dot

        if (all(pch == "|"))
        {
            mult <- if (notch) 1 - notch.frac else 1
            mosaiq.segments(levels.fos - mult * blist.height / 2,
                            blist.stats[, 3],
                            levels.fos + mult * blist.height / 2,
                            blist.stats[, 3],
                            col = col, ...)
        }
        else
        {
            mosaiq.points(x = levels.fos,
                          y = blist.stats[, 3],
                          col = col, fill = col, ...)
        }

        ## outliers

        mosaiq.points(x = rep(levels.fos, sapply(blist.out, length)),
                      y = unlist(blist.out),
                      col = col, ...)
    }
}

prepanel.mosaiq.bwplot <- function(..., give.limits)
    panel.mosaiq.bwplot(..., give.limits = TRUE)




## barchart

panel.mosaiq.barchart <-
    function(panel.vars = list(x = NULL, y = NULL, groups = NULL),
             which.packet, packets,
             data, enclos,
             give.limits = FALSE,
             stack = TRUE,
             origin = 0,
             ...,
             horizontal = TRUE)
{
    packet <- packets[[which.packet]]
    x <- evaluate(panel.vars$x, data = data, subset = packet, enclos = enclos)
    y <- evaluate(panel.vars$y, data = data, subset = packet, enclos = enclos)
    if (missing(horizontal)) 
    {
        horizontal <- if (is.factor(x)) FALSE else TRUE
    }
    groups <- evaluate(panel.vars$groups, data = data, subset = packet, enclos = enclos)
    if (give.limits)
    {
        limits <- default.limits(x, y)
        if (stack) 
        {
            stack.lim <- function(u, v)
            {
                upos <- if (any(u > 0)) tapply(u[u > 0], v[u > 0, drop = TRUE], sum, na.rm = TRUE) else 0
                uneg <- if (any(u < 0)) tapply(u[u < 0], v[u < 0, drop = TRUE], sum, na.rm = TRUE) else 0
                range(upos, uneg, finite = TRUE)
            }
            if (horizontal) limits$xlim <- stack.lim(x, y)
            else limits$ylim <- stack.lim(y, x)
        }
        if (is.finite(origin))
            if (horizontal)
                limits$xlim <- range(origin, limits$xlim)
            else
                limits$ylim <- range(origin, limits$ylim)
        return(limits)
    }
    if (any(is.finite(x) & is.finite(y)))
    {
        mosaiq.bars(x = x, y = y, groups = groups,
                    stack = stack, 
                    horizontal = horizontal, ...)
    }
}


prepanel.mosaiq.barchart <- function(..., give.limits)
    panel.mosaiq.barchart(..., give.limits = TRUE)


## maps

prepanel.mosaiq.mapplot <- function(..., give.limits)
    panel.mosaiq.mapplot(..., give.limits = TRUE)


panel.mosaiq.mapplot <-
    function(panel.vars = list(x = NULL, y = NULL),
             which.packet, packets,
             data, enclos,
             map,
             give.limits = FALSE,
             cuts = 30, breaks,
             colramp = colorRampPalette(c("white", "black")),
             ...)
{
    if (give.limits) return(default.limits(map$x, map$y))
    packet <- packets[[which.packet]]
    x <- evaluate(panel.vars$x, data = data, subset = packet, enclos = enclos)
    y <- evaluate(panel.vars$y, data = data, subset = packet, enclos = enclos)
    names(x) <- as.character(y)
    if (missing(breaks))
        breaks <-
            if (is.factor(x)) seq_len(1 + nlevels(x)) - 0.5
            else do.breaks(range(x, finite = TRUE), cuts)
    interval <-
        cut(x[map$names], breaks = breaks,
            labels = FALSE, include.lowest = TRUE)
    col.regions <- colramp(length(breaks) - 1)
    col <- col.regions[interval]
    mosaiq.polygon(map, col = col, ...)
}



