

mosaiq.text <-
    function(x, y = NULL, labels = seq_along(x),
             rot = 0, 
             font = qfont(),
             ...,
             painter)
{
    qfont(painter) <- font
    labels <- as.character(labels)
    xy <- xy.coords(x, y, recycle = recycle)
    labels <- rep(labels, length = length(xy[["x"]]))
    stopifnot(length(labels) == length(xy[["x"]]))
    qdrawText(painter,
              labels, xy$x, xy$y,
              rot = rot)
}

mosaiq.segments <-
    function(x0, y0, x1, y1, col = "black",
             ..., painter)
{
    rgb.col <- col2rgb(col, TRUE)
    qdrawSegment(painter, x0, y0, x1, y1, stroke = rgb.col) 
}


mosaiq.loess <-
    function(x, y, span = 2/3, degree = 1,
             family = c("symmetric", "gaussian"),
             evaluation = 50,
             horizontal = FALSE,
             ...,
             painter)
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) return()
    if (horizontal)
    {
        smooth <-
            loess.smooth(y[ok], x[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        mosaiq.points(x = smooth$y, y = smooth$x, type = "l", ...)
    }
    else
    {
        smooth <-
            loess.smooth(x[ok], y[ok], span = span, family = family,
                         degree = degree, evaluation = evaluation)
        mosaiq.points(x = smooth$x, y = smooth$y, type = "l", ...)
    }
}

mosaiq.abline <-
    function(a = NULL, b = NULL,
             h = NULL, v = NULL,
             reg = NULL, coef = NULL,
             ...,
             item, painter, exposed)
{
    cl <- list(xlim = exposed[, 1],
               ylim = exposed[, 2])
    if (!is.null(h) || !is.null(v))
    {
        h <- unique(h)
        v <- unique(v)
        nh <- length(h)
        nv <- length(v)
        x0 <- c(numeric(0), rep(cl$xlim[1], nh), v)
        x1 <- c(numeric(0), rep(cl$xlim[2], nh), v)
        y0 <- c(numeric(0), h, rep(cl$ylim[1], nv))
        y1 <- c(numeric(0), h, rep(cl$ylim[2], nv))
        mosaiq.segments(x0, y0, x1, y1, ...,
                        painter = painter)
    }
    if (!is.null(reg))
    {
        if (is.null(coef)) warning("'coef' overridden by 'reg'")
        coef <- coef(reg)
    }
    if (!is.null(coef))
    {
        if (!(is.null(a) && is.null(b))) warning("'a' and 'b' overridden by 'coef'")
        a <- coef[1]
        b <- coef[2]
    }
    if (!is.null(a))
    {
        if (is.null(b))
        {
            if (length(a) == 2)
            {
                b <- a[2]
                a <- a[1]
            }
            else stop("'a' must have length 2 if 'b' is NULL")
        }
        if (any(!is.finite(b))) stop("all elements of 'b' must be finite; use 'v' instead.")
        fabline <- function(x) { a + b * x }
        fbaline <- function(y) { (y - a) / b } ## b shouldn't be 0 then
        y0 <- fabline(cl$xlim[1])
        y1 <- fabline(cl$xlim[2])
        if (any(c(y0,y1) < cl$ylim[1] |
                c(y0,y1) > cl$ylim[2])) warning("FIXME: potential clipping issues")
        if (FALSE) ## check b != 0, and what about lines completely outside?
        {
            x0 <- fbaline(cl$ylim[1])
            x1 <- fbaline(cl$ylim[2])
            ## do something with these
        }
        mosaiq.segments(cl$xlim[1], y0, cl$xlim[2], y1,
                        ..., painter = painter)
    }
}


mosaiq.lmline <- function(x, y, ...)
{
    if (length(x) > 1)
    {
        fm <- lm(as.numeric(y) ~ as.numeric(x))
        mosaiq.abline(coef = coef(fm), ...)
    }
}






## low-level plotting functions written using the qtpaint painter API

mosaiq.points <-
    function(x, y = NULL, type = "p", jitter.x = FALSE, jitter.y = FALSE,
             factor = 0.5, amount = NULL, horizontal = FALSE,
             pch = qpathCircle(0, 0, 5),
             cex = 1, col = "black", fill = "transparent", ..., painter)
{
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    if (any(is.finite(x) & is.finite(y)))
    {
        rgb.col <- col2rgb(col, TRUE)
        rgb.fill <- col2rgb(fill, TRUE)
        if ("o" %in% type || "b" %in% type)
            type <- c(type, "p", "l")
        if ("p" %in% type)
            qdrawGlyph(painter, pch,
                       x = if (jitter.x) jitter(x, factor = factor, amount = amount) else x,
                       y = if (jitter.y) jitter(y, factor = factor, amount = amount) else y,
                       cex = cex, stroke = rgb.col, fill = rgb.fill)
        if ("l" %in% type)
            qdrawLine(painter, x = x, y = y, stroke = rgb.col)
        if ("h" %in% type)
        {
            if (horizontal)
                qdrawSegment(painter, x, y, 0, y, stroke = rgb.col)
            else
                qdrawSegment(painter, x, y, x, 0, stroke = rgb.col)
        }
        if ("r" %in% type)
            mosaiq.lmline(x, y, ...)
        if ("smooth" %in% type)
            mosaiq.loess(x, y, horizontal = horizontal, ...)
        ## if ("a" %in% type) qv.panel.average(x, y, horizontal = horizontal)
    }
}



mosaiq.grid <-
    function(h = 3, v = 3, col = "grey", ...,
             item, painter, exposed)
{
    cl <- list(xlim = exposed[, 1],
               ylim = exposed[, 2])
    h <- h[1]; v <- v[1]
    if (h < 0)
        mosaiq.abline(h = pretty(cl$ylim, n = -h),
                      col = col, ..., item = item, painter = painter, exposed = exposed)
    if (v < 0)
        mosaiq.abline(v = pretty(cl$xlim, n = -h),
                      col = col, ..., item = item, painter = painter, exposed = exposed)
    if (h > 0)
        mosaiq.abline(h = do.breaks(cl$ylim, h + 1)[-c(1, h + 2)],
                      col = col, ..., item = item, painter = painter, exposed = exposed)
    if (v > 0)
        mosaiq.abline(v = do.breaks(cl$xlim, v + 1)[-c(1, v + 2)],
                      col = col, ..., item = item, painter = painter, exposed = exposed)
}

mosaiq.rect <-
    function(xleft, ybottom, xright, ytop,
             x = (xleft + xright)/2, 
             y = (ybottom + ytop)/2,
             width = xright - xleft,
             height = ytop - ybottom,
             col = "black", fill = "transparent",
             ...,
             painter)
{
    qantialias(painter) <- FALSE
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    n <- length(x)
    ## col <- rep(col, length = n)
    ## fill <- rep(fill, length = n)
    rgb.col <- col2rgb(col, TRUE)
    rgb.fill <- col2rgb(fill, TRUE)
    qdrawRect(painter,
              x - width/2, y - height/2, 
              x + width/2, y + height/2, 
              stroke = rgb.col,
              fill = rgb.fill)
}

mosaiq.polygon <-
    function(x, y = NULL,
             border = "black",
             col = "transparent",
             ...,
             painter) 
{
    qantialias(painter) <- FALSE
    if (sum(!is.na(x)) < 1) return()
    border <- 
        if (all(is.na(border)))
            "transparent"
        else if (is.logical(border))
        {
            if (border) "black"
            else "transparent"
        }
        else border
    xy <- xy.coords(x, y, recycle = TRUE)
    rgb.border <- col2rgb(border, TRUE)
    rgb.col <- col2rgb(col, TRUE)
    qdrawPolygon(painter, xy$x, xy$y,
                 stroke = rgb.border, fill = rgb.col)
}


mosaiq.bars <-
    function(x, y, groups,
             stack = TRUE, 
             horizontal = TRUE,
             origin = 0, reference = TRUE,
             box.ratio = 1,
             box.width = box.ratio / (1 + box.ratio),
             ...,
             theme = mosaiq.theme(),
             col = theme$polygon$col,
             fill = theme$polygon$fill,
             col.ref = theme$reference$col,
             item, painter, exposed)
{
    str(list(x, y))
    if (is.null(groups)) groups <- gl(1, length(x))
    groups <- as.factor(groups)
    vals <- levels(groups)
    nvals <- length(vals)
    col <- rep(col, length = nvals)
    fill <- rep(fill, length = nvals)
    cpl <- list(xlim = exposed[, 1],
                ylim = exposed[, 2])
    origin <-
        if (stack) 0
        else if (horizontal)
        {
            if (origin < cpl$xlim[1]) cpl$xlim[1]
            else if (origin > cpl$xlim[2]) cpl$xlim[2]
            else origin
        }
        else ## if (!horizontal)
        {
            if (origin < cpl$ylim[1]) cpl$ylim[1]
            else if (origin > cpl$ylim[2]) cpl$ylim[2]
            else origin
        }
    x <- as.numeric(x)
    y <- as.numeric(y)
    groups <- as.numeric(groups)
    if (horizontal)
    {
        if (stack)
        {
            for (i in unique(y))
            {
                ok <- y == i
                ord <- sort.list(groups[ok])
                pos <- x[ok][ord] > 0
                nok <- sum(pos, na.rm = TRUE)
                if (nok > 0)
                {
                    x.left <- cumsum(c(0, x[ok][ord][pos][-nok]))
                    mosaiq.rect(xleft = x.left,
                                xright = x.left + x[ok][ord][pos],
                                y = rep(i, nok),
                                height = rep(box.width, length = nok),
                                col = col[groups[ok][ord][pos]],
                                fill = fill[groups[ok][ord][pos]],
                                ...,
                                item= item, painter = painter, exposed = exposed)
                }
                neg <- x[ok][ord] < 0
                nok <- sum(neg, na.rm = TRUE)
                if (nok > 0)
                {
                    x.left <- cumsum(c(0, x[ok][ord][neg][-nok]))
                    mosaiq.rect(xleft = x.left,
                                xright = x.left + x[ok][ord][neg],
                                y = rep(i, nok),
                                height = rep(box.width, nok),
                                col = col[groups[ok][ord][neg]],
                                fill = fill[groups[ok][ord][neg]],
                                ...,
                                item= item, painter = painter, exposed = exposed)
                }
            }
        }
        else ## stack = FALSE
        {
            for (i in unique(y))
            {
                ok <- y == i
                nok <- sum(ok, na.rm = TRUE)
                mosaiq.rect(xleft = rep(origin, nok),
                            xright = x[ok],
                            y = (i + box.width/nvals * (groups[ok] - (nvals + 1)/2)),
                            height = rep(box.width/nvals, length = nok),
                            col = col[groups[ok]],
                            fill = fill[groups[ok]],
                            ...,
                            item= item, painter = painter, exposed = exposed)
            }
        }
    }
    else # if (!horizontal)
    {
        if (stack)
        {
            for (i in unique(x))
            {
                ok <- x == i
                ord <- sort.list(groups[ok])
                pos <- y[ok][ord] > 0
                nok <- sum(pos, na.rm = TRUE)
########
                if (nok > 0)
                {
                    y.bottom <- cumsum(c(0, y[ok][ord][pos][-nok]))
                    mosaiq.rect(ybottom = y.bottom,
                                ytop = y.bottom + y[ok][ord][pos],
                                x = rep(i, nok),
                                width = rep(box.width, length = nok),
                                col = col[groups[ok][ord][pos]],
                                fill = fill[groups[ok][ord][pos]],
                                ...,
                                item= item, painter = painter, exposed = exposed)
                }
                neg <- y[ok][ord] < 0
                nok <- sum(neg, na.rm = TRUE)
                if (nok > 0)
                {
                    y.bottom <- cumsum(c(0, y[ok][ord][neg][-nok]))
                    mosaiq.rect(ybottom = y.bottom,
                                ytop = y.bottom + y[ok][ord][neg],
                                x = rep(i, nok),
                                width = rep(box.width, nok),
                                col = col[groups[ok][ord][neg]],
                                fill = fill[groups[ok][ord][neg]],
                                ...,
                                item= item, painter = painter, exposed = exposed)
                }
            }
        }
        else ## stack = FALSE
        {
            for (i in unique(x))
            {
                ok <- x == i
                nok <- sum(ok, na.rm = TRUE)
                mosaiq.rect(ybottom = rep(origin, nok),
                            ytop = y[ok],
                            x = (i + box.width/nvals * (groups[ok] - (nvals + 1)/2)),
                            width = rep(box.width/nvals, length = nok),
                            col = col[groups[ok]],
                            fill = fill[groups[ok]],
                            ...,
                            item= item, painter = painter, exposed = exposed)
            }
        }
    }
}



mosaiq.fill <-
    function(col = "grey", border = "black", ...,
             item, painter, exposed)
{
    cl <- list(xlim = exposed[, 1],
               ylim = exposed[, 2])
    mosaiq.rect(cl$xlim[1], cl$ylim[1],
                cl$xlim[2], cl$ylim[2],
                col = border, fill = col,
                painter = painter)
}

