

mosaiq.segments <-
    function(x0, y0, x1, y1, col = "black",
             ..., painter)
{
    rgb.col <- col2rgb(col, TRUE)
    qdrawSegment(painter, x0, y0, x1, y1, stroke = rgb.col) 
}




## low-level plotting functions written using the qtpaint painter API

mosaiq.points <-
    function(x, y = NULL, type = "p", jitter.x = FALSE, jitter.y = FALSE,
             factor = 0.5, amount = NULL, horizontal = FALSE,
             cex = 1, col = "black", fill = "transparent", painter)
{
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    if (any(is.finite(x) & is.finite(y)))
    {
        circle <- qpathCircle(0, 0, 5)
        rgb.col <- col2rgb(col, TRUE)
        rgb.fill <- col2rgb(fill, TRUE)
        if ("o" %in% type || "b" %in% type)
            type <- c(type, "p", "l")
        if ("p" %in% type)
            qdrawGlyph(painter, circle,
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
        ##         if ("r" %in% type)
        ##             qv.panel.lmline(x, y)
        ##         if ("smooth" %in% type)
        ##             qv.panel.loess(x, y, horizontal = horizontal)
        ## if ("a" %in% type) qv.panel.average(x, y, horizontal = horizontal)
    }
}



mosaiq.rect <-
    function(xleft, ybottom, xright, ytop,
             x = (xleft + xright)/2, 
             y = (ybottom + ytop)/2,
             width = xright - xleft,
             height = ytop - ybottom,
             col = "black", fill = "transparent",
             painter)
{
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
             col.ref = theme$reference$col)
{
    if (is.null(groups)) groups <- gl(1, length(x))
    groups <- as.factor(groups)
    vals <- levels(groups)
    nvals <- length(vals)
    col <- rep(col, length = nvals)
    fill <- rep(fill, length = nvals)
    cpl <- qv.getScale()
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
                                ...)
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
                                ...)
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
                            ...)
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
                                ...)
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
                                ...)
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
                            ...)
            }
        }
    }
}


