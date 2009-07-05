

Rows <- function(x, i)
{
    lapply(x, function(u) rep(u, length.out = max(i, length(u)))[i])
}

keyPoints <- function(...)
{
    renderInfo <- {
        qvizexpt:::resetPanelInfo()
        qv.setScale(c(-1, 1), c(-1, 1))
        qv.setPar(...)
        qv.panel.points(0, 0)
        qvizexpt:::getPanelInfo()
    }
    qvBasicWidget(renderInfo, yexpand = FALSE, xexpand = FALSE, minwidth = 10)
}

keyLines <- function(...)
{
    renderInfo <- {
        qvizexpt:::resetPanelInfo()
        qv.setScale(c(-1, 1), c(-1, 1))
        qv.setPar(...)
        qv.panel.segments(-1, 0, 1, 0)
        qvizexpt:::getPanelInfo()
    }
    qvBasicWidget(renderInfo, yexpand = FALSE, xexpand = FALSE, minwidth = 10)
}

keyRectangles <- function(...)
{
    renderInfo <- {
        qvizexpt:::resetPanelInfo()
        qv.setScale(c(-1, 1), c(-1, 1))
        qv.setPar(...)
        qv.panel.rect(-1, -1, 1, 1)
        qvizexpt:::getPanelInfo()
    }
    qvBasicWidget(renderInfo, yexpand = FALSE, xexpand = FALSE, minwidth = 10)
}


keyCompList <- function(type = c("points", "lines", "rectangles"), pars, n)
{
    type <- match.arg(type)
    compFun <- switch(type, points = keyPoints, lines = keyLines, rectangles = keyRectangles)
    ans <- vector(mode = "list", length = n)
    for (i in seq_len(n))
    {
        ans[[i]] <- do.call(compFun, Rows(pars, i))
    }
    ans
}

mosaiq.legend <-
    function(text = NULL, ...,
             theme = mosaiq.theme(),
             font = theme$add.text,
             title = NULL,
             columns = 1,
             space = "top")
{
    n <- length(text)
    rows <- as.integer(ceiling(n / columns))
    if (n == 0) return(NULL)
    dots <- list(...)
    nmdots <- names(dots)
    if (!all(nmdots %in% c("points", "lines", "rectangles")))
        stop("Invalid argument names")
    keep <- sapply(dots, function(x) (is.list(x) || isTRUE(x))) ## list or TRUE
    dots <- dots[keep]
    nmdots <- nmdots[keep]
    pars <- 
        sapply(nmdots,
               function(s) {
                   switch(s,
                          points = theme$default,
                          lines = theme$default,
                          rectangles = theme$polygon)
               },
               simplify = FALSE)
    textComps <-
        lapply(text, qlabel, font = do.call(qv.font, font),
               xexpand = FALSE, yexpand = FALSE)
    otherComps <- vector(mode = "list", length = length(pars))
    names(otherComps) <- nmdots
    for (i in seq_along(pars))
    {
        if (isTRUE(dots[[i]])) dots[[i]] <- list()
        pars[[i]] <- modifyList(Rows(pars[[i]], seq_len(n)), dots[[i]])
        otherComps[[i]] <- keyCompList(type = nmdots[i], pars = pars[[i]], n = n)
    }
    allComps <- do.call(cbind, c(list(text = textComps), otherComps))
    key <- qvBasicWidget(xexpand = FALSE, yexpand = FALSE)
    if (is.character(title)) key[1, 1, 1, ncol(allComps)] <- qlabel(title[1], xexpand = FALSE, yexpand = FALSE)
    for (i in seq_len(nrow(allComps)))
        for (j in seq_len(ncol(allComps)))
        {
            key[2L + ((i-1L) %% rows), j + (ncol(allComps) * ((i-1L) %/% rows))] <- allComps[i, j][[1]]
        }
    keyContainer <- qvBasicWidget(xexpand = FALSE, yexpand = FALSE, margin = 10L) ## needed to compress legend
    keyContainer[2, 2] <- key
    keyContainer[1, 2] <- qvBasicWidget(xexpand = FALSE, yexpand = FALSE)
    keyContainer[3, 2] <- qvBasicWidget(xexpand = FALSE, yexpand = FALSE)
    keyContainer[2, 1] <- qvBasicWidget(xexpand = FALSE, yexpand = FALSE)
    keyContainer[2, 3] <- qvBasicWidget(xexpand = FALSE, yexpand = FALSE)
    ans <- list(keyContainer)
    names(ans) <- space
    ans
}



auto.legend <- function(include = "points", vars = panel.vars, data, enclos, ..., theme = mosaiq.theme(), legend.args = list(), space = "top")
{
    groups <- evaluate(vars$groups, data = data, enclos = enclos)
    if (is.null(groups)) NULL
    else
    {
        largs <-
            list(text = levels(as.factor(groups)),
                 theme = mosaiq.theme())
        for (inc in include) largs[[inc]] <- TRUE
        largs <- modifyList(largs, legend.args)
        ans <- do.call(mosaiq.legend, largs)
    }
}


