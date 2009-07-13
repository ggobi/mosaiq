
create.panels <-
    function(layout,
             packets, limits,
             panel.vars, panel,
             data = .GlobalEnv,
             enclos = .GlobalEnv, ...)
{
    ans <- array(list(NULL), dim = dim(layout))
    for (p in seq_len(length(ans)))
    {
        i <- layout[p]
        if (i > 0)
        {
            ans[[p]] <-
            {
                local(
                  {
                      ## scene <- qgraphicsScene()
                      ## root <- qlayer(scene)

                      paintFun <- function(item, painter, exposed)
                      {
                          ## message("i is ", i)
                          ## str(packets[[i]])
                          ## qv.panel.fill(col = "transparent", border = "black")

                          cl <- list(xlim = exposed[, 1], ylim = exposed[, 2])
                          const <- 0
                          mosaiq.rect(cl$xlim[1]+const, cl$ylim[1]+const,
                                      cl$xlim[2]-const, cl$ylim[2]-const, 
                                      col = "black", fill = "transparent", painter = painter)
                          panel(panel.vars = panel.vars,
                                packet = packets[[i]],
                                limits = limits[[i]],
                                data = data,
                                enclos = enclos, ...,
                                item = item,
                                painter = painter,
                                exposed = exposed)
                      }
                      ## FIXME: do we really need this hack?
                      assign("i", i, environment(paintFun))
                      panel.layer <- qlayer(NULL, paintFun = paintFun)
                      qlimits(panel.layer) <-
                          qrect(limits[[i]]$xlim, limits[[i]]$ylim)
                      qcacheMode(panel.layer) <- "none"
                      qminimumSize(panel.layer) <- qsize(20, 20)
                      panel.layer
                  })
            }
        }
    }
    ans
}



create.strip.top <-
    function(layout,
             packets,
             which.margins = seq_along(dim(packets)),
             ...,
             theme = mosaiq.theme(),
             col = theme$strip$col,
             fill = theme$strip$fill)
{
    ans <- array(list(NULL), dim = dim(layout))
    if (is.null(dimnames(packets))) return(ans) # no conditioning variables
    nmargin <- length(which.margins)
    col <- rep(col, length = nmargin)
    fill <- rep(fill, length = nmargin)
    margin.combs <- do.call(expand.grid, dimnames(packets))
    for (p in seq_len(length(ans)))
    {
        i <- layout[p]
        if (i > 0)
        {
            labs <-
                sapply(which.margins,
                       function(w) {
                           as.character(margin.combs[i, w])
                       })
            ans[[p]] <-
                labelLayer(paste(labs, collapse = "/"),
                           col = col, fill = fill)
        }
    }
    ans
}


create.axis <-
    function(layout, limits, which, side,
             theme = mosaiq.theme(),
             col = theme$axis$col[1],
             font = qv.font(family = theme$axis$famiy,
                            pointsize = theme$axis$pointsize),
             ...)
{
    ans <- array(list(NULL), dim = dim(layout))
    for (p in seq_len(length(ans)))
    {
        i <- layout[p]
        if (i > 0)
            ans[[p]] <-
                switch(which,
                       x = qxaxis(limits[[i]]$xlim,
                                  side = side,
                                  at = limits[[i]]$xat,
                                  labels = limits[[i]]$xlabels,
                                  font = font),
                       y = qyaxis(limits[[i]]$ylim,
                                  side = side,
                                  at = limits[[i]]$yat,
                                  labels = limits[[i]]$ylabels,
                                  font = font))
    }
    ans
}

create.xaxis.bottom <- function(...)
{
    create.axis(..., which = "x", side = "bottom")
}
create.xaxis.top <- function(...)
{
    create.axis(..., which = "x", side = "top")
}
create.yaxis.left <- function(...)
{
    create.axis(..., which = "y", side = "left")
}
create.yaxis.right <- function(...)
{
    create.axis(..., which = "y", side = "right")
}



create.page <-
    function(page = 1,
             panel.widgets,
             strip.top.widgets,
             xaxis.bottom.widgets,
             xaxis.top.widgets,
             yaxis.left.widgets,
             yaxis.right.widgets,
             relation,
             alternating)
{
    ldim <- dim(panel.widgets)
    if (page > ldim[3]) stop("Invalid value of 'page'")

    if (relation$x == "same") 
    {
        xaxis.bottom.row <- ldim[2]
        xaxis.top.row <- 1L
    }
    else 
    {
        xaxis.bottom.row <- seq_len(ldim[2])
        xaxis.top.row <- integer(0)
    }

    if (relation$y == "same") 
    {
        yaxis.right.column <- ldim[1]
        yaxis.left.column <- 1L
    }
    else 
    {
        yaxis.left.column <- seq_len(ldim[1])
        yaxis.right.column <- integer(0)
    }


    scene <- qgraphicsScene()
    root <- qlayer(scene)


    checkAndAdd <- function(x, layer, i, j,
                            colstretch = 0, rowstretch = 0)
    {
        if (is(layer, "QViz::RLayer"))
        {
            qaddItem(x, layer, i, j)
            qrowStretch(layer) <- rowstretch
            qcolStretch(layer) <- colstretch
        }
    }

    ## panels
    for (column in seq_len(ldim[1]))
        for (row in seq_len(ldim[2]))
        {
            pos <- compute.position(column, row, what = "panel")
            checkAndAdd(root,
                        panel.widgets[column, row, page][[1]],
                        pos["row"], pos["column"],
                        colstretch = 1, rowstretch = 1)
        }
    ## strip.top
    for (column in seq_len(ldim[1]))
        for (row in seq_len(ldim[2]))
        {
            pos <- compute.position(column, row, what = "strip.top")
            checkAndAdd(root,
                        strip.top.widgets[column, row, page][[1]],
                        pos["row"], pos["column"],
                        colstretch = 1, rowstretch = 0)
        }
    ## xaxis
    for (column in seq_len(ldim[1]))
    {
        ## bottom
        for (row in xaxis.bottom.row)
        {
            if (rep(alternating$x, length = column)[column] %in% c(0, 2)) next
            pos <- compute.position(column, row, what = "xaxis.bottom")
            checkAndAdd(root,
                        xaxis.bottom.widgets[column, row, page][[1]],
                        pos["row"], pos["column"],
                        colstretch = 1, rowstretch = 0)
        }
        ## top
        for (row in xaxis.top.row)
        {
            if (rep(alternating$x, length = column)[column] %in% c(0, 1)) next
            pos <- compute.position(column, row, what = "xaxis.top")
            checkAndAdd(root,
                        xaxis.top.widgets[column, row, page][[1]],
                        pos["row"], pos["column"],
                        colstretch = 1, rowstretch = 0)
            
        }
    }
    
    ## yaxis
    for (row in seq_len(ldim[2]))
    {
        for (column in yaxis.left.column)
        {
            if (rep(alternating$y, length = row)[row] %in% c(0, 2)) next
            pos <- compute.position(column, row, what = "yaxis.left")
            checkAndAdd(root,
                        yaxis.left.widgets[column, row, page][[1]],
                        pos["row"], pos["column"],
                        colstretch = 0, rowstretch = 1)
        }
        for (column in yaxis.right.column)
        {
            if (rep(alternating$y, length = row)[row] %in% c(0, 1)) next
            pos <- compute.position(column, row, what = "yaxis.right")
            checkAndAdd(root,
                        yaxis.right.widgets[column, row, page][[1]],
                        pos["row"], pos["column"],
                        colstretch = 0, rowstretch = 1)
        }
    }
    ## .MosaicEnv$panel.layout <- l
    ## container

    view <- qplotView(scene = scene, opengl = FALSE)
    view$focusPolicy <- 0
    view

}




create.page.QWidget <-
    function(page = 1,
             panel.widgets,
             strip.top.widgets,
             xaxis.bottom.widgets,
             xaxis.top.widgets,
             yaxis.left.widgets,
             yaxis.right.widgets,
             relation,
             alternating)
{
    ldim <- dim(panel.widgets)
    if (page > ldim[3]) stop("Inalid value of 'page'")

    if (relation$x == "same") 
    {
        xaxis.bottom.row <- ldim[2]
        xaxis.top.row <- 1L
    }
    else 
    {
        xaxis.bottom.row <- seq_len(ldim[2])
        xaxis.top.row <- integer(0)
    }

    if (relation$y == "same") 
    {
        yaxis.right.column <- ldim[1]
        yaxis.left.column <- 1L
    }
    else 
    {
        yaxis.left.column <- seq_len(ldim[1])
        yaxis.right.column <- integer(0)
    }
    container <- qwidget()
    l <- qlayout(NULL)
    l$margin <- 0
    l$spacing <- 0
    qsetLayout(container, l)

    checkAndAdd <- function(layout, widget, i, j)
    {
        if (is(widget, "QWidget"))
            qaddWidgetToLayout(layout, widget, i, j)
    }

    ## panels
    for (column in seq_len(ldim[1]))
        for (row in seq_len(ldim[2]))
        {
            pos <- compute.position(column, row, what = "panel")
            checkAndAdd(l,
                        panel.widgets[column, row, page][[1]],
                        pos["row"], pos["column"])
        }
    ## strip.top
    for (column in seq_len(ldim[1]))
        for (row in seq_len(ldim[2]))
        {
            pos <- compute.position(column, row, what = "strip.top")
            checkAndAdd(l,
                        strip.top.widgets[column, row, page][[1]],
                        pos["row"], pos["column"])
        }
    ## xaxis
    for (column in seq_len(ldim[1]))
    {
        ## bottom
        for (row in xaxis.bottom.row)
        {
            if (rep(alternating$x, length = column)[column] %in% c(0, 2)) next
            pos <- compute.position(column, row, what = "xaxis.bottom")
            checkAndAdd(l,
                        xaxis.bottom.widgets[column, row, page][[1]],
                        pos["row"], pos["column"])
        }
        ## top
        for (row in xaxis.top.row)
        {
            if (rep(alternating$x, length = column)[column] %in% c(0, 1)) next
            pos <- compute.position(column, row, what = "xaxis.top")
            checkAndAdd(l,
                        xaxis.top.widgets[column, row, page][[1]],
                        pos["row"], pos["column"])
            
        }
    }
    
    ## yaxis
    for (row in seq_len(ldim[2]))
    {
        for (column in yaxis.left.column)
        {
            if (rep(alternating$y, length = row)[row] %in% c(0, 2)) next
            pos <- compute.position(column, row, what = "yaxis.left")
            checkAndAdd(l,
                        yaxis.left.widgets[column, row, page][[1]],
                        pos["row"], pos["column"])
        }
        for (column in yaxis.right.column)
        {
            if (rep(alternating$y, length = row)[row] %in% c(0, 1)) next
            pos <- compute.position(column, row, what = "yaxis.right")
            checkAndAdd(l,
                        yaxis.right.widgets[column, row, page][[1]],
                        pos["row"], pos["column"])
        }
    }
    .MosaicEnv$panel.layout <- l
    container
}



