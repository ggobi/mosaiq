
qxaxis <-
    function(xlim, tick.number = 5,
             at = pretty(xlim, tick.number),
             labels = as.character(at),
             font = qv.font(),
             side = c("bottom", "top"),
             ## y = switch(side, bottom = 1, top = 0),
             tck = 1,
             rot = 0,
             hadj = 0.75,
             vadj = 0.25, ##switch(side, bottom = 1, top = -0.5),
             minheight = 0, minwidth = 0)
{
    force(labels)
    side <- match.arg(side)
    id <- at >= xlim[1] & at <= xlim[2]
    at <- at[id]
    labels <- labels[id]
    scene <- qgraphicsScene()
    root <- qlayer(scene)
    paintFun <- function(item, painter, exposed)
    {
        qdrawText(painter, labels, at, 0.5,
                  halign = "center", valign = "center",
                  rot = rot)
        ## add segments
    }
    axis.layer <- qlayer(root, paintFun)
    qlimits(axis.layer) <- qrect(xlim, c(0, 1))
    view <- qplotView(scene = scene,
                      rescale = "geometry",
                      opengl = FALSE)
    view$horizontalScrollBarPolicy <- 1
    view$verticalScrollBarPolicy <- 1
    qsetExpanding(view, vertical = FALSE, horizontal = TRUE)
    view
}


qyaxis <-
    function(ylim, tick.number = 5,
             at = pretty(ylim, tick.number),
             labels = as.character(at),
             font = qv.font(),
             side = c("left", "right"),
             x = switch(side, left = 1, right = 0),
             tck = 1,
             rot = 0,
             hadj = switch(side, left = 1, right = 0),
             vadj = 0.25,
             minheight = 0, minwidth = 0)
{
    force(labels)
    side <- match.arg(side)
    labels <- switch(side, # FIXME: temporary hack
                     left = paste(labels, "-"),
                     right = paste("-", labels))
    id <- at >= ylim[1] & at <= ylim[2]
    at <- at[id]
    labels <- labels[id]
    scene <- qgraphicsScene()
    root <- qlayer(scene)
    paintFun <- function(item, painter, exposed)
    {
        ## FIXME: how to convey width?
        labsize <- lapply(qstrWidth(painter, labels), max)
        qdrawText(painter, labels, x, at,
                  halign = switch(side, left = "right", right = "left"),
                  rot = rot)
        ## add segments
    }
    axis.layer <- qlayer(root, paintFun)
    ## qlimits(axis.layer) <- qrect(c(0, labsize), ylim)
    qlimits(axis.layer) <- qrect(c(0, 1), ylim)
    view <- qplotView(scene = scene,
                      rescale = "geometry",
                      opengl = FALSE)
    view$horizontalScrollBarPolicy <- 1
    view$verticalScrollBarPolicy <- 1
    qsetExpanding(view, vertical = TRUE, horizontal = FALSE)
    ## qresize(view, w = 100, h = NULL)
    .MosaicEnv$axisview <- view
    view
}

