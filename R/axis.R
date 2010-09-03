
labelLayer <- function(s, rot = 0,
                       col = "black", fill = "transparent")
{
    force(s)
    ## container layer
    toplayer <- qlayer(NULL)
    toplayer$minimumSize <- qsize(15, 15)
    ## background box (unclipped)
    box.layer <- qlayer(toplayer, paintFun = panel.box(col = col, fill = fill))
    box.layer$minimumSize <- qsize(15, 15)
    ## text label (clipped)
    paintFun <- function(item, painter, exposed)
    {
        qdrawText(painter, as.character(s), 0.5, 0.5,
                  halign = "center", valign = "center",
                  rot = rot)
    }
    label.layer <- qlayer(toplayer, paintFun, limits = qrect(c(0, 1), c(0, 1)),
                          clip = TRUE)
    label.layer$minimumSize <- qsize(15, 15)
    ## return container layer
    toplayer
}


labelWidget <-
    function(s, horizontal = TRUE, 
             col = "transparent", fill = "transparent")
{
    ans <- Qt$QLabel(s)
    ans$alignment <- 132
    ans
}



## labelWidget <-
##     function(s, horizontal = TRUE, 
##              col = "transparent", fill = "transparent")
## {
##     rot <- if (horizontal) 0 else 90
##     scene <- qgraphicsScene()
##     root <- qlayer(scene)
##     layer <- labelLayer(s, rot = rot,
##                         col = "green", fill = fill)
    
##     qaddItem(root, layer, 0, 0)
##     qrowStretch(layer) <- as.integer(!horizontal)
##     qcolStretch(layer) <- as.integer(horizontal)
##     view <- qplotView(scene = scene, opengl = FALSE)
##     view$focusPolicy <- 0
##     ## qsetExpanding(view, vertical = !horizontal, horizontal = horizontal)
##     qsetExpanding(view, vertical = FALSE, horizontal = FALSE)
##     view$size <- qpoint(20, 20)
##     view
## }




qxaxis <-
    function(xlim, tick.number = 5,
             at = NULL,
             labels = NULL,
             font = qfont(),
             side = c("bottom", "top"),
             ## y = switch(side, bottom = 1, top = 0),
             tck = 1,
             rot = 0,
             hadj = 0.75,
             vadj = 0.25, ##switch(side, bottom = 1, top = -0.5),
             minheight = 0, minwidth = 0,
             ...,
             item, painter, exposed)
{
    if (is.null(at)) at <- pretty(xlim, tick.number)
    if (is.null(labels)) labels <- as.character(at)
    side <- match.arg(side)
    id <- at >= xlim[1] & at <= xlim[2]
    at <- at[id]
    labels <- labels[id]
    switch(side,
           bottom = {
               mosaiq.segments(at, 1, at, 1.3,
                               col = "black", painter = painter)
               qdrawText(painter, labels, at, 0.5,
                         halign = "center", valign = "center",
                         rot = rot)
           },
           top = {
               mosaiq.segments(at, -0.3, at, 0,
                               col = "black", painter = painter)
               qdrawText(painter, labels, at, 0.5,
                         halign = "center", valign = "center",
                         rot = rot)
           })
    ## axis.layer <- qlayer(NULL, paintFun)
    ## qlimits(axis.layer) <-
    ##     qrect(xlim,
    ##           switch(side,
    ##                  bottom = c(0, 1.3),
    ##                  top = c(-0.3, 1)))
    ## qminimumSize(axis.layer) <- qsize(20, 20)
    ## qcacheMode(axis.layer) <- "none"
    ## qsetItemFlags(axis.layer, "clipsToShape", FALSE)
    ## axis.layer
}


qyaxis <-
    function(ylim, tick.number = 5,
             at = NULL,
             labels = NULL,
             font = qfont(),
             side = c("left", "right"),
             x = switch(side, left = 1, right = 0),
             tck = 1,
             rot = 0,
             hadj = switch(side, left = 1, right = 0),
             vadj = 0.25,
             minheight = 0, minwidth = 0,
             ...,
             item, painter, exposed)
{
    if (is.null(at)) at <- pretty(ylim, tick.number)
    if (is.null(labels)) labels <- as.character(at)
    side <- match.arg(side)
    labels <- switch(side, # FIXME: temporary hack
                     left = paste(labels, "-"),
                     right = paste("-", labels))
    id <- at >= ylim[1] & at <= ylim[2]
    at <- at[id]
    labels <- labels[id]
    ## FIXME: how to convey width?
    ## labsize <- lapply(qstrWidth(painter, labels), max)
    qdrawText(painter, labels, x, at,
              halign = switch(side, left = "right", right = "left"),
              rot = rot)
    ## add segments

    minwidth <- 10 * max(sapply(labels, nchar))
    ## axis.layer <- qlayer(NULL, paintFun)
    ## qlimits(axis.layer) <- qrect(c(0, 1), ylim)
    ## qcacheMode(axis.layer) <- "none"
    ## qsetItemFlags(axis.layer, "clipsToShape", FALSE)
    item$minimumSize <- qsize(minwidth, 20)
    ## axis.layer
}

