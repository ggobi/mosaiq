
## As a first step in resurrecting mosaiq, try out a basic layout
## example using the new smoke-based system

## QT example

library(qtbase)

qcircle <-  Qt$QPainterPath()
qcircle$addEllipse(-5, -5, 10, 10)


graphicsWidgetWithPoints <- function()
{
    w <- Qt$QGraphicsWidget()
    w$setPreferredSize(qsize(100, 100))
    w$setSizePolicy(Qt$QSizePolicy$Expanding,
                    Qt$QSizePolicy$Expanding)
    ## rectangle
    r <- Qt$QGraphicsRectItem()
    r$setParentItem(w)
    r$setRect(Qt$QRect(0, 0, 100, 100))
    ## points
    n <- 10L
    x <- 100 * runif(n)
    y <- 100 * runif(n)
    for (i in seq_len(n))
    {
        p <- Qt$QGraphicsPathItem()
        p$setParentItem(w)
        p$setPath(qcircle)
        ## if (cex != 1) p$scale(cex, cex)
        p$setFlag(Qt$QGraphicsItem$ItemIgnoresTransformations, TRUE)
        p$setPos(x[i], y[i])
    }
    w
}

plotwidget <- Qt$QGraphicsWidget()
plotwidget$setSizePolicy(Qt$QSizePolicy$Expanding,
                         Qt$QSizePolicy$Expanding)

plotlayout <- Qt$QGraphicsGridLayout()
plotlayout$setSpacing(0)
plotwidget$setLayout(plotlayout)

strips <-
    lapply(month.name[1:4],
           function(s) {
               foo <- Qt$QGraphicsProxyWidget()
               foo$setWidget(Qt$QLabel(s))
               foo
           })

plotlayout$addItem(strips[[1]], 0, 0)
plotlayout$addItem(strips[[2]], 2, 0)
plotlayout$addItem(strips[[3]], 0, 2)
plotlayout$addItem(strips[[4]], 2, 2)

plotlayout$addItem(graphicsWidgetWithPoints(), 1, 0)
plotlayout$addItem(graphicsWidgetWithPoints(), 1, 2)
plotlayout$addItem(graphicsWidgetWithPoints(), 3, 0)
plotlayout$addItem(graphicsWidgetWithPoints(), 3, 2)


## plotlayout$setRowStretchFactor(0, 0)
## plotlayout$setRowStretchFactor(1, 1)
## plotlayout$setRowStretchFactor(2, 0)
## plotlayout$setRowStretchFactor(3, 1)
## plotlayout$setColumnStretchFactor(0, 0)
## plotlayout$setColumnStretchFactor(1, 1)
## plotlayout$setColumnStretchFactor(2, 0)
## plotlayout$setColumnStretchFactor(3, 1)

figurescene <- Qt$QGraphicsScene()
figurescene$addItem(plotwidget)

figureview <- Qt$QGraphicsView()
figureview$resize(300, 300)
figureview$setScene(figurescene)
figureview

## figureview$scale(1/2, 1/2)

plotwidget$resize(400, 400)

## figureview$fitInView(figureview$sceneRect,
##                      Qt$Qt$IgnoreAspectRatio)

## ww <- graphicsWidgetWithPoints()
## plotlayout$addItem(ww, 1, 0)

## plotlayout$addItem(ww, 2, 3)



