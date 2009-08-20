
## a basic demo of mouse event callbacks

library(qtutils)
library(qtbase)
library(qtpaint)
library(mosaiq)



tsplot <- function(y, x = NULL)
{
    if (missing(x))
        x <-
            if (is(x, "ts")) times(y)
            else seq_along(y)
    render_data<- function(item, painter, exposed) {
        qantialias(painter) <- TRUE
        mosaiq.points(x = x, y = y, type = "o", painter = painter)
    }
    handle_wheel <- function(event)
    {
        ## print(event$delta)
        lims <- qlimits(points)
        llist <- list(xlim = lims[, 1], ylim = lims[, 2])
        f <- -0.01 * event$delta / 120
        qlimits(event$item) <-
            qrect(extendrange(llist$xlim, f = f),
                  llist$ylim)
        qupdate(view)
        ## qupdate(points)
    }
    scene <- qgraphicsScene()
    root <- qlayer(scene)
    points <-
        qlayer(root,
               paintFun = render_data,
               wheelFun = handle_wheel)
    qlimits(points) <- qrect(range(x), range(y))
    view <- qplotView(scene = scene, opengl = FALSE)
    qsetContextMenuPolicy(view, "actions")
    qsetDeleteOnClose(view, TRUE)
    view
}

